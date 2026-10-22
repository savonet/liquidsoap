# Language notes: FFmpeg network and device I/O

Advice for implementing [io.md](../io.md) in OCaml on the FFmpeg bindings.
Not normative.

## 1. Where the pieces live

Orientation only, for someone replacing the existing layer.

| Area                                               | Location                                                                   |
| -------------------------------------------------- | -------------------------------------------------------------------------- |
| `input.ffmpeg`, `input.http`                       | `src/core/optionals/ffmpeg/io/`                                            |
| `input.harbor.dynamic.regexp`                      | `src/core/optionals/ffmpeg/io/`, over the generic harbor input             |
| `input.v4l2`, `video.testsrc.ffmpeg`, `input.rtmp` | `src/libs/ffmpeg.liq`                                                      |
| `input.hls` replacement                            | `src/libs/hls.liq`                                                         |
| `input.harbor.dynamic`                             | `src/libs/http.liq`                                                        |
| `output.url`                                       | `src/core/outputs/`, with the URL target injected by the encoder layer     |
| Icecast/Harbor `%ffmpeg` special cases             | `src/core/outputs/` (the shared format-to-MIME/encoder step, icecast info) |

The I/O library is an optional dune library. An optional library with a
broken stanza disappears silently and the `%ifdef input.ffmpeg` guards in the
stdlib follow it: a build meant to contain it should fail when it does not
(for instance a test that requires `input.ffmpeg`).

## 2. Bindings used

| io.md step                   | Binding                                                                                                                                                                                                                          |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| input open (§2.2 step 3)     | `Av.open_input ~interrupt ?format ~opts ~configure_audio_stream …`; `opts` is consumed in place and what is left is the leftover set of step 4                                                                                   |
| content type, decoder record | the decoder layer's interfaces ([decoder.md §15](../decoder.md#15-interfaces-reused-elsewhere))                                                                                                                                  |
| ICY headers (§8)             | the `icy_metadata_headers` string AVOption, read through the input's option object, searching children                                                                                                                           |
| harbor open (§10.3)          | `Av.open_input_stream ?format read`: no interrupt; the read closure aborts itself                                                                                                                                                |
| abort from the read closure  | return the negative `AVERROR_EXIT` code; a negative result is passed to FFmpeg as the read's error code ([avformat.md §7.1.1](../../../../../modules/synced/ffmpeg/spec/avformat.md#711-custom-read--read-buffer-offset-length)) |
| `output.url` open (§11.2)    | `Av.open_output ~interrupt ?format ~interleaved ~opts url`, reached through the encoder factory                                                                                                                                  |
| close                        | `Av.close`; a closed container raises its closed error on any later use, which makes a late read after a stop harmless                                                                                                           |

Raising from the read closure gives FFmpeg `AVERROR_EXTERNAL`, which the
decoder treats as an error, not an end of input. Return codes instead of
raising, so that an abort, a timeout and a real end stay distinct.

FFmpeg polls the interrupt closure on whatever thread is inside the container
operation. Keep it to two atomic reads: the attempt's abort flag and the
shutdown flag.

## 3. State machines

- One `Atomic.t` per source, holding a polymorphic variant whose constructors
  carry their data, for instance
  ``[ `Stopped | `Starting of attempt | `Polling of attempt | `Connected of attempt * string * connection | `Stopping of attempt | `Failed of string * exn ]``.
- `attempt` can be the abort flag itself, a fresh `bool Atomic.t` per
  attempt. Transitions use `Atomic.compare_and_set` against the exact boxed
  value read, so physical equality on the attempt is the identity check.
- Make every transition a function that returns whether it won, and put the
  release of the connection in the caller that won. Writing the state
  unconditionally after a failed compare-and-set is the bug io.md §2.1 rules
  out.
- The connection record groups what is released together: container,
  decoder record, decoder buffer, and a `bool Atomic.t` saying whether it
  delivered data (for the next-attempt delay).
- `on_connect` / `on_disconnect` pairing: keep a per-connection flag set once
  the connect callbacks ran, and test it on every end path.

## 4. Tasks and threads

- The connect attempt and the `Failed` URL check are scheduler tasks with the
  threaded priority. Keep the task handle in the state, or in an `Atomic.t`
  beside it, and drop it on stop, so that the scheduler does not retain a
  stopped source.
- `debug=true` only raises the log level of backtraces. Nothing raises out
  of `generate_frame`; an exception inside a scheduler task reaches the
  scheduler's own error handling, not the clock's, so catch it in the task.
- `timeout`: record the start time of the blocking call in progress beside
  the abort flag; the interrupt closure compares it with a monotonic now.
  Clear it when the call returns, so an idle connection is not aborted.
- Option validation at evaluation needs the set of option names that FFmpeg
  input components define: `AVFormatContext`'s class, every demuxer's
  private class, every protocol's class. If the bindings do not enumerate
  them, that is a bindings addition.
- `restart_delay = null` on `output.url`: call the output's own stop from
  the error path; do not raise into the clock.
- HTTP 4xx answers reach the caller as the bindings' mapped FFmpeg error
  codes (`AVERROR_HTTP_NOT_FOUND`, `AVERROR_HTTP_4XX`, …); classify on those
  codes, not on message text.
- Read retry times from a monotonic time source, not `Unix.gettimeofday`.

## 5. Clock

- Declare one self-paced sync source per input instance at creation, and
  report it through the source's current-answer mechanism
  ([pacing.md §3](../../../../clock/spec/pacing.md#3-finding-the-sync-source)).
  Read the `self_sync` getter when the source prepares its streaming cycle.
- `output.url` keeps the generic sync source of outputs for `self_sync=true`.

## 6. Harbor client

- The feed task can be a loop of `decode` calls whose read closure blocks in
  `Unix.select` on the socket, in slices no longer than the abort check
  needs, retrying on the harbor's retry signal and on empty readiness, and
  returning the timeout code after `timeout` seconds without data. This holds
  one threaded worker per connected client for the client's lifetime. The
  alternative, waking on socket readability, must still decode until the
  container's own I/O buffer is empty, which the bindings do not expose.
- Write `dumpfile` and `logfile` from the read closure, the only place that
  sees the bytes.
- Remove the client's table entry in the single "end the client" function.
- Build the handler source value from the generic source methods plus the
  harbor methods, so that its type carries `id` and the rest.

## 7. Script-level notes

- `input.hls`: a binding such as `let replaces input.hls = input.ffmpeg`
  followed by a wrapper does not generalise: the value restriction leaves a
  weak variable, printed `'A`. Define the replacement directly as a function:
  `def replaces input.hls(~id=null, uri) = input.ffmpeg(id=id, uri) end`.
- `video.testsrc.ffmpeg`: give `duration` the number constraint, through
  whatever numeric operation or annotation infers it, and render the value
  as a decimal number without a trailing `.` before interpolating. If the
  language offers no way to constrain an optional argument to numbers, an
  `int` and a `float` form need another route; see io.md §1.2.
- `input.v4l2`: add `~self_sync=true` and pass it through.
- Icecast/Harbor mp3 overrides: copy the format's option table
  (`Hashtbl.copy`) before setting `id3v2_version` and `write_xing`.
- Callback descriptors carry no deprecated-argument form; a field for one
  that nothing reads should not exist.

## 8. Formats seen

- `rw_timeout`: int64 microseconds.
- `icy_metadata_headers`: CR/LF-separated `Name: value` lines.
- `Unrecognized options:` rendering: `name=value` joined by `,`.
- lavfi source string `pattern=size=WxH:rate=R[:duration=D]`.

## 9. Tests

Existing tests that pin behaviour, and what the normative spec changes:

- `ffmpeg-errors.liq` (missing file → `on_error` kind `ffmpeg`): unchanged.
- `ffmpeg-trim.liq` (URL whitespace trimmed): unchanged.
- `GH5389.liq` (input replaced inside a dynamic source is stopped, then
  collected): unchanged; relies on the task handle being dropped.
- `ffmpeg-copy-input-http.liq`, `LS268.liq`, `GH3276.liq`, `GH4840.liq`:
  `input.http` connection paths. A test that counts `on_error` at a clean end
  of input changes: none is expected.
- `GH2850.liq` (`output.url` `on_error`): unchanged for transient failures;
  a configuration error reports once.
- `harbor_dynamic_failed_connect.liq`: a failed open ends the client.
- `icecast_server_*.liq`: `icecast.server` over `input.harbor.dynamic` and
  `copy_encoder`.

Tests worth adding, from io.md's conformance items: a stop during every step
of an attempt (container count back to zero), a permanent failure that stays
failed, a finite lavfi input whose sample count matches per connection, a
dropped harbor source released after `timeout`, an mp3 format value
unchanged after `output.harbor`.
