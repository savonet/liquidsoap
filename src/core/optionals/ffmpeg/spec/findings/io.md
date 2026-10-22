# Findings: FFmpeg network and device I/O

## Defects

### Container leaked on post-open connect failures

`ffmpeg_io.ml:122-146`: after `Av.open_input` succeeds, the "Unrecognized
options" failure, the content-type mismatch failure, a `mk_decoder_record`
failure and an exception in a user `on_connect` all leave the container
open; it is released only by the GC finaliser. Combined with unbounded
retries (below), a misconfigured source opens a new connection every
`poll_delay` and leaks each one until collection. Read only.

### Configuration errors retried forever

`ffmpeg_io.ml:131-134,169-177`: "Unrecognized options" and content-type
mismatch are deterministic, yet go through the same `poll_delay` retry as a
network error. Checked: `int_args=[("bogus_opt",1)]` fails and calls
`on_error` once per second indefinitely.

### Disconnect racing a successful connect loses the connection

`ffmpeg_io.ml:198-203`: disconnect reads `` `Polling closed ``, then the task
CASes `Polling`→`Connected`; disconnect's CAS `Polling`→`Stopping` fails and
it sets `` `Stopped `` unconditionally. The connected decoder is never closed,
`on_disconnect` never runs, and the sync source set by `on_connect` stays.
Read only.

### `output.url` opens the URL on the streaming thread without interrupt

`pipe_output.ml:69-82` + `ffmpeg_encoder_common.ml:205`: the URL is opened
lazily inside `encode`, on the clock thread, via `Av.open_output` without
`~interrupt`, although the binding supports one. An unreachable RTMP server
blocks the clock for the protocol's own timeout; shutdown cannot abort it.
Read only.

### `output.url` metadata path bypasses restart handling

`pipe_output.ml:107-111`: `encode_metadata` may instantiate the encoder
(open the URL) outside `url_output#encode`'s `try`, so an open failure
triggered by metadata escapes `apply_on_error` (no restart delay, no
`on_error`). Read only.

### `%ffmpeg` mp3 format mutated by icecast/harbor outputs

`icecast_utils.ml:87-94`: `encoder_overrides` writes `id3v2_version=0` and
`write_xing=0` into the format value's shared `opts` table. The same format
used later for `output.file` gets them too. Checked: printing the format
after `output.harbor` shows `write_xing=0,id3v2_version=0`.

### Harbor dynamic read maps `Harbor.Retry` and errors to end of input

`ffmpeg_harbor_input.ml:185-200`: the container read returns 0 on
`Harbor.Retry` (the generic reader retries) and on any exception, without
disconnecting. 0 is end of input for libav, so a transient retry ends the
stream. Read only.

### Harbor dynamic sources leak when never woken

`ffmpeg_harbor_input.ml:444-446`: a source is removed from `sources` only on
sleep. When the open fails, or the user function does not attach the source
to anything that runs, it never wakes or sleeps: it stays in the queue (and
keeps matching `find` for that mount, shadowing later clients for ICY
metadata) and, in the second case, the client connection stays open
unread. Read only.

## Asymmetries

### Decoder flush never called on end of input

`ffmpeg_io.ml:254-262`: on `End_of_file` the input reconnects without calling
the decoder's `eof`; frames held by codecs with delay are lost at each end.
The harbor feed loop likewise closes without `eof`. File decoding (the
decoder agent's area) does flush. Read only.

**Verification** (verification pass): Worse: `End_of_file` lands in the generic `with exn` of `generate_frame`, so every clean end of input is also logged severe as `Feeding failed` and runs the user's `on_error` callbacks.

### Harbor dynamic skips the content-type check

`ffmpeg_harbor_input.ml:305-316` builds the decoder for `self#content_type`
without `Ffmpeg_decoder.get_type` / `Decoder.can_decode_type`, unlike
`input.ffmpeg` step 7. A mismatch shows up later as a decode failure. Read only.

### Track mark at position 0 for metadata

`ffmpeg_io.ml:251-253`: `new_track_on_metadata` puts the track mark at
position 0 of the frame, not at the metadata's position; metadata in the
middle of a frame starts the new track up to one frame early. Read only.

### Deduplication state survives reconnects

`ffmpeg_io.ml:583-593`: `last_meta` is per operator, so after a reconnect the
stream's first metadata, if equal to the last one before the drop, is
suppressed and no new track is marked for it. Read only.

### `self_sync` getter sampled only at connect

`ffmpeg_io.ml:85-87`: the getter is read once per connection; changes take
effect at the next reconnect. Read only.

### `input.v4l2` is not a sync source

A capture device has its own clock, yet `input.v4l2` keeps `self_sync=false`
while `input.rtmp` sets `true`. Read only.

### Analysis log names the regexp, not the client URI

`ffmpeg_harbor_input.ml:214` passes `mountpoint` (the regexp text) as URL.
Checked: log `Analyzing container for format: "ogg", uri: "^/live$"`.

## Gaps

### `input.hls` type has a non-generalised variable

Checked: `liquidsoap -h input.hls` prints `source('A)` (uppercase), unlike
every other input (`source('a)`). Origin: `let replaces input.hls =
input.ffmpeg` then a `def` calling it (`hls.liq:190-200`).

### `dumpfile` / `logfile` inert on `input.harbor.dynamic`

`ffmpeg_harbor_input.ml:185` reads the socket itself; the generic
`read_from` that writes the dump and log is never called. The files are
opened (and created empty). Read only.

### `video.testsrc.ffmpeg` `duration` typed `'a?`

Checked: any value is accepted and interpolated as text into the lavfi
string. Also its category is "Video processing", not "Input".

### Handler source lacks generic methods

Checked: the source passed to the `input.harbor.dynamic` handler has no `id`
(or other generic source method) in its type; using `s.id()` is a type error.

### Bytes in libav's I/O buffer not seen by the feed loop

`harbor_input.ml:266`: the feed loop decodes again only while the _socket_
has data; bytes already in libav's I/O buffer wait for the next socket event
(or `timeout`). Read only, possible added latency.

### `max_buffer` rarely effective

io.md §5: decoding stops at one frame, so the generator limit only bites when
a single `decode` produces more than `max_buffer`. The argument documented as
"maximum duration of buffered data" has little effect. Read only.

### `register_deprecated_argument` unused

`Lang_source` callback field `register_deprecated_argument = true` on all
`input.ffmpeg` callbacks is never read anywhere in `src/`. Read only.

## To verify

### `output.url` `on_start` override

`pipe_output.ml:197-198`: `method! on_start` replaces the generic one with a
list run on every (re)connect. Which callers use the OCaml `on_start` method
on outputs, and whether script `on_start` (via `register_on_start`) is meant
to fire on restarts, is unverified.

### `debug=true` in the connect task

`ffmpeg_io.ml:176`: re-raising from a `Scheduler.Async` task; what the
scheduler does with it (crash, log, task death) is unverified.

### Second wake-up of a harbor dynamic source

`ffmpeg_harbor_input.ml:141-143`: after sleep (which disconnects) a second
wake-up runs `start_feed` again on a closed container; `register_decoder`
builds a decoder on it. Unverified whether anything reads it.
