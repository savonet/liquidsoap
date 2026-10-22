# FFmpeg network and device I/O

This file owns the operators that move bytes between Liquidsoap and the
network or a device through libavformat: `input.ffmpeg`, `input.http`, the
script wrappers built on them (`input.v4l2`, `input.rtmp`,
`video.testsrc.ffmpeg`, `input.hls`), `input.harbor.dynamic` /
`input.harbor.dynamic.regexp`, `output.url`, and the places where
`output.icecast` / `output.harbor` special-case a `%ffmpeg` format. It
describes connection lifecycle, retries, interruption of blocking calls,
buffering, metadata, clock behaviour and failure handling. It relies on the
decoder contract in [decoder.md](decoder.md) (how a container becomes a
decoder, content-type negotiation, stream configuration), on the encoder in
[encoder.md](encoder.md) and its timestamp rules in
[timestamps.md](timestamps.md), on the clock model in
`src/core/clock/spec/`, and on the bindings' `Av` contract in
`src/modules/synced/ffmpeg/spec/avformat.md`.

## 1. Operators

| Operator                      | Defined as                                     | Category                  | Availability                         |
| ----------------------------- | ---------------------------------------------- | ------------------------- | ------------------------------------ |
| `input.ffmpeg`                | builtin                                        | Source / Input / Active   | ffmpeg optional built                |
| `input.http`                  | builtin, `input.ffmpeg` plus HTTP options      | Source / Input / Active   | ffmpeg optional built                |
| `input.v4l2`                  | script, over `input.ffmpeg`                    | Source / Input / Active   | `%ifdef input.ffmpeg`                |
| `input.rtmp`                  | script, over `input.ffmpeg`                    | Source / Input / Active   | `%ifdef input.ffmpeg`                |
| `video.testsrc.ffmpeg`        | script, over `input.ffmpeg`                    | Source / Video processing | `%ifdef input.ffmpeg`                |
| `input.hls`                   | script; `input.ffmpeg` replaces the native one | Source / Input / Active   | `%ifdef input.ffmpeg`                |
| `input.harbor.dynamic.regexp` | builtin                                        | Source / Input / Active   | ffmpeg optional built                |
| `input.harbor.dynamic`        | script, over the regexp form                   | Source / Input / Active   | `%ifdef input.harbor.dynamic.regexp` |
| `output.url`                  | builtin (core outputs)                         | Source / Output           | always; accepts only `%ffmpeg`       |

### 1.1 `input.ffmpeg` and `input.http`

Types (from the implementation, `liquidsoap -h`):

```
input.ffmpeg : (?id : string?, ?debug : bool, ?deduplicate_metadata : bool,
 ?float_args : [string * float], ?format : string?,
 ?int_args : [string * int], ?max_buffer : float,
 ?metadata_filter : (([string * string]) -> [string * string])?,
 ?new_track_on_metadata : bool, ?poll_delay : float, ?self_sync : {bool},
 ?start : bool, ?string_args : [string * string], ?trim_url : bool, {string}) ->
source('a)

input.http : (?id : string?, ?debug : bool, ?deduplicate_metadata : bool,
 ?float_args : [string * float], ?format : string?,
 ?int_args : [string * int], ?max_buffer : float,
 ?metadata_filter : (([string * string]) -> [string * string])?,
 ?new_track_on_metadata : bool, ?poll_delay : float, ?self_sync : {bool?},
 ?start : bool, ?string_args : [string * string], ?timeout : float,
 ?trim_url : bool, ?user_agent : string, {string}) -> source('a)
```

| Argument                | Type                | Default                      | Meaning                                                                                                                |
| ----------------------- | ------------------- | ---------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `start`                 | `bool`              | `true`                       | Transition to started when the source wakes up (§2.1).                                                                 |
| `int_args`              | `[string * int]`    | `[]`                         | FFmpeg options, each stored as an integer option.                                                                      |
| `float_args`            | `[string * float]`  | `[]`                         | FFmpeg options, stored as float.                                                                                       |
| `string_args`           | `[string * string]` | `[]`                         | FFmpeg options, stored as string.                                                                                      |
| `metadata_filter`       | nullable function   | `null`                       | Replaces the default filter (§6).                                                                                      |
| `deduplicate_metadata`  | `bool`              | `true`                       | Drop a metadata set equal to the previous kept one (§6).                                                               |
| `new_track_on_metadata` | `bool`              | `true`                       | A frame carrying kept metadata also gets a track mark (§6).                                                            |
| `max_buffer`            | `float` (seconds)   | `5.0`                        | Generator maximum length (§5).                                                                                         |
| `self_sync`             | `{bool}`            | `false`                      | `input.ffmpeg`: be a sync source while connected (§7).                                                                 |
| `self_sync`             | `{bool?}`           | `null`                       | `input.http`: `null` means "true when ICY headers were received" (§8).                                                 |
| `debug`                 | `bool`              | `false`                      | Re-raise connection exceptions instead of retrying (§9).                                                               |
| `poll_delay`            | `float` (seconds)   | `2.0`                        | Delay before the next connection attempt after a failed one (§2.3).                                                    |
| `format`                | `string?`           | `null`                       | Input format short name, looked up when the operator is created.                                                       |
| `trim_url`              | `bool`              | `true`                       | Strip leading/trailing whitespace from the URL on each read of it.                                                     |
| unlabeled               | `{string}`          | required                     | URL getter, read once per connection attempt.                                                                          |
| `timeout`               | `float` (seconds)   | `10.0`                       | `input.http` only: FFmpeg `rw_timeout`.                                                                                |
| `user_agent`            | `string`            | Liquidsoap's HTTP user agent | `input.http` only. Rendered default: `"Liquidsoap/2.5.0+git@7c66c387b (Unix; OCaml 5.5.0)"` (from the implementation). |

Option tables: the three `*_args` lists are folded, in the order int, float,
string, into one name→value table; a later entry with the same name replaces
an earlier one. `input.http` then forces three entries, replacing any user
value of the same name: `icy` = int `1`, `user_agent` = string, `rw_timeout`
= int64 `timeout × 1 000 000` (microseconds, truncated toward zero).

`format`: when not null, the name is resolved with the bindings' input-format
lookup at operator creation. An unknown name raises `Invalid_value` on the
`format` value with message `Could not find ffmpeg input format with that
name` (a script error at evaluation time).

There is no `fallible` argument: the source is always fallible.

Methods beyond the generic source ones:

| Method                        | Type                 | Behaviour                                                                                                                    |
| ----------------------------- | -------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `url`                         | `() -> string`       | Current URL getter value, trimmed when `trim_url`.                                                                           |
| `set_url`                     | `({string}) -> unit` | Replaces the URL getter. Takes effect at the next connection attempt.                                                        |
| `status`                      | `() -> string`       | One of `"stopped"`, `"starting"`, `"polling"`, `"connected <url>"`, `"stopping"` (§2.1).                                     |
| `start`, `stop`, `is_started` | generic start/stop   | `stop` on these sources never raises (they are fallible).                                                                    |
| `seek`                        | `(float) -> float`   | Delegates to the connected decoder's seek; `0` when not connected; `input.http` returns `-1` when ICY headers were received. |
| `remaining`                   | `() -> float`        | Connected decoder's remaining estimate; `-1` (unknown) otherwise.                                                            |
| `skip`                        | `() -> unit`         | Adds a track mark to the buffer.                                                                                             |

Example status string (from the implementation): `connected sine=frequency=440:duration=1`.

Callbacks (each `(synchronous : bool, fn) -> {release}`):

| Callback              | Argument                                                            | Fired                                                                                                                          |
| --------------------- | ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `on_connect`          | `input.ffmpeg`: none; `input.http`: `[string * string]` ICY headers | After the decoder is built, before the state becomes connected (§2.2 step 9).                                                  |
| `on_disconnect`       | none                                                                | When a connected source is disconnected, and when a connect is undone by a concurrent stop (§2.2 step 11).                     |
| `on_error`            | `error`                                                             | Each failed connection attempt (§2.3), each feeding failure (§4), each ICY header read failure (§8). Error kind is `"ffmpeg"`. |
| `on_start`, `on_stop` | none                                                                | Generic start/stop transitions.                                                                                                |

### 1.2 Script wrappers

| Wrapper                                                                               | Expands to                                                                                                    | Returned type                          |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `input.v4l2(~id=null, ~max_buffer=0.5, ~device="/dev/video0")`                        | `input.ffmpeg(id, format="v4l2", max_buffer, device)`                                                         | `source(video=yuv420p('a))`            |
| `input.rtmp(~id=null, ~max_buffer=5., ~listen=true, url)`                             | `input.ffmpeg(id, max_buffer, format="live_flv", self_sync=true, int_args=[("listen", listen ? 1 : 0)], url)` | `source('a)`                           |
| `video.testsrc.ffmpeg(~id=null, ~pattern="testsrc", ~max_buffer=0.5, ~duration=null)` | `input.ffmpeg(id, max_buffer, format="lavfi", src)`                                                           | `source(video=yuv420p('b))`            |
| `input.hls(~id=null, uri)`                                                            | `input.ffmpeg(id=id, uri)` with every other argument at its default                                           | `source('A)` (from the implementation) |

`video.testsrc.ffmpeg` builds `src` at call time as
`"<pattern>=size=<W>x<H>:rate=<R>"` followed by `":duration=<D>"` when
`duration` is not null, where W, H, R are the current values of
`settings.frame.video.width`, `.height`, `.framerate`. With defaults and
`duration=10`: `testsrc=size=1280x720:rate=25:duration=10` (from the
implementation). `duration` has type `'a?` and is interpolated as text.

`input.v4l2` and `video.testsrc.ffmpeg` annotate their result as
`source(video=yuv420p)`, which hides `input.ffmpeg`'s methods and callbacks.

`input.hls`: the stdlib first binds `input.hls` to `input.hls.native`; when
`input.ffmpeg` exists it replaces it with `input.ffmpeg`, then defines
`input.hls(~id, uri)` calling that binding.

### 1.3 `input.harbor.dynamic.regexp` / `input.harbor.dynamic`

`input.harbor.dynamic.regexp` takes the generic harbor input arguments
(`buffer` default `12.0` here, `max` `20.0`, `timeout` `30.0`, `user`
`"source"`, `password` `"hackme"`, `port` `8005`, `transport`, `icy` `false`,
`icy_metadata_charset`, `metadata_charset`, `replay_metadata` `false`, `auth`,
`dumpfile`, `logfile`, `debug` `false`), an unlabeled `regexp` mountpoint, and
a required `on_connect`. It returns `unit`. `buffer >= max` raises
`Invalid_value` on `max`: `Maximum buffering inferior to pre-buffered data`.

`input.harbor.dynamic(..., path)` is the same with `path : string`,
converted by `harbor.http.regexp_of_path`.

`on_connect` type (from the implementation):

```
({ copy_encoder : (?string?, ?options : [string * string]) -> format('a),
   format : string?, headers : [string * string], mime_type : string?,
   query : [string * string],
   streams : [{ channel_layout? : string, channels? : int, codec : string,
                field : string, frame_rate? : float?, height? : int,
                pixel_format? : string, samplerate? : int, type : string,
                width? : int }],
   uri : string })
-> (source('a).{ buffer_length : () -> float, connected_client : () -> string?,
                 on_connect : (synchronous : bool, (([string * string]) -> unit)) -> {release : () -> unit},
                 on_disconnect : (synchronous : bool, (() -> unit)) -> {release : () -> unit},
                 status : () -> string, stop : () -> unit })
-> unit
```

The source handed to the inner function carries only these methods and
callbacks; generic source methods such as `id` are not in its type.

### 1.4 `output.url`

```
output.url : (?id : string?, ?export_cover_metadata : bool, ?fallible : bool,
 ?register_telnet : bool, ?restart_delay : float?, ?self_sync : bool,
 ?start : bool, url : string, format('a), source('a)) -> unit
```

| Argument                                                        | Default                  | Meaning                                                              |
| --------------------------------------------------------------- | ------------------------ | -------------------------------------------------------------------- |
| `url`                                                           | required                 | Output URL handed to the muxer.                                      |
| `restart_delay`                                                 | `2.0`                    | Seconds before a restart after an error; `null` re-raises the error. |
| `self_sync`                                                     | `false`                  | Declares a static sync source (§11.4).                               |
| `fallible`, `start`, `register_telnet`, `export_cover_metadata` | generic output arguments |                                                                      |

The format must be `%ffmpeg`; any other raises `Invalid_value` on the format
value: `Encoding format does not support output url!`. The format's output
target is replaced by the URL (output mode `Url url` instead of `Stream`).
Callback `on_error : (error) -> unit`, error kind `"output"`, plus the generic
output callbacks (`on_start`, `on_stop`, `on_output`).

`output.youtube.live.rtmp` is `output.url(url="#{url}/#{key}", encoder, s)`
with `url` default `"rtmp://a.rtmp.youtube.com/live2"`.

## 2. `input.ffmpeg` connection lifecycle

### 2.1 States

The source holds one state. A **transition** `A` → `B` below sets `B` only
when the state is `A`, in one indivisible step, and reports whether it did:

| State                | `status` string   | Meaning                                                                                         |
| -------------------- | ----------------- | ----------------------------------------------------------------------------------------------- |
| `Stopped`            | `stopped`         | No connection, no attempt scheduled.                                                            |
| `Starting`           | `starting`        | A connection task exists and will run, or is waiting `poll_delay`.                              |
| `Polling closed`     | `polling`         | An attempt is running. `closed` is that attempt's abort flag.                                   |
| `Connected (url, c)` | `connected <url>` | `c` holds the decoder, its `remaining` function, the decoder buffer and the same `closed` flag. |
| `Stopping`           | `stopping`        | A stop arrived while an attempt was running; the attempt finalizes it.                          |

The source is ready (`can_generate_frame`) when it is started (generic
start/stop state is `Started`) and the state is `Connected`.

Start/stop: the generic start/stop layer queues transitions and applies them
before each streaming cycle. Waking up queues `Started` when `start=true`.
Going to sleep applies `Stopped` at once. `Started` calls _connect_;
`Stopped` and `Idle` call _disconnect_. The generic `reset` (on a started
source) is disconnect then connect.

_connect_: when the state is `Starting`, `Polling` or `Connected`, nothing.
Otherwise set `Starting`, create an asynchronous threaded task running
§2.2, remember it, and wake it up.

_disconnect_, by current state:

| State                 | Action                                                                                                                                                                                                  |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Stopping`, `Stopped` | nothing                                                                                                                                                                                                 |
| `Starting`            | set `Stopped`; stop and forget the task.                                                                                                                                                                |
| `Polling closed`      | set `closed`; transition `Polling`→`Stopping`, or if that fails set `Stopped`; stop and forget the task.                                                                                                |
| `Connected`           | set `closed`; close the decoder (an exception is logged `Error while disconnecting: <exn>` at severe level with backtrace and swallowed); run `on_disconnect`; set `Stopped`; stop and forget the task. |

Stopping a task removes it from the scheduler; it does not interrupt a run
in progress. The `closed` flag does (§3).

_reconnect_: only from `Connected`: disconnect, then connect. In other
states, nothing.

### 2.2 One connection attempt

The task body, run on a threaded scheduler worker:

1. Set the generator maximum length to `max_buffer` (§5).
2. Create a fresh `closed = false` flag for this attempt.
3. Transition `Starting` → `Polling closed`. On failure the attempt ends as
   "stopped" (go to the end of step 11's stop handling).
4. Copy the option table; read the URL getter (trimmed when `trim_url`).
5. Open the input with the bindings' `open_input`, passing: the interrupt
   function `shutdown || closed` (§3), `format` when given, the option copy,
   and the decoder's audio, video and subtitle stream configuration
   callbacks ([decoder.md](decoder.md)).
6. If the option copy is not empty, fail with
   `Unrecognized options: <name>=<value>,...`. Example (from the
   implementation): `Unrecognized options: bogus_opt=1`.
7. Compute the container's content type against the source's content type
   ([decoder.md §5.2](decoder.md#52-container-description-and-content-type)). If the decoder
   registry reports it cannot produce the source's type, fail with
   `url "<url>" cannot produce content of type <type>`.
8. Build a decoder record from the container with "decode first metadata"
   enabled, and a decoder buffer writing into the source's generator
   ([decoder.md](decoder.md)).
9. Compute the connect metadata (empty for `input.ffmpeg`, ICY headers for
   `input.http`, §8) and run every `on_connect` callback with it. The state is
   still `Polling` here: `status()` inside `on_connect` returns `"polling"`
   (from the implementation).
10. Add a track mark to the generator.
11. Transition `Polling closed` → `Connected (url, c)`. If it fails (a stop came in),
    close the decoder, run `on_disconnect`, and end as stopped.

Return value: `-1` (task sleeps until woken) on success and on every
"stopped" end; `poll_delay` after a failure (the task runs again after that
many seconds).

Stopped end: transition `Stopping` → `Stopped` (ignored when the state is
something else).

### 2.3 Failure of an attempt

Any exception from steps 4–11 is handled in this order:

1. If the state is no longer `Polling closed` (transition `Polling`→`Starting`
   fails), a disconnect interrupted the attempt: transition `Stopping`→`Stopped`,
   return `-1`. Nothing is logged, no callback runs.
2. Otherwise the state is now `Starting`. Log at severe level
   `Decoding failed: <exn>` with the backtrace at debug level, build a runtime
   error of kind `"ffmpeg"` from the exception, run every `on_error`
   callback (inside them `status()` is `"starting"`, from the
   implementation), re-raise if `debug=true`, else return `poll_delay`.

Retries are unbounded: every failure, including a configuration error such as
unrecognised options, is retried every `poll_delay` seconds until the source
stops. The URL getter is re-read on each attempt.

The container opened in step 5 is not closed on failures of steps 6–9.

### 2.4 Shutdown

A process-wide `shutdown` flag is raised by the "before core shutdown"
lifecycle hook (named `input.ffmpeg shutdown`). It makes every interrupt
function return true and makes the feeding loop raise "not connected".

## 3. Interrupting blocking libav calls

The interrupt function installed on the container in step 5 returns
`shutdown || closed`. It covers the open and every later read of that
container ([avformat.md §7.1.4](../../../../modules/synced/ffmpeg/spec/avformat.md#714-interrupt--interrupt-)).
When it returns true, the blocked libav call returns with the `Exit` error,
which the decoder reports as end of file ([decoder.md](decoder.md)).

`closed` is per attempt: disconnect sets it, so a pending open of attempt N
aborts while a later attempt N+1 gets a new, false flag. Once connected, the
same flag keeps aborting reads of that container after disconnect.

`input.http` additionally relies on `rw_timeout` (§1.1) to bound each
network read.

## 4. Frame generation

Each streaming cycle on a ready source:

1. Take the connected container record; if the state is not `Connected`,
   raise "not connected".
2. While the generator holds less than one frame (`Frame.size` main ticks):
   if `shutdown` or `closed`, raise "not connected"; else call the decoder's
   `decode` once with the decoder buffer.
3. Slice one frame from the generator.
4. Apply the frame-level metadata processing of §6.

Failure of any step (decoder end of file, decoding error, not connected):

1. Log at severe level `Feeding failed: <exn>` with the backtrace.
2. Run `on_error` with a kind `"ffmpeg"` error.
3. Reconnect (§2.1): disconnect, then connect.
4. Return what the generator still holds (sliced up to one frame) followed by
   an end of track.

A finite input therefore loops: end of file ends the track, reconnects, and
plays again from the start. Example (from the implementation): `input.ffmpeg`
on `lavfi` `sine=frequency=440:duration=1` with `poll_delay=1.` logs
`Feeding failed: Ffmpeg_decoder.End_of_file`, calls `on_error` with
`kind=ffmpeg`, then `on_disconnect`, then `on_connect` again, once per second.

The decoder's end-of-stream flush is not called on these paths.

Decoding runs on the clock's streaming thread and blocks it while the network
read blocks, up to the interrupt or `rw_timeout`.

## 5. Buffering

The generator's maximum length is `max_buffer` seconds converted to main
ticks, set at the start of each attempt. When content added to the generator
makes its buffered length exceed the maximum, the generator logs at severe
level `Generator max buffered length exceeded (<max> < <len>)! Dropping
content..` and drops the oldest excess; when no synchronised content can be
dropped it logs `Generator does not have synchronized content to drop.
Content may get out of sync!` and drops unsynchronised content.

Because §4 decodes only until one frame is buffered, the generator holds at
most one frame plus what one `decode` call produced; the limit is reached
only when one decode call yields more than `max_buffer`.

`buffered`, `remaining` and `seek` read the connected state; see §1.1.

## 6. Metadata

Metadata enters the generator from the decoder: container metadata on change
and stream metadata, the first set included ([decoder.md](decoder.md)).
`input.ffmpeg` filters it at frame level, after slicing:

1. For each `(position, metadata)` of the frame, apply the effective filter.
   Keep the pair only when the result is non-empty.
2. Replace the frame's metadata by the kept pairs.
3. If at least one pair was kept and `new_track_on_metadata`, add a track
   mark at position 0 of the frame.

Effective filter, composed once per operator:

1. Base: the user `metadata_filter` when given; else drop every key matching
   the regular expression `^id3v2_priv`.
2. When `deduplicate_metadata`: apply the base; if the result is empty, or its
   sorted list equals the last kept sorted list, return empty; else remember
   the sorted list and return the result.

The deduplication memory lives as long as the operator, across reconnects.

## 7. Clock and `self_sync`

`input.ffmpeg` and `input.http` are active sources. Their sync source is
dynamic: on each successful connect (an `on_connect` callback registered
before any user callback), the source sets its sync source to its own generic
sync source, named `source(id=<id>)`, when the `self_sync` getter (read at
that moment) is true, and to none otherwise. On disconnect it sets none.
`self_sync()` reports the current value. Example (from the implementation):
`input.ffmpeg(self_sync=true, format="lavfi", "sine")` makes its clock log
`Now following sync source source(id=input.ffmpeg.1) (latency: 0.10s, maximum
latency: 60.00s)`.

The source does not pace itself; with `self_sync` true the clock stops
sleeping and the blocking reads of §4 set the pace. See the clock spec for
sync sources and the single-sync rule.

`input.rtmp` passes `self_sync=true`. `input.v4l2`, `video.testsrc.ffmpeg`
and `input.hls` keep `false`.

## 8. `input.http` specifics

- Name `input.http`; options of §1.1 forced.
- Connect metadata: read the string AVOption `icy_metadata_headers` from the
  input, searching children. Split on `\r?\n`; for each non-empty line
  matching `([^:]*):\s*(.*)` add `(name, value)`; lines not matching are
  skipped. The list is built by prepending, so it is in reverse line order.
  Failure to read the option logs `Error while fetching icy headers: <exn>`
  (severe, with backtrace), runs `on_error` (kind `"ffmpeg"`), and yields
  `[]`.
- `is_icy` is set to "the list is non-empty" on every connect.
- `self_sync`: the user value when not null; else `is_icy`.
- `seek` returns `-1` when `is_icy`, else delegates.
- `on_connect` callbacks receive the header list.

## 9. Failure summary (`input.ffmpeg`, `input.http`)

| Failure                                    | Logged                                                       | Callback                                                  | Next                                 |
| ------------------------------------------ | ------------------------------------------------------------ | --------------------------------------------------------- | ------------------------------------ |
| Unknown `format` name                      | —                                                            | —                                                         | `Invalid_value` at script evaluation |
| Open fails (unreachable, not found, …)     | severe `Decoding failed: <exn>`                              | `on_error`                                                | retry after `poll_delay`             |
| Unrecognised options                       | severe `Decoding failed: Failure("Unrecognized options: …")` | `on_error`                                                | retry after `poll_delay`             |
| Content type mismatch                      | severe `Decoding failed: …cannot produce content of type…`   | `on_error`                                                | retry after `poll_delay`             |
| Exception in a user `on_connect`           | severe `Decoding failed: <exn>`                              | `on_error`                                                | retry after `poll_delay`             |
| Any of the above with `debug=true`         | as above                                                     | `on_error`                                                | exception re-raised in the task      |
| Stop during an attempt                     | —                                                            | `on_disconnect` only if step 11 lost the race             | `Stopped`                            |
| Decode error / end of file while connected | severe `Feeding failed: <exn>`                               | `on_error`, `on_disconnect`, then `on_connect` on success | end of track, reconnect at once      |
| Decoder close fails on disconnect          | severe `Error while disconnecting: <exn>`                    | `on_disconnect` still runs                                | `Stopped`                            |

Example error message (from the implementation), missing file:
`Avutil.Error(No such file or directory)`.

## 10. `input.harbor.dynamic`

### 10.1 Registration

Calling the operator forces `settings.init.force_start` to true and registers
one harbor handler on `port` for the mountpoint regexp, with the given
transport and `icy`. The handler keeps a queue of `(mount, source)` pairs,
`mount` being the request URI without one leading `/` (when the URI is longer
than one character).

- ICY metadata updates and MIME queries for a mount are routed to the first
  queued source whose stored mount equals the trimmed request mount; with
  none, they are ignored / answer none.
- Login is the generic harbor login built from `user`, `password`, `auth`.

### 10.2 Per client

On each accepted source client (harbor thread):

1. Create a new source. Push it in the queue; on its sleep, remove it.
2. Id: the URI without its leading `/`; `input.harbor.dynamic` when the URI
   is empty or `/`.
3. Relay the client into it (generic harbor input): claim the connection,
   record the MIME type (the `Content-Type` up to the first `;`), the request
   headers, the URI and regexp groups, then run step 4. The harbor writes its
   reply and releases its use of the connection after this returns.
4. Acquire a use of the connection and schedule a threaded task (delay 0)
   running §10.3. When the connection is already aborted, nothing.

### 10.3 Opening the container

1. Map the lower-cased MIME type to an input format:

   | MIME                                   | Format     |
   | -------------------------------------- | ---------- |
   | `audio/mpeg`, `audio/mp3`              | `mp3`      |
   | `audio/ogg`, `application/ogg`         | `ogg`      |
   | `audio/flac`                           | `flac`     |
   | `audio/aac`, `audio/aacp`              | `aac`      |
   | `video/x-matroska`, `audio/x-matroska` | `matroska` |
   | `video/webm`, `audio/webm`             | `webm`     |
   | `video/mp4`, `audio/mp4`               | `mp4`      |
   | `video/x-flv`                          | `flv`      |
   | `video/mpeg`                           | `mpegts`   |
   | anything else                          | probe      |

2. Open with the bindings' `open_input_stream` (no interrupt function, empty
   options) over a read function: when `shutdown` (the
   `input.harbor.dynamic shutdown` lifecycle flag) return 0; else wait up to
   `timeout` seconds for the socket to be readable and read. A harbor
   "retry" or thread-exit exception returns 0; any other exception is logged
   `Connection failed: <exn>` (severe, backtrace) and returns 0. Returning 0
   is end of input for libav.
   Open failure: log info `Failed to open FFmpeg container: <exn>`, fail
   with "unknown codec".
3. Leftover options: log info `Unrecognized FFmpeg options: …`.
4. Describe the container ([stream-description.md §2](stream-description.md#2-container-format-url-input),
   [§5](stream-description.md#5-describe)), with the operator's mountpoint regexp text as its URL. On
   failure: close the container, log info `Failed to analyze container:
<exn>`, fail with "unknown codec". Log info `Container: <description>`.
5. Store the container in the source.
6. Build the connection record (§10.4) and the source value with its methods
   and callbacks.
7. Release the use taken in §10.2 step 4.
8. Apply `on_connect` to the record, then the returned function to the
   source.

A failure in steps 1–6 or in step 8 logs at severe level `Error while opening
container: <exn>!` and disconnects the connection (closes the socket, runs
the source's `on_disconnect` callbacks).

### 10.4 Connection record

| Field          | Value                                                                                   |
| -------------- | --------------------------------------------------------------------------------------- |
| `uri`          | Request URI.                                                                            |
| `query`        | Regexp groups of the mount match, as `(label, value)` pairs.                            |
| `format`       | Container format name from the description, or null.                                    |
| `mime_type`    | The client MIME when it contains `/`; else the MIME of the described format; else null. |
| `streams`      | One record per described stream, in description order (below).                          |
| `headers`      | Request headers as received.                                                            |
| `copy_encoder` | Function returning a `%ffmpeg` copy format for this container (§10.5).                  |

Stream records are those of [stream-description.md §7](stream-description.md#7-script-visible-records).

Example (from the implementation), an `output.icecast` of
`%ffmpeg(format="ogg", %audio(codec="libvorbis"))` on mount `live`:
`uri=/live`, `query=[]`, `format=ogg`, `mime=application/ogg`, one stream
`field=audio type=audio codec=vorbis samplerate=44100 channels=2
layout=stereo`; log line `Container: audio: {codec: vorbis, 44100Hz, 2
channel(s)}`.

### 10.5 `copy_encoder`

`copy_encoder(?format, ?options)` returns a `%ffmpeg` format with:

- `format`: the argument when not null, else the described container format;
- `mime_type`: as `mime_type` above;
- output mode `Stream`, interleaving `default`, no metadata;
- one stream per described audio, video and subtitle stream, keyed by its
  field, in copy mode waiting for a keyframe; data streams are omitted;
- `options` as string options.

Rendered (from the implementation): `copy_encoder(null, options=[])` →
`%ffmpeg(format="ogg",interleaved="default",%audio.copy(wait_for_keyframe))`;
`copy_encoder("matroska", options=[("live","1")])` →
`%ffmpeg(format="matroska",interleaved="default",%audio.copy(wait_for_keyframe),live="1")`.

The stdlib's `icecast.server` (extra library) builds outputs with
`copy_encoder(format, options=…)` and the setting
`settings.icecast.server.default_muxer_options`, default
`[("matroska", [("dash","1"),("live","1")]), ("webm", [("dash","1"),("live","1")])]`.

### 10.6 Feeding

When the source wakes up (the user function attached it to something that
runs):

1. Build a decoder record on the stored container for the source's content
   type, "decode first metadata" enabled, and a decoder buffer on its
   generator ([decoder.md](decoder.md)). With no stored container, fail with
   "unknown codec". No content-type compatibility check runs beforehand.
2. Add pending ICY metadata, run the source's `on_connect` callbacks with the
   request headers, open `dumpfile` / `logfile` when set.
3. If a use of the connection can be acquired, run the generic harbor feed
   loop: a threaded task woken on socket readability or `timeout`; on wake,
   decode once, then again while the socket has pending data; a wake with no
   readability raises a timeout. Any exception adds a track mark, logs at
   severe level `Feeding stopped: <exn>`, disconnects the connection, closes
   the decoder (which closes the container) and releases the use.

The generator's maximum length is `max` seconds, set on wake up; the source
is ready once `buffer` seconds are buffered (generic harbor input). Going to
sleep disconnects. The source's `stop` method disconnects the client.

The container reads through the read function of §10.3, not through the
generic harbor reader: `dumpfile` and `logfile` receive no data.

Interruption: no interrupt function is installed. A blocked read returns when
the socket becomes readable, when the `timeout` wait fails, or when the
connection is aborted (socket shut down), each of which ends in 0 bytes, i.e.
end of input.

## 11. `output.url`

### 11.1 Encoder slot

The output holds an encoder slot: none, pending, or created. _create_ sets
pending. Reading the encoder in the pending state instantiates it: the
`%ffmpeg` encoder factory with the output id and the current metadata (empty
when none), which opens the URL with the bindings' `open_output`, without an
interrupt function ([encoder.md](encoder.md)). _close_ calls the encoder's
`stop` (flush and close; exceptions swallowed) and sets none.

### 11.2 Start, encode, stop

- Start: _connect_. When the slot is none and `now >= restart_time`
  (initially 0): set pending and run the output's own start hooks; an
  exception here goes to §11.3.
- Encode a frame: if the slot is none and `now >= restart_time`, connect
  then encode; if none and the restart time is not reached, drop the frame;
  otherwise encode (a frame with position 0 is skipped). The encoded bytes are
  discarded: the muxer writes to the URL itself.
- Metadata: passed to the encoder's metadata insertion, instantiating it if
  pending, outside the error handling of §11.3.
- Stop: close.
- Reset: nothing.

### 11.3 Errors

Any exception while encoding (including the URL open, which happens on the
first encode after a connect):

1. Close the encoder, ignoring errors.
2. Log at severe level `Error while connecting: <exn>` with backtrace.
3. Run `on_error` with a runtime error of kind `"output"`.
4. With `restart_delay = null`, re-raise. Otherwise set
   `restart_time = now + restart_delay`, log at important level
   `Will try again in <d with 2 decimals> seconds.`; frames are dropped until
   then.

Restarts are unbounded. Each restart builds a new encoder: muxer, streams and
timestamp state start fresh ([timestamps.md](timestamps.md)).

### 11.4 Clock

`self_sync=true` makes the output a static sync source (its generic sync
source); `false` declares none. Pacing then comes from the muxer's blocking
writes.

## 12. Other outputs special-casing `%ffmpeg`

`output.icecast` and `output.harbor` derive their stream MIME type and
encoder through one shared step:

- MIME of a `%ffmpeg` format: its `mime_type` when set, else the MIME of its
  container format name in Liquidsoap's container table (e.g. `adts` →
  `audio/aac`, `ac3` → `audio/x-ac3`; this table differs from
  [encoder-format.md §12](encoder-format.md#12-file-extension-and-mime-type)), else none (then the user must pass `format=`, or
  `Invalid_value` `No format (mime) found, please specify one.` is raised).
- When the `%ffmpeg` format name is `mp3`, the option `id3v2_version` is set
  to int `0` and `write_xing` to int `0` unless already present. The options
  table of the format value is modified in place: printing the format after
  creating the output shows them. Example (from the implementation):
  before `%ffmpeg(format="mp3",interleaved="default",%audio(codec="libmp3lame",samplerate=44100,channels=2))`,
  after `output.harbor` `…,channels=2),write_xing=0,id3v2_version=0)`.

`output.icecast` stream info for a `%ffmpeg` format: bitrate is the encoder's
reported bitrate divided by 1000 (when known); samplerate and channels come
from the `audio` field when it is an encoded stream; quality is none.

## 13. Timestamps across reconnects

- `input.ffmpeg`, `input.http`: each connection builds a new decoder record,
  which allocates a new stream index for its copy/raw content; a reconnect is
  therefore a new stream for downstream encoders. The connect adds a track
  mark before the first data and a feeding failure ends the track. The
  encoder's handling of a new stream index and of a new track is in
  [timestamps.md](timestamps.md).
- `input.harbor.dynamic`: one source per client; a reconnecting client is a
  new source with a new decoder record.
- `output.url`: each restart is a new encoder (§11.3).

No offset or timestamp state is kept by the I/O layer itself.
