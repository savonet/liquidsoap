# FFmpeg network and device I/O

This file owns the operators that move bytes between Liquidsoap and the
network or a device through libavformat: `input.ffmpeg`, `input.http`, the
script wrappers built on them (`input.v4l2`, `input.rtmp`,
`video.testsrc.ffmpeg`, `input.hls`), `input.harbor.dynamic` /
`input.harbor.dynamic.regexp`, `output.url`, and the places where
`output.icecast` / `output.harbor` special-case a `%ffmpeg` format. It owns
their connection lifecycles (states, transitions, races, ownership of what a
connection holds), failure classes and retries, interruption of blocking
calls, buffering, metadata and clock behaviour. It relies on the decoder
contract of [decoder.md](decoder.md) (how a container becomes a decoder,
content-type negotiation, draining at end of input), on the encoder of
[encoder.md](encoder.md), on [draining.md](draining.md) for what ending a
stage means, on source identity in [content.md §5](content.md#5-stream_idx-and-time_base),
on the clock spec (`src/core/clock/spec/`, chiefly
[pacing.md](../../../clock/spec/pacing.md) and
[clock.md](../../../clock/spec/clock.md)), and on the bindings' `Av` contract
([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md)).

Three principles hold for every operator of this file:

1. **Every connection has one owner at a time.** A connection's resources
   (container, decoder, encoder, client socket, scheduled task) are owned by
   exactly one party, and ownership moves only with a state transition. The
   party that moves the state out of a connected state releases what the
   connection holds, on every path.
2. **A clean end of input is not an error.** It drains, ends the track, and
   reports no error. A would-block or retry signal from a transport is not an
   end of input.
3. **No blocking call escapes a stop.** A connection is never opened on the
   streaming thread. Every blocking call on a connection is aborted when the
   connection is stopped or the application shuts down, and is bounded by a
   named timeout.
4. **Faults follow [faults.md](faults.md).** Every fault this file detects is
   placed in one of the classes of [faults.md §2](faults.md#2-classes):
   configuration errors fail the script before it streams; a lost or failed
   connection is a source fault ([faults.md §3](faults.md#3-source-faults-reach-the-script))
   or an output fault; nothing raises out of a source while it streams. Every
   source of this file is fallible: while it cannot produce data it is not
   ready, so the script's `fallback` or `switch` decides what plays. No
   source of this file invents content.

## 1. Operators

| Operator                      | Defined as                                     | Category                | Availability                         |
| ----------------------------- | ---------------------------------------------- | ----------------------- | ------------------------------------ |
| `input.ffmpeg`                | builtin                                        | Source / Input / Active | FFmpeg support built                 |
| `input.http`                  | builtin, `input.ffmpeg` plus HTTP options      | Source / Input / Active | FFmpeg support built                 |
| `input.v4l2`                  | script, over `input.ffmpeg`                    | Source / Input / Active | `%ifdef input.ffmpeg`                |
| `input.rtmp`                  | script, over `input.ffmpeg`                    | Source / Input / Active | `%ifdef input.ffmpeg`                |
| `video.testsrc.ffmpeg`        | script, over `input.ffmpeg`                    | Source / Input / Active | `%ifdef input.ffmpeg`                |
| `input.hls`                   | script; `input.ffmpeg` replaces the native one | Source / Input / Active | `%ifdef input.ffmpeg`                |
| `input.harbor.dynamic.regexp` | builtin                                        | Source / Input / Active | FFmpeg support built                 |
| `input.harbor.dynamic`        | script, over the regexp form                   | Source / Input / Active | `%ifdef input.harbor.dynamic.regexp` |
| `output.url`                  | builtin (core outputs)                         | Source / Output         | always; accepts only `%ffmpeg`       |

### 1.1 `input.ffmpeg` and `input.http`

Types, as `liquidsoap -h` prints them (example):

```
input.ffmpeg : (?id : string?, ?debug : bool, ?deduplicate_metadata : bool,
 ?float_args : [string * float], ?format : string?,
 ?int_args : [string * int], ?max_buffer : float,
 ?metadata_filter : (([string * string]) -> [string * string])?,
 ?new_track_on_metadata : bool, ?poll_delay : float, ?self_sync : {bool},
 ?start : bool, ?string_args : [string * string], ?timeout : float,
 ?trim_url : bool, {string}) -> source('a)

input.http : (?id : string?, ?debug : bool, ?deduplicate_metadata : bool,
 ?float_args : [string * float], ?format : string?,
 ?int_args : [string * int], ?max_buffer : float,
 ?metadata_filter : (([string * string]) -> [string * string])?,
 ?new_track_on_metadata : bool, ?poll_delay : float, ?self_sync : {bool?},
 ?start : bool, ?string_args : [string * string], ?timeout : float,
 ?trim_url : bool, ?user_agent : string, {string}) -> source('a)
```

| Argument                | Type                | Default                      | Meaning                                                                                                             |
| ----------------------- | ------------------- | ---------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| `start`                 | `bool`              | `true`                       | Start connecting when the source wakes up (§2.1).                                                                   |
| `int_args`              | `[string * int]`    | `[]`                         | FFmpeg options, each stored as an integer option.                                                                   |
| `float_args`            | `[string * float]`  | `[]`                         | FFmpeg options, stored as float.                                                                                    |
| `string_args`           | `[string * string]` | `[]`                         | FFmpeg options, stored as string.                                                                                   |
| `metadata_filter`       | nullable function   | `null`                       | Replaces the default filter (§6).                                                                                   |
| `deduplicate_metadata`  | `bool`              | `true`                       | Drop a metadata set equal to the previous kept one of the same connection (§6).                                     |
| `new_track_on_metadata` | `bool`              | `true`                       | Kept metadata starts a new track at its position (§6).                                                              |
| `max_buffer`            | `float` (seconds)   | `5.0`                        | Bound on decoded data held by the source (§5).                                                                      |
| `self_sync`             | `{bool}`            | `false`                      | `input.ffmpeg`: pace the clock while connected; for live URLs only (§7).                                            |
| `self_sync`             | `{bool?}`           | `null`                       | `input.http`: `null` means "true when ICY headers were received" (§8).                                              |
| `debug`                 | `bool`              | `false`                      | Log every fault with its backtrace at error level (§2.3).                                                           |
| `poll_delay`            | `float` (seconds)   | `2.0`                        | Delay before a retry after a transient failure; first back-off delay; interval of the URL check when failed (§2.3). |
| `format`                | `string?`           | `null`                       | Input format short name, looked up when the operator is created.                                                    |
| `trim_url`              | `bool`              | `true`                       | Strip leading/trailing whitespace from the URL on each read of it.                                                  |
| unlabeled               | `{string}`          | required                     | URL getter, read once per connection attempt and by the URL check of §2.3.                                          |
| `timeout`               | `float` (seconds)   | `10.0`                       | Longest a single blocking open or read may last (§3); `input.http` also passes it as `rw_timeout`.                  |
| `user_agent`            | `string`            | Liquidsoap's HTTP user agent | `input.http` only.                                                                                                  |

Option tables: the three `*_args` lists are folded, in the order int, float,
string, into one name→value table; a later entry with the same name replaces
an earlier one. `input.http` then forces three entries, replacing any user
value of the same name: `icy` = int `1`, `user_agent` = string, `rw_timeout`
= int64 `timeout × 1 000 000` (microseconds, truncated toward zero).

`format`: when not null, the name is resolved with the bindings' input-format
lookup at operator creation. An unknown name is a script error at evaluation
time, raised on the `format` value (example message:
`Could not find ffmpeg input format with that name`).

There is no `fallible` argument: the source is always fallible.

Methods beyond the generic source ones:

| Method                        | Type                 | Behaviour                                                                                                                                                |
| ----------------------------- | -------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `url`                         | `() -> string`       | Current URL getter value, trimmed when `trim_url`.                                                                                                       |
| `set_url`                     | `({string}) -> unit` | Replaces the URL getter. Takes effect at the next attempt; a source in `Failed` leaves it at once (§2.1).                                                |
| `status`                      | `() -> string`       | One of `"stopped"`, `"starting"`, `"polling"`, `"connected <url>"`, `"stopping"`, `"failed"` (§2.1).                                                     |
| `start`, `stop`, `is_started` | generic start/stop   | `stop` never raises.                                                                                                                                     |
| `seek`                        | `(float) -> float`   | Seeks the connected decoder ([decoder.md §12](decoder.md#12-seeking)); `0` when not connected; `input.http` returns `-1` when ICY headers were received. |
| `remaining`                   | `() -> float`        | Connected decoder's remaining estimate; `-1` (unknown) otherwise.                                                                                        |
| `skip`                        | `() -> unit`         | Ends the current track at the current position.                                                                                                          |

Example status string: `connected sine=frequency=440:duration=1`.

A seek is a source switch downstream: the decoder discards what it holds
([draining.md §5](draining.md#5-discarded-on-purpose)) and continues under a
new source id ([content.md §5](content.md#5-stream_idx-and-time_base)).

Callbacks (each `(synchronous : bool, fn) -> {release}`):

| Callback              | Argument                                                            | Fired                                                                                                                                                       |
| --------------------- | ------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `on_connect`          | `input.ffmpeg`: none; `input.http`: `[string * string]` ICY headers | Once per connection, after the decoder is built and before the state becomes `Connected` (§2.2 step 8).                                                     |
| `on_disconnect`       | none                                                                | Once per connection whose `on_connect` callbacks ran, when that connection ends, whatever the cause (§2.5).                                                 |
| `on_error`            | `error`                                                             | Each source fault: each failed attempt (§2.3), each feeding error (§4). Error kind `"ffmpeg"`. Never for a clean end of input (§2.5) nor for an item fault. |
| `on_start`, `on_stop` | none                                                                | Generic start/stop transitions.                                                                                                                             |

For one connection, `on_disconnect` runs after its `on_connect` callbacks
returned. A synchronous callback runs on the thread that makes the transition
it belongs to.

**Conformance.**

- `input.ffmpeg(format="nonexistent", …)` fails at evaluation.
- `input.ffmpeg(int_args=[("bogus_opt", 1)], …)` fails at evaluation (§2.3).
- `input.http` with a user `rw_timeout` in `int_args` opens with the value
  derived from `timeout`.

### 1.2 Script wrappers

| Wrapper                                                                               | Expands to                                                                                                    | Returned type               |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- | --------------------------- |
| `input.v4l2(~id=null, ~max_buffer=0.5, ~self_sync=true, ~device="/dev/video0")`       | `input.ffmpeg(id, format="v4l2", max_buffer, self_sync, device)`                                              | `source(video=yuv420p('a))` |
| `input.rtmp(~id=null, ~max_buffer=5., ~listen=true, url)`                             | `input.ffmpeg(id, max_buffer, format="live_flv", self_sync=true, int_args=[("listen", listen ? 1 : 0)], url)` | `source('a)`                |
| `video.testsrc.ffmpeg(~id=null, ~pattern="testsrc", ~max_buffer=0.5, ~duration=null)` | `input.ffmpeg(id, max_buffer, format="lavfi", src)`                                                           | `source(video=yuv420p('b))` |
| `input.hls(~id=null, uri)`                                                            | `input.ffmpeg(id=id, uri)` with every other argument at its default                                           | `source('a)`                |

`input.v4l2` paces its clock by default: a capture device delivers frames at
its own rate and the read blocks until the next frame ([§7](#7-clock-and-self_sync)).
Its `self_sync` argument documentation states the consequence: a clock
holds one sync source at a time, so a camera in the same clock as another
sync source (a sound card input or output, a second camera, a live network
input with `self_sync`) raises the clock's sync error
([pacing.md §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)).
The fix it names is `self_sync=false` on the camera, or `buffer()` around one
side to put it in its own clock.

`video.testsrc.ffmpeg`: `duration` is a nullable number of seconds and
accepts both an int and a float, through the language's numeric type
constraint: its type is `'a?` with `'a` constrained to be a number (printed
`'a where 'a is a number`), the constraint the arithmetic operators use. Any
other value is a type error.
`src` is built at call time as `"<pattern>=size=<W>x<H>:rate=<R>"`, followed
by `":duration=<D>"` when `duration` is not null, where W, H, R are the
current values of `settings.frame.video.width`, `.height`, `.framerate` and D
is `duration` written as a decimal number that FFmpeg's duration syntax
accepts. Examples, with defaults: `duration=10` and `duration=10.` both give
`testsrc=size=1280x720:rate=25:duration=10`; `duration=2.5` gives
`…:duration=2.5`.

`input.v4l2` and `video.testsrc.ffmpeg` annotate their result as
`source(video=yuv420p)`, which hides `input.ffmpeg`'s methods and callbacks.

`input.hls`: when `input.ffmpeg` exists, `input.hls` is a function over it
whose type has its type variable generalised (`source('a)`), like every
other input.

**Conformance.**

- `liquidsoap -h input.hls` prints a lowercase type variable.
- `video.testsrc.ffmpeg(duration="x")` is a type error;
  `duration=10` and `duration=10.` both typecheck and produce a ten-second
  source.

### 1.3 `input.harbor.dynamic.regexp` / `input.harbor.dynamic`

`input.harbor.dynamic.regexp` takes the generic harbor input arguments
(`buffer` default `12.0` here, `max` `20.0`, `timeout` `30.0`, `user`
`"source"`, `password` `"hackme"`, `port` `8005`, `transport`, `icy` `false`,
`icy_metadata_charset`, `metadata_charset`, `replay_metadata` `false`, `auth`,
`dumpfile`, `logfile`, `debug` `false`), an unlabeled `regexp` mountpoint, and
a required `on_connect`. It returns `unit`. `buffer >= max` is a script error
raised on `max` (example message:
`Maximum buffering inferior to pre-buffered data`).

`input.harbor.dynamic(..., path)` is the same with `path : string`,
converted by `harbor.http.regexp_of_path`.

`on_connect` type, with `…` standing for the generic source methods:

```
({ copy_encoder : (?string?, ?options : [string * string]) -> format('a),
   format : string?, headers : [string * string], mime_type : string?,
   query : [string * string],
   streams : [{ channel_layout? : string, channels? : int, codec : string,
                field : string, frame_rate? : float?, height? : int,
                pixel_format? : string, samplerate? : int, type : string,
                width? : int }],
   uri : string })
-> (source('a).{ …, buffer_length : () -> float, connected_client : () -> string?,
                 on_connect : (synchronous : bool, (([string * string]) -> unit)) -> {release : () -> unit},
                 on_disconnect : (synchronous : bool, (() -> unit)) -> {release : () -> unit},
                 status : () -> string, stop : () -> unit })
-> unit
```

The source handed to the inner function carries every generic source method
(`id`, `is_ready`, …) plus the methods and callbacks listed.

### 1.4 `output.url`

```
output.url : (?id : string?, ?export_cover_metadata : bool, ?fallible : bool,
 ?register_telnet : bool, ?restart_delay : float?, ?self_sync : bool,
 ?start : bool, ?timeout : float, url : string, format('a), source('a)) -> unit
```

| Argument                                                        | Default                  | Meaning                                                                                                  |
| --------------------------------------------------------------- | ------------------------ | -------------------------------------------------------------------------------------------------------- |
| `url`                                                           | required                 | Output URL handed to the muxer.                                                                          |
| `restart_delay`                                                 | `2.0`                    | Seconds before a retry after a transient failure; `null`: the output stops at its first failure (§11.3). |
| `self_sync`                                                     | `false`                  | Declares a sync source (§11.4).                                                                          |
| `timeout`                                                       | `10.0`                   | Longest a single blocking open or write may last (§3).                                                   |
| `fallible`, `start`, `register_telnet`, `export_cover_metadata` | generic output arguments |                                                                                                          |

The format must be `%ffmpeg`; any other is a script error raised on the
format value (example message:
`Encoding format does not support output url!`). The output's encoder writes to the URL instead of returning bytes.
Callback `on_error : (error) -> unit`, error kind `"output"`, plus the generic
output callbacks (`on_start`, `on_stop`, `on_output`).

`output.youtube.live.rtmp` is `output.url(url="#{url}/#{key}", encoder, s)`
with `url` default `"rtmp://a.rtmp.youtube.com/live2"`.

## 2. `input.ffmpeg` connection lifecycle

This section also covers `input.http` and every wrapper of §1.2.

### 2.1 States

An **attempt** is one try at connecting. Each attempt has a fresh identity
and its own **abort flag**. The source holds one state; each state carries
the data that exists only in it:

| State                 | Carries                                                                                                  | `status`          |
| --------------------- | -------------------------------------------------------------------------------------------------------- | ----------------- |
| `Stopped`             | nothing                                                                                                  | `stopped`         |
| `Starting a`          | the next attempt `a` and the time it runs                                                                | `starting`        |
| `Polling a`           | attempt `a`, running                                                                                     | `polling`         |
| `Connected (a, u, c)` | attempt `a`, URL `u`, connection `c`: container, decoder, decoder buffer, whether `c` delivered data yet | `connected <url>` |
| `Stopping a`          | attempt `a`, aborted and still running                                                                   | `stopping`        |
| `Failed (u, e)`       | the URL `u` that failed permanently and the error `e`                                                    | `failed`          |

Every transition is one indivisible compare-and-set on the whole state: it
takes effect only when the state is exactly the expected one, attempt
identity included, and reports whether it did. A party that loses a
transition does not write the state.

**Ownership.** An attempt owns what it opened until it moves the state from
`Polling a` to `Connected (a, u, c)`; from then on `c` belongs to the state.
Whoever moves the state out of `Connected (a, u, c)` owns `c` and ends it
(§2.5). An attempt that fails to make its last transition owns what it
opened and releases it. Every path through these rules releases each
container and decoder exactly once.

Transitions:

| Event                                     | From                  | To                           | Who                   | Effect                                                                    |
| ----------------------------------------- | --------------------- | ---------------------------- | --------------------- | ------------------------------------------------------------------------- |
| start                                     | `Stopped`, `Failed`   | `Starting a` (now)           | start/stop layer      | schedule `a`                                                              |
| start                                     | `Stopping b`          | `Starting a` (now)           | start/stop layer      | schedule `a`; `b` finishes on its own                                     |
| start                                     | any other             | unchanged                    |                       |                                                                           |
| attempt `a` runs                          | `Starting a`          | `Polling a`                  | attempt               | §2.2; when the state is anything else, `a` ends at once, touching nothing |
| attempt `a` succeeds                      | `Polling a`           | `Connected (a, u, c)`        | attempt               | report the sync source (§7)                                               |
| attempt `a` succeeds, stop won            | `Stopping a`          | `Stopped`                    | attempt               | release `c`, run `on_disconnect`                                          |
| attempt `a` fails, transient              | `Polling a`           | `Starting b` (+`poll_delay`) | attempt               | §2.3                                                                      |
| attempt `a` fails, permanent              | `Polling a`           | `Failed (u, e)`              | attempt               | §2.3                                                                      |
| attempt `a` fails, stop won               | `Stopping a`          | `Stopped`                    | attempt               | release what `a` opened; report nothing                                   |
| attempt `a` ends after a later start      | `Starting b`, …       | unchanged                    | attempt               | release what `a` opened; report nothing                                   |
| end of input                              | `Connected (a, u, c)` | `Starting b`                 | streaming thread      | §2.5                                                                      |
| feeding error                             | `Connected (a, u, c)` | `Starting b`                 | streaming thread      | §2.5                                                                      |
| stop                                      | `Starting a`          | `Stopped`                    | start/stop layer      | cancel `a`                                                                |
| stop                                      | `Polling a`           | `Stopping a`                 | start/stop layer      | set `a`'s abort flag                                                      |
| stop                                      | `Connected (a, u, c)` | `Stopped`                    | start/stop layer      | set `a`'s abort flag, end `c` (§2.5)                                      |
| stop                                      | `Failed`              | `Stopped`                    | start/stop layer      | cancel the URL check                                                      |
| stop                                      | `Stopped`, `Stopping` | unchanged                    |                       |                                                                           |
| URL check finds another URL, or `set_url` | `Failed (u, e)`       | `Starting a` (now)           | URL check / `set_url` | schedule `a`                                                              |

Start and stop come from the generic start/stop layer: waking up starts the
source when `start=true`; going to sleep, `stop()` and `Idle` stop it; the
generic `reset` is a stop then a start. A stopped source holds no scheduled
task and nothing scheduled holds a stopped source: the clock's rule that a
source which left it is not kept alive
([clock.md §7](../../../clock/spec/clock.md#7-sources-on-a-clock)) holds for
this source too.

The source is **ready** when it is started and either the state is
`Connected` or it still holds data of a connection that ended (§2.5).

The attempt identity is what makes every race safe: a disconnect racing a
successful connect (the attempt moves `Polling a` → `Connected` while the
stop reads `Polling a`) resolves to one of two orders, and both end
`Stopped` with `c` released and `on_disconnect` run once.

**Conformance.**

- Stop the source while an attempt is opening, at every point of §2.2:
  afterwards the state is `Stopped`, no container is left open, and
  `on_disconnect` ran exactly as many times as `on_connect`.
- Start and stop the source many times while the remote accepts slowly: the
  number of open containers returns to zero.
- A source replaced inside `source.dynamic` is stopped and then collected.

### 2.2 One connection attempt

Attempt `a` runs on a scheduler task, never on the streaming thread:

1. Transition `Starting a` → `Polling a`. On failure, end at once.
2. Copy the option table; read the URL getter (trimmed when `trim_url`).
3. Open the input with the bindings' `open_input`, passing: the interrupt
   function "`a`'s abort flag or shutdown" (§3), `format` when given, the
   option copy, and the decoder's stream configuration callbacks
   ([decoder.md §5.1](decoder.md#51-opening)).
4. If the option copy is not empty: permanent failure
   `Unrecognized options: <name>=<value>,...` (example:
   `Unrecognized options: bogus_opt=1`).
5. Compute the container's content type against the source's content type
   ([decoder.md §5.2](decoder.md#52-container-description-and-content-type)).
   If the decoder registry cannot produce the source's type from it: a
   probably-permanent failure (§2.3),
   `url "<url>" cannot produce content of type <type>`. This covers a copy
   relay whose remote restarts with new codec parameters: the content-format
   merge rejects the new stream at reconnect
   ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)), and the source stays
   not ready, retrying on the back-off delay, until a compatible stream comes
   back. A script that must follow such a change re-encodes, or turns on
   `settings.ffmpeg.content.copy.relaxed_compatibility_check`.
6. Build a decoder record from the container with "decode first metadata"
   enabled, and a decoder buffer writing into the source's buffer
   ([decoder.md §15](decoder.md#15-interfaces-reused-elsewhere)). Set the
   buffer's maximum to `max_buffer` (§5). Reset the metadata deduplication
   memory (§6).
7. Compute the connect metadata (empty for `input.ffmpeg`, ICY headers for
   `input.http`, §8).
8. Run every `on_connect` callback with it. The state is `Polling a`:
   `status()` inside `on_connect` returns `"polling"`.
9. Transition `Polling a` → `Connected (a, u, c)`, then report the sync
   source (§7). If the transition fails, a stop won: release `c`, run
   `on_disconnect`, transition `Stopping a` → `Stopped` (ignored when the
   state is something else).

The first data of each connection starts a new track: exactly one track
boundary separates the data of two connections.

### 2.3 Failure of an attempt

A failure in steps 2–8 is handled in this order:

1. Release what the attempt opened: the decoder record when built (which
   closes the container), else the container when opened. When `on_connect`
   callbacks ran, run `on_disconnect`.
2. If the state is no longer `Polling a`, a stop or a later start won:
   transition `Stopping a` → `Stopped` (ignored when the state is something
   else) and end. No error is logged or reported.
3. Classify the failure, make the transition, then log and report it.

**Classes.**

- **Permanent**: the failure follows from the operator's own options and
  the URL value alone, so no change on the remote side can fix it. The only
  such failure is the leftover options of step 4: which options the demuxer
  and the protocol consume is fixed by the options and the URL. An option
  name that no FFmpeg input component (generic container options, any
  demuxer, any protocol) defines is a configuration error, reported at
  evaluation before anything streams. An option some component defines but
  the chosen URL leaves unused is known only at connect. It is a source
  fault, not a hard error, because the URL is a getter: a later URL can use
  the option. Transition `Polling a` → `Failed (u, e)`.
- **Probably permanent**: the remote answered, and its answer would fail the
  same way if it did not change, but a remote can change while a stream runs
  for months. These are the content-type mismatch of step 5 (the remote
  serves a stream without the media the source needs) and an HTTP client
  error status (4xx: a mount not up yet, an expired token). Transition
  `Polling a` → `Starting b`, `b` running after the **back-off delay**.
- **Transient**: every other failure, the open's network errors
  (unreachable, refused, timeout, a 5xx status) and an error raised by an
  `on_connect` callback included. Transition `Polling a` → `Starting b`, `b`
  running `poll_delay` seconds later.

**Back-off delay.** `poll_delay` at the first probably-permanent failure,
doubled at each consecutive one, capped at the parameter
`slow_retry_max_delay` (recommended 60 s). A successful connection and a URL
change reset it. The cap protects the log, the error handlers and the remote
from a reconnect flood while the source still recovers on its own within one
cap of the remote being fixed.

Rationale: a long-running stream recovers without a restart whenever the
remote can fix the cause; only a failure that the script's own
configuration fixes is given up on, and even that one is retried when the URL
changes.

Every failure of an attempt is a source fault
([faults.md §3](faults.md#3-source-faults-reach-the-script)): the source is
not ready while it lasts, and `on_error` carries it to the script.

Reporting: log at error level (example: `Connection failed: <error>`), with
the backtrace at debug level; build a runtime error of kind `"ffmpeg"` from
the failure; run every `on_error` callback. Inside them `status()` is
`"starting"` or `"failed"`. A permanent failure is reported once. A
probably-permanent failure is reported at each attempt.

**Leaving `Failed`.** While `Failed (u, e)`, the source re-reads its URL
getter every `poll_delay` seconds without connecting. When the getter returns
a URL other than `u`, or `set_url` is called, or the source is stopped and
started again, the source leaves `Failed` (§2.1).

**`debug=true`.** Every fault is logged with its backtrace at error level.
Nothing else changes: no fault raises out of the source.

**Conformance.**

- An option that some protocol defines but the URL's protocol does not
  consume: `on_error` runs once, the state stays `failed`, and no further
  connection is opened over ten `poll_delay`s.
- A permanent failure followed by `set_url` to a valid URL connects.
- A server that answers 404 and then starts serving the mount is connected
  within `slow_retry_max_delay` of the change, without a restart.
- A content-type mismatch is retried with delays `poll_delay`,
  `2 × poll_delay`, … up to `slow_retry_max_delay`.
- An unreachable URL is retried every `poll_delay` and runs `on_error` each
  time; each failed attempt leaves no container open.
- `input.ffmpeg("invalid")` reports an error of kind `ffmpeg` whose cause is
  "no such file or directory".

### 2.4 Shutdown

A process-wide `shutdown` flag is raised by the "before core shutdown"
lifecycle hook. Every interrupt function of §3 returns true once it is
raised, so every blocking call of every connection of this file ends. A
connection that ends because of shutdown is not an end of input (§2.5).

### 2.5 End of a connection

A connection `c` ends in one of these ways. The party that moved the state
out of `Connected (a, u, c)` ends it:

| Cause         | Class                                                                     | Detected by                                                      | Data the decoder holds                                                                                                                                                              | `on_error` | Next state   |
| ------------- | ------------------------------------------------------------------------- | ---------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ------------ |
| end of input  | none: a normal end of track                                               | `decode` returns end of input                                    | drained ([draining.md](draining.md), [decoder.md §13.1](decoder.md#131-end-of-input)) into the source's buffer, followed by the end of the track                                    | no         | `Starting b` |
| feeding error | source fault ([faults.md §3](faults.md#3-source-faults-reach-the-script)) | `decode` returns a source fault (a read past `timeout` included) | what the decoder drained before reporting the fault ([decoder.md §13.2](decoder.md#132-errors): every stage but the one that failed) is delivered, followed by the end of the track | yes        | `Starting b` |
| abort         | none                                                                      | `decode` returns an abort (§3)                                   | discarded: the connection was stopped or the application is shutting down                                                                                                           | no         | as the stop  |
| stop          | none                                                                      | start/stop layer                                                 | discarded with the source's buffer: the reader has gone                                                                                                                             | no         | `Stopped`    |
| shutdown      | none                                                                      | the shutdown flag                                                | discarded                                                                                                                                                                           | no         | `Stopped`    |

Ending `c`, in order: clear the source's sync source (§7); drain or discard
as above; close the decoder, which closes the container (a failure to close
is logged at error level, example `Error while disconnecting: <error>`, and
the end continues: the connection is already over, so the failure has no
further effect); run `on_disconnect`.

While the source is between connections it is not ready, once the data of
the ended connection is consumed. `on_disconnect` and `on_error` carry the
source fault to the script.

`decode` never raises: it returns data, an end of input, an abort or a
source fault ([decoder.md §8.2](decoder.md#82-one-decode-step)). An abort
means a stop or a shutdown already owns the connection, so the streaming
thread leaves the state alone.

An end of input is logged at info level.

**Next attempt delay.** `Starting b` runs at once when `c` delivered at least
one frame of data, and after `poll_delay` otherwise. Rationale: a remote that
accepts connections and ends them without data would otherwise be
reconnected in a tight loop.

A finite input therefore plays again from the start after its end. Example:
`input.ffmpeg` on `lavfi` `sine=frequency=440:duration=1` plays one second,
runs `on_disconnect`, then `on_connect`, and plays again, without
`on_error`.

**Conformance.**

- A finite input of known length (a lavfi source with a duration, a file
  served over HTTP) delivers every sample and every frame of it per
  connection, with the codec's delay included (the counts of
  [draining.md §6](draining.md#6-conformance)).
- Its end runs no `on_error` and logs nothing at error level.
- A remote that closes every connection at once is reconnected no more often
  than every `poll_delay`.

## 3. Interrupting blocking libav calls

Every blocking call these operators make:

| Call                                                     | Thread                           | Aborted by                                        | Bounded by                                             |
| -------------------------------------------------------- | -------------------------------- | ------------------------------------------------- | ------------------------------------------------------ |
| input open (§2.2 step 3)                                 | attempt task                     | interrupt: the attempt's abort flag or `shutdown` | `timeout`                                              |
| input read inside `decode` (§4)                          | streaming thread                 | the same interrupt, for the connection's lifetime | `timeout`; `input.http` also passes it as `rw_timeout` |
| decoder and container close                              | the thread ending the connection | the same interrupt                                | —                                                      |
| harbor client read ([§10.3](#103-opening-the-container)) | client task                      | the client ending, `shutdown`                     | `timeout` per wait                                     |
| `output.url` open ([§11.2](#112-start-encode-stop))      | connect task                     | interrupt: the attempt's abort flag or `shutdown` | `timeout`                                              |
| `output.url` write inside `encode`                       | streaming thread                 | the same interrupt                                | `timeout`                                              |

The interrupt function is installed when the container is opened and covers
every blocking call of that container ([avformat.md §7.1.4](../../../../modules/synced/ffmpeg/spec/avformat.md#714-interrupt--interrupt-)).
When it returns true, the blocked call returns with the `Exit` error.

The abort flag is per attempt: a stop sets it, so a pending open of attempt
`a` aborts while a later attempt `b` has its own flag. Once connected, the
same flag aborts every later read or write of that connection.

A read or write on the streaming thread blocks the clock for as long as it
lasts. That is how a self-paced input or output paces its clock (§7, §11.4).
Its bound against a stalled remote is the operator's `timeout`: the
interrupt function also returns true once the blocking call in progress has
lasted `timeout` seconds. The bound lives in the interrupt rather than in a
protocol option because a device or a lavfi input consumes no protocol
option. An input read past its bound is a source fault (§2.5); an
`output.url` open or write past its bound is an output fault (§11.3). A stop
or a shutdown ends the call at once. A connection open never runs on the streaming
thread, because a connect to an unreachable host lasts the protocol's whole
connect timeout.

**Conformance.**

- Stopping the application while an input reads from a remote that sends
  nothing returns within the interrupt's polling interval.
- `output.url` to an unreachable host leaves the clock ticking: other outputs
  of the same clock keep producing while the open is pending.

## 4. Frame generation

Each streaming cycle on a ready source:

1. When the state is `Connected (a, u, c)`: while the source's buffer holds
   less than one frame (`Frame.size` main ticks) and neither `a`'s abort flag
   nor `shutdown` is set, call the decoder's `decode` once.
2. Slice up to one frame from the buffer.
3. Apply the frame-level metadata processing of §6.

An end of input or a source fault returned by `decode` in step 1 ends `c`
(§2.5) and the cycle
continues with step 2 on what the buffer holds. Data left in the buffer after
the connection ended is delivered by the following cycles; the source stays
ready until it is consumed.

A feeding error is a source fault. It is logged at error level (example:
`Feeding failed: <error>`) with the backtrace, and runs `on_error` with an
error of kind `"ffmpeg"`. Item faults inside the data (a corrupt packet, a
non-increasing timestamp) are dropped by the decoder and escalate under its
bound `max_consecutive_faults`
([decoder.md §13](decoder.md#13-end-of-stream-and-errors)); an escalated run
is returned by `decode` as a source fault.

A read the transport cannot serve yet (would-block) is read again by the
decoder ([decoder.md §8.2](decoder.md#82-one-decode-step)); it is neither an
end of input nor an error.

Decoding runs on the streaming thread (§3).

## 5. Buffering

The source's buffer holds decoded data between the decoder and the
streaming cycle. `max_buffer` (seconds, converted to main ticks) is its
maximum length, set at each connection. It protects memory.

§4 decodes only until one frame is buffered, so the buffer normally holds
under one frame plus what one `decode` call produced. The bound applies when
one stream of the container runs far ahead of another: the buffer then holds
the leading stream's data until the lagging one reaches one frame. When
content added to the buffer makes it exceed `max_buffer`, the oldest excess
is dropped (generic buffer behaviour; example:
`Generator max buffered length exceeded (<max> < <len>)! Dropping content..`).
Each such drop is an item fault ([faults.md §4](faults.md#4-item-faults-escalate)):
logged as a warning, counted per source. After `max_consecutive_faults`
([faults.md §4](faults.md#4-item-faults-escalate)) consecutive streaming cycles that
each dropped, the source treats it as a feeding error (§2.5): a container
whose streams stay that far apart is broken, and the script is told.

`remaining` and `seek` read the connected state; see §1.1.

## 6. Metadata

Metadata enters the buffer from the decoder: container metadata on change
and stream metadata, the first set included
([decoder.md §10](decoder.md#10-metadata)). `input.ffmpeg` filters it at frame
level, after slicing:

1. For each `(position, metadata)` of the frame, apply the effective filter.
   Keep the pair only when the result is non-empty.
2. Replace the frame's metadata by the kept pairs.
3. When `new_track_on_metadata`, put a track boundary at the position of each
   kept pair. A position carries at most one track boundary: a pair at the
   start of a connection's data shares the connection's boundary.

Effective filter:

1. Base: the user `metadata_filter` when given; else drop every key matching
   the regular expression `^id3v2_priv`.
2. When `deduplicate_metadata`: apply the base; if the result is empty, or its
   sorted list equals the last kept sorted list of the same connection, return
   empty; else remember the sorted list and return the result.

The deduplication memory is reset at each connection (§2.2 step 6): the first
metadata of a connection is always kept, since it describes a new stream.

**Conformance.**

- Metadata arriving mid-frame starts the new track at its own position.
- A stream that drops and reconnects with the same title as before the drop
  delivers that title again, with a new track.

## 7. Clock and `self_sync`

`input.ffmpeg` and `input.http` are active sources. Each has one sync source
of kind device-paced (`self-paced`,
[pacing.md §2](../../../clock/spec/pacing.md#2-sync-source)): the read in
`decode` blocks until the remote or the device has the next data, and that
blocking paces the stream. Its identity is one per source, kept across
reconnects; its name is the source's id.

The source reports this sync source while all of these hold, and none
otherwise ([pacing.md §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)):

- the state is `Connected`;
- the source is ready;
- the `self_sync` getter (for `input.http`, §8) returns true.

The getter is read each time the source prepares a streaming cycle, so a
change takes effect at the next cycle. On connect, the source reports after
the state became `Connected`; on the end of a connection, it clears its
report before releasing the connection (§2.5). A clock following the sync
source changes pacing within one tick of the report
([pacing.md §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)).

`self_sync=true` is for live URLs: a stream that a server or a device
delivers at its real-time rate, so that reads block until the next data.
The argument's documentation says so. On an input whose reads do not block
(a local file, an on-demand HTTP file, a lavfi generator) the clock then runs
as fast as the reads return.

Defaults: `input.rtmp` and `input.v4l2` pass `self_sync=true` (a pushed live
stream, a capture device); `video.testsrc.ffmpeg` (a generator) and
`input.hls` (segments fetched in bursts) pass `false`.

**Conformance.**

- `input.ffmpeg(self_sync=true, …)` on a live stream: over a measured span,
  stream time equals real time within the clock's tolerance, and the clock
  never rests.
- Flipping the `self_sync` getter while connected changes the clock's
  followed sync source within two ticks.
- `input.v4l2` and an `input.alsa` with `self_sync=true` in one clock raise
  the clock's sync error.

## 8. `input.http` specifics

- Name `input.http`; options of §1.1 forced.
- Connect metadata: read the string AVOption `icy_metadata_headers` from the
  input, searching children. Split on `\r?\n`; for each non-empty line
  matching `([^:]*):\s*(.*)` add `(name, value)`, in line order; a line that
  does not match is skipped. A failure to read the option is an item fault:
  logged as a warning (example: `Error while fetching icy headers: <error>`),
  it yields `[]` and the connection continues.
- `is_icy` is "the list is non-empty", set at each connection.
- `self_sync`: the user value when not null; else `is_icy`.
- `seek` returns `-1` when `is_icy`, else delegates.
- `on_connect` callbacks receive the header list.

## 9. Failure summary (`input.ffmpeg`, `input.http`)

Classes are those of [faults.md §2](faults.md#2-classes).

| Failure                                                                                      | Class                | Logged           | Callbacks                                       | Next                                                       |
| -------------------------------------------------------------------------------------------- | -------------------- | ---------------- | ----------------------------------------------- | ---------------------------------------------------------- |
| Unknown `format` name                                                                        | configuration error  | —                | —                                               | script error at evaluation                                 |
| Option no FFmpeg input component defines                                                     | configuration error  | —                | —                                               | script error at evaluation                                 |
| Open fails (unreachable, refused, 5xx, `timeout`)                                            | source fault         | error            | `on_error`                                      | not ready; retry after `poll_delay`                        |
| Option the URL's components leave unused                                                     | source fault         | error, once      | `on_error`, once                                | not ready; `Failed` until the URL changes or a restart     |
| Content type mismatch, HTTP 4xx                                                              | source fault         | error            | `on_error`                                      | not ready; retry after the back-off delay                  |
| Error raised by an `on_connect` callback                                                     | source fault         | error            | `on_disconnect`, `on_error`                     | not ready; retry after `poll_delay`                        |
| Stop during an attempt                                                                       | none                 | —                | `on_disconnect` when `on_connect` ran           | `Stopped`                                                  |
| End of input while connected                                                                 | none                 | info             | `on_disconnect`, then `on_connect` on reconnect | drain, end of track, reconnect                             |
| Source fault returned by `decode` (decode error, escalated item faults, read past `timeout`) | source fault         | error            | `on_error`, `on_disconnect`                     | drained data delivered, end of track, not ready, reconnect |
| Buffer overflow drop                                                                         | item fault           | warning, counted | —                                               | continue; `max_consecutive_faults` escalates               |
| ICY header read failure                                                                      | item fault           | warning          | —                                               | continue with `[]`                                         |
| Decoder close fails                                                                          | none (already ended) | error            | `on_disconnect` still runs                      | as the cause of the end                                    |

Example cause for a missing file: `No such file or directory`.

## 10. `input.harbor.dynamic`

### 10.1 Registration

Calling the operator forces `settings.init.force_start` to true and registers
one harbor handler on `port` for the mountpoint regexp, with the given
transport and `icy`. The handler keeps a table of live clients, each entry a
`(mount, source)` pair, `mount` being the request URI without one leading `/`
(when the URI is longer than one character). An entry exists exactly while
its client is not `Ended` (§10.2).

- ICY metadata updates and MIME queries for a mount are routed to the first
  live entry whose mount equals the trimmed request mount; with none, they
  are ignored / answer none.
- Login is the generic harbor login built from `user`, `password`, `auth`.

### 10.2 Per client

Each accepted source client gets its own source. Its id is the URI without
its leading `/`, or `input.harbor.dynamic` when the URI is empty or `/`. The
client goes through these states:

| State                  | Carries                                                                            |
| ---------------------- | ---------------------------------------------------------------------------------- |
| `Opening`              | the client connection (one use held), the request                                  |
| `Offered (k, d)`       | the opened container `k`; the deadline `d` = time the handler returned + `timeout` |
| `Feeding (k, decoder)` | the decoder on `k`, the feed task                                                  |
| `Ended`                | nothing                                                                            |

Transitions:

| Event                                                    | From            | To               |
| -------------------------------------------------------- | --------------- | ---------------- |
| client accepted                                          | —               | `Opening`        |
| container opened and described, handler returned (§10.3) | `Opening`       | `Offered (k, d)` |
| open, description or handler fails                       | `Opening`       | `Ended`          |
| source wakes up and the content type matches (§10.6)     | `Offered`       | `Feeding`        |
| source wakes up and the content type does not match      | `Offered`       | `Ended`          |
| deadline `d` passes without a wake-up                    | `Offered`       | `Ended`          |
| end of input, feeding error                              | `Feeding`       | `Ended`          |
| sleep, `stop()`, shutdown, client aborted                | any but `Ended` | `Ended`          |
| wake-up                                                  | `Ended`         | `Ended`          |

Transitions are compare-and-set on the state, as in §2.1. Whoever moves the
state into `Ended` ends the client, exactly once and in order: stop the feed
task; drain the decoder when the cause is an end of input (§10.6), else
discard; close the decoder, or the container when no decoder was built;
close the client connection; remove the client's table entry; run the
source's `on_disconnect` callbacks when its `on_connect` callbacks ran.

Every way a client ends other than a clean end of input, a sleep, a `stop()`
or shutdown is a source fault of the client's source
([faults.md §3](faults.md#3-source-faults-reach-the-script)): an open,
description or handler failure, a content-type mismatch, a feeding error, a
read past `timeout`, the deadline. It is logged at error level with the
client URI (the deadline at warning level), its track ends, and the source,
which is fallible, is not ready from then on; `on_disconnect` carries it to
the script. A fault of one client never affects another client or the
harbor.

The deadline bounds what a client costs when the handler attaches its source
to nothing that runs: the connection and the table entry are released after
`timeout`, and a later client on the same mount is not shadowed. The
deadline is logged at warning level when it fires.

A source is single-use: once `Ended`, a later wake-up feeds nothing and the
source never becomes ready. A client that reconnects is a new client with a
new source.

**Conformance.**

- A handler that drops its source: after `timeout`, the client socket is
  closed and the table holds no entry for the mount.
- An open that fails leaves no table entry and a closed socket.
- Waking a source again after it slept produces no data and no error.

### 10.3 Opening the container

Runs on a scheduler task, holding one use of the client connection:

1. Map the lower-cased MIME type (the `Content-Type` up to the first `;`) to
   an input format:

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

2. Open with the bindings' `open_input_stream`, empty options, over the
   client read function below.
3. Describe the container ([stream-description.md §2](stream-description.md#2-container-format-url-input),
   [§5](stream-description.md#5-describe)) with the request URI as its URL.
   Log at info level the description (example: `Container: <description>`).
4. Build the connection record (§10.4) and the source value with its methods
   and callbacks.
5. Release the use of the connection taken for the open.
6. Apply the handler to the record, then the returned function to the source.
7. Transition `Opening` → `Offered`.

A failure in steps 2–6 ends the client (§10.2), logged at error level with
the client URI (example: `Error while opening container: <error>!`).

**Client read function.** libav calls it for bytes; its results are those of
the bindings' custom read ([avformat.md §7.1.1](../../../../modules/synced/ffmpeg/spec/avformat.md#711-custom-read--read-buffer-offset-length)):

| Situation                                   | Result                                                                                     |
| ------------------------------------------- | ------------------------------------------------------------------------------------------ |
| bytes available on the socket               | copy them, write them to `dumpfile` and log them to `logfile` when set, return their count |
| no bytes yet, or the harbor signals a retry | wait for readability and try again; neither end of input nor an error                      |
| no bytes after waiting `timeout` seconds    | the timeout error code: a feeding error                                                    |
| the peer closed the connection              | 0: end of input                                                                            |
| the client ended (§10.2) or `shutdown`      | the exit error code (`AVERROR_EXIT`): an abort                                             |
| any other failure                           | logged at error level, an I/O error code: a feeding error                                  |

A client ending wakes a read blocked in its wait. `open_input_stream` takes
no interrupt function; the read function is the only blocking I/O of the
container and checks the abort itself.

### 10.4 Connection record

| Field          | Value                                                                                                                               |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `uri`          | Request URI.                                                                                                                        |
| `query`        | Regexp groups of the mount match, as `(label, value)` pairs.                                                                        |
| `format`       | Container format name from the description, or null.                                                                                |
| `mime_type`    | The client MIME when it contains `/`; else the MIME of the described format; else null.                                             |
| `streams`      | One record per described stream, in description order ([stream-description.md §7](stream-description.md#7-script-visible-records)). |
| `headers`      | Request headers as received.                                                                                                        |
| `copy_encoder` | Function returning a `%ffmpeg` copy format for this container (§10.5).                                                              |

Example, an `output.icecast` of
`%ffmpeg(format="ogg", %audio(codec="libvorbis"))` on mount `live`:
`uri=/live`, `query=[]`, `format=ogg`, `mime=application/ogg`, one stream
`field=audio type=audio codec=vorbis samplerate=44100 channels=2 layout=stereo`.

### 10.5 `copy_encoder`

`copy_encoder(?format, ?options)` returns a `%ffmpeg` format with:

- `format`: the argument when not null, else the described container format;
- `mime_type`: as `mime_type` above;
- output to a stream, interleaving `default`, no metadata;
- one stream per described audio, video and subtitle stream, keyed by its
  field, in copy mode waiting for a keyframe; data streams are omitted;
- `options` as string options.

Rendered examples: `copy_encoder(null, options=[])` →
`%ffmpeg(format="ogg",interleaved="default",%audio.copy(wait_for_keyframe))`;
`copy_encoder("matroska", options=[("live","1")])` →
`%ffmpeg(format="matroska",interleaved="default",%audio.copy(wait_for_keyframe),live="1")`.

The stdlib's `icecast.server` (extra library) builds outputs with
`copy_encoder(format, options=…)` and the setting
`settings.icecast.server.default_muxer_options`, default
`[("matroska", [("dash","1"),("live","1")]), ("webm", [("dash","1"),("live","1")])]`.

### 10.6 Feeding

When the source wakes up in `Offered (k, d)`:

1. Compute the content type of `k` against the source's content type and
   check that the decoder registry can produce it, as `input.ffmpeg` does
   (§2.2 step 5). A mismatch ends the client, logged at error level with the
   client URI and both types.
2. Build a decoder record on `k` for the source's content type, "decode
   first metadata" enabled, and a decoder buffer on the source's buffer
   ([decoder.md §15](decoder.md#15-interfaces-reused-elsewhere)). Set the
   buffer's maximum to `max` seconds.
3. Transition `Offered` → `Feeding`. Add pending ICY metadata, run the
   source's `on_connect` callbacks with the request headers.
4. Start the feed task: it calls `decode` repeatedly. It decodes bytes the
   container already read without waiting on the socket, and waits only
   inside the read function when the container needs more bytes. Feeding
   runs off the streaming thread.
5. End of input (the read function returned 0): drain the decoder
   ([draining.md](draining.md)) into the source's buffer, end the track, end
   the client. A source fault returned by `decode`: deliver what the
   decoder drained before reporting it
   ([decoder.md §13.2](decoder.md#132-errors)), end the track, log at error
   level (example: `Feeding stopped: <error>`), end the client. An abort
   discards and ends the feed task without a report.

The source is ready once `buffer` seconds are buffered (generic harbor input)
and stays ready while it holds data. Going to sleep and the `stop` method end
the client.

**Conformance.**

- A client that sends a whole file and closes: every sample of the file
  reaches the source's reader, and the track ends after the last one.
- A client that pauses sending for less than `timeout` does not end its
  track.
- `dumpfile` receives exactly the bytes the client sent.
- A client whose stream has no video, attached to a source typed with video,
  is disconnected with a log naming both types.

## 11. `output.url`

### 11.1 States

| State                | Carries                                      | Frames  |
| -------------------- | -------------------------------------------- | ------- |
| `Stopped`            | nothing                                      | —       |
| `Connecting a`       | attempt `a` and its abort flag               | dropped |
| `Connected (a, enc)` | the encoder instance `enc`, writing to `url` | encoded |
| `Waiting t`          | the retry time `t`                           | dropped |

The output also keeps its **current metadata**: the last metadata it
received, whatever the state.

Transitions are compare-and-set as in §2.1, with the same ownership rule:
the attempt owns the encoder it built until it moves the state into
`Connected`; whoever moves the state out of `Connected` ends the encoder.

Frames that arrive while the output is not connected are dropped. Rationale:
a live output has no place to keep the stream while the URL is unreachable;
holding it grows memory without bound and delivers stale data. When the
output connects after dropping, it logs at warning level the stream time it
dropped.

Times are read from a monotonic clock, so a wall-clock jump neither delays
nor hastens a retry.

### 11.2 Start, encode, stop

- **Start** (`Stopped` → `Connecting a`): schedule attempt `a`. The attempt
  runs on a scheduler task, never on the streaming thread. It builds the
  `%ffmpeg` encoder instance with the output id and the current metadata;
  building it opens the URL with the bindings' `open_output` and the
  interrupt function "`a`'s abort flag or `shutdown`" ([encoder.md §3](encoder.md#3-building-an-instance)).
  Then `Connecting a` → `Connected (a, enc)`. When that transition fails (a
  stop won), the attempt stops `enc`, which has received no data and writes
  nothing ([encoder.md §7](encoder.md#7-stop)).
- **Encode a frame**: in `Connected`, pass the frame to `enc` on the
  streaming thread (a frame of length 0 is skipped); the encoder writes to the
  URL itself. In `Waiting t` with now ≥ `t`: `Waiting t` → `Connecting a`,
  then drop the frame. In any other state, drop the frame.
- **Metadata**: in `Connected`, pass it to `enc`'s metadata insertion; a
  failure there is handled as an encode failure (§11.3). In every state it
  becomes the current metadata.
- **Stop** (any state → `Stopped`): in `Connected`, stop `enc`: it drains
  every stage and writes the trailer ([draining.md](draining.md),
  [encoder.md §7](encoder.md#7-stop)). In `Connecting a`, set `a`'s abort
  flag. In `Waiting`, nothing more.
- **Reset**: nothing.

The script `on_start` and `on_stop` callbacks are the generic output ones:
they run once per start and stop of the output, not per connection.

### 11.3 Errors

A failure of the attempt (§11.2 Start) or of an encode or metadata call in
`Connected`:

1. When in `Connected`: move the state out of it, then release `enc` without
   draining: a stage in error is released as it is
   ([draining.md §5](draining.md#5-discarded-on-purpose)). When in
   `Connecting a`: the attempt releases what it built.
2. Classify the failure ([faults.md §2](faults.md#2-classes)):
   - **Configuration error**: the failure follows from the format value, the
     fixed `url` and the host alone, so no later attempt can succeed: an
     option no consumer recognised, a codec or muxer that does not exist, a
     stream parameter the codec or the muxer rejects for the configured
     format. Wherever the encoder can know it before streaming, it is raised
     at evaluation or start-up ([encoder.md §4](encoder.md#4-option-handling)).
     When it shows only at the open or at the first frame, it is a hard
     error then: it is raised under the clock's source error rule
     ([clock.md §11](../../../clock/spec/clock.md#11-failure)) and the script
     fails, naming the output and the cause.
   - **Probably permanent** (output fault): a stream parameter the codec or
     the muxer rejects because of the source's data, and an HTTP 4xx answer
     to the open. The first depends on the data the source delivers, which
     changes; the second on the server.
   - **Transient** (output fault): every other failure, the URL open's
     network errors, every write and a call past `timeout` included.
     An output fault restarts or stops the output under its policy and reports
     through `on_error`; the clock and the other outputs go on.
3. Probably permanent: → `Waiting t`, with `t`
   after the back-off delay of §2.3, `restart_delay` taking the place of
   `poll_delay`. Transient: → `Waiting (now + restart_delay)`. Log at
   important level the delay (example: `Will try again in 2.00 seconds.`).
4. With `restart_delay = null`, the script explicitly opts out of restarts
   (the default is `2.0`): instead of waiting, the output stops itself
   (`on_stop` runs) and stays stopped until its `start()` is called. The
   failure stays with the output: the clock and its other outputs keep
   running.
5. Log at error level with the backtrace (example:
   `Error while connecting: <error>`), and run `on_error` with a runtime error of kind `"output"`.

Data faults are contained before they reach this section. A packet or frame
the encoder cannot write is dropped with a warning by the encoder
([encoder.md](encoder.md), [timestamps.md](timestamps.md)); only a failure of
the connection or of the encoder as a whole reaches `output.url`. An
incompatible copied source ([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources))
is a transient failure here: the next connection is a new stream to the
server, a clean restart whose new encoder takes the current source's
parameters.

Each
connection builds a new encoder instance: a new connection to the server, its
container and timestamps starting fresh ([timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what)).

**Conformance.**

- A metadata update that triggers the failure runs `on_error` and waits
  `restart_delay`, exactly as a failing frame does.
- An unknown codec option fails the script, at evaluation when the encoder
  can know it, else at the open.
- With `restart_delay=null`, a failing output stops; another output on the
  same clock keeps streaming.
- Stopping the output while the open is pending returns at once and leaves
  no open connection.
- A clean stop of a connected output writes the container's trailer.

### 11.4 Clock

`self_sync=true` declares the output's generic sync source
([pacing.md §2](../../../clock/spec/pacing.md#2-sync-source)): it takes the
clock's single sync source slot and the clock keeps pacing.
`self_sync=false` declares none.

## 12. Other outputs special-casing `%ffmpeg`

`output.icecast` and `output.harbor` derive their stream MIME type and
encoder from a `%ffmpeg` format through one shared step:

- MIME of a `%ffmpeg` format: its `mime_type` when set, else the MIME of its
  container format name in Liquidsoap's container table (e.g. `adts` →
  `audio/aac`, `ac3` → `audio/x-ac3`; this table differs from
  [encoder-format.md §12](encoder-format.md#12-file-extension-and-mime-type)), else none. With none,
  the user must pass `format=`, or the output fails at evaluation (example
  message: `No format (mime) found, please specify one.`).
- When the `%ffmpeg` format name is `mp3`, the output's encoder is built from
  a copy of the format with the option `id3v2_version` set to int `0` and
  `write_xing` to int `0`, each unless the user set it. Reason: a listener
  joining mid-stream must not receive an ID3v2 header or a Xing frame in the
  middle of the stream. The script's format value is not modified: an
  output never mutates a format value it is given, since the same value can
  feed other outputs.

`output.icecast` stream info for a `%ffmpeg` format: bitrate is the encoder's
reported bitrate divided by 1000 (when known); samplerate and channels come
from the `audio` field when it is an encoded stream, internal or raw; none
for a copied stream. Quality is none.

**Conformance.**

- Printing an mp3 `%ffmpeg` format after creating an `output.harbor` with it
  shows the format unchanged; an `output.file` with the same format writes
  an ID3v2 header and a Xing frame as the format asks.

## 13. Timestamps across reconnects

- `input.ffmpeg`, `input.http`: each connection builds a new decoder, which
  allocates a new source id for its content
  ([content.md §5](content.md#5-stream_idx-and-time_base)). A reconnect is
  therefore a source switch downstream, handled like any other
  ([encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state),
  [timestamps.md](timestamps.md)): timestamps stay strictly increasing per
  output stream across it. The drained output of the ended connection
  precedes the first data of the next one, with one track boundary between
  them (§2.2, §2.5).
- A seek on a connected input is a source switch in the same way (§1.1).
- `input.harbor.dynamic`: one source per client; a reconnecting client is a
  new source with a new decoder.
- `output.url`: each connection is a new encoder instance and a new stream to
  the server (§11.3).

The I/O layer keeps no offset or timestamp state of its own.
