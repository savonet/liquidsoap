# Language notes: FFmpeg network and device I/O

OCaml structure behind [io.md](../io.md): modules, classes, concurrency
primitives, and which tests pin which behaviour.

## 1. Files and library

| File                                                            | Content                                                   |
| --------------------------------------------------------------- | --------------------------------------------------------- |
| `src/core/optionals/ffmpeg/io/ffmpeg_io.ml`                     | `input` and `http_input` classes, `register_input`.       |
| `src/core/optionals/ffmpeg/io/ffmpeg_harbor_input.ml`           | `ffmpeg_http_input` class, `input.harbor.dynamic.regexp`. |
| `src/libs/ffmpeg.liq` (lines 198–245)                           | `input.v4l2`, `video.testsrc.ffmpeg`, `input.rtmp`.       |
| `src/libs/hls.liq` (lines 190–200)                              | `input.hls` replacement.                                  |
| `src/libs/http.liq` (lines 1044–1052)                           | `input.harbor.dynamic` over the regexp form.              |
| `src/core/outputs/pipe_output.ml`                               | `url_output` class, `output.url`.                         |
| `src/core/outputs/icecast_utils.ml`                             | `format_of_encoder`, `encoder_overrides` (§12 of io.md).  |
| `src/core/outputs/icecast2.ml` (~line 110)                      | icecast stream info for `Encoder.Ffmpeg`.                 |
| `src/core/encoder/encoder.ml` (`url_output`, `with_url_output`) | gate and URL injection for `output.url`.                  |

Dune library `liquidsoap_ffmpeg_io`, `(wrapped false)`, `(optional)`, depends
on `liquidsoap_ffmpeg_base` and `liquidsoap_ffmpeg_decoder`. Being optional,
it silently disappears when a dependency is missing; the `%ifdef input.ffmpeg`
guards in the stdlib follow.

## 2. `Ffmpeg_io.input`

- Inherits `Start_stop.active_source ~fallible:true`. `start`/`stop`
  (private) are `connect`/`disconnect`.
- `source_status` is one `Atomic.t` of the polymorphic variant
  ``[`Stopped | `Starting | `Polling of bool Atomic.t | `Connected of string * container | `Stopping]``.
  All transitions that can race with the connect task use
  `Atomic.compare_and_set` against the exact variant value read (physical
  equality on the boxed `` `Polling closed ``).
- `container` record: `decoder : Decoder.decoder`, `remaining : unit -> int`,
  `buffer : Decoder.buffer`, `closed : bool Atomic.t`.
- The connect task is a `Scheduler.Async` task with priority `` `Threaded ``;
  its function returns a float: `-1.` sleeps, a positive value re-runs after
  that delay (Duppy `Async` semantics). `connect_task` (an `Atomic.t` option)
  is exchanged to `None` on disconnect so the stopped task no longer retains
  the source (commit 138d0f8d8, "Stop and release active inputs replaced
  through source.dynamic").
- Callbacks are `Callbacks.t` values; `register_*` is used by the script
  callback layer (releasable), `on_*` by OCaml code. The sync-source hook is
  added with `self#on_connect` in the initializer.
- `self_sync` is `self#dynamic_self_sync` (``(`Dynamic, own_sync_source)``),
  updated through `set_sync_source`.
- `generate_frame` catches every exception, including `Not_connected` and
  `Ffmpeg_decoder.End_of_file`.
- `http_input` subclasses it, mutating the shared `opts` `Hashtbl` at class
  construction, overriding `on_connect_metadata_map` and `seek`.
- `register_input` builds both operators from one prototype with an `is_http`
  switch; `parse_args` fills one `Hashtbl` from the three `*_args` lists.
- The metadata filter closure (with its `last_meta` ref for deduplication) is
  built once in the operator's constructor function.

## 3. `Ffmpeg_harbor_input.ffmpeg_http_input`

- Inherits `Harbor_input.http_input_base` (generic harbor input: generator,
  `relay`, `feed`, `Connection` use counting). Overrides `register_decoder`
  to install a `create_decoder` that ignores its input argument and returns
  the pre-built FFmpeg decoder and buffer.
- `ffmpeg_container` and `remaining` are `Atomic.t` options.
- `on_relay` registers `open_container` and, each time, a new
  `on_wake_up (start_feed)`.
- `Harbor_input.Connection` (`users`, `aborted`, a reserved blocking slot)
  guarantees the socket is closed only when unused; `open_container`
  acquires a use for the duration of the open.
- The source value given to the user is built with `Lang.meth` from
  `Harbor_input.meth ()` and `Harbor_input.callbacks ()`; `on_connect_t`
  builds the matching type from the same lists.
- `copy_encoder_of_description` builds an `Encoder.Ffmpeg` record
  (`Ffmpeg_format.t`) directly; `Lang_encoder.L.format` wraps it.
- `sources` is a `Queues.Queue` of `(string * ffmpeg_http_input)`.

## 4. `output.url`

- `url_output` inherits `Pipe_output.base` (itself `Output.encoded`) with
  `write_pipe` a no-op. The encoder slot is
  ``[`None | `Pending | `Created of Encoder.encoder]``.
- `Encoder.with_url_output` replaces `Ffmpeg_format.output` with
  `` `Url url ``; the encoder then calls `Av.open_output ?format ~interleaved
~opts url` (in `ffmpeg_encoder_common.ml`) without `~interrupt`.
- `url_output` overrides `on_start` with its own list (`on_start`
  callbacks registered through that OCaml method run on every connect); the
  script `on_start` callback registers through `register_on_start` into the
  generic `Start_stop` callbacks.
- `restart_time` uses `Unix.gettimeofday`.

## 5. Formats seen in the code

- `rw_timeout` option: int64 microseconds, seen in `http_input`.
- `icy_metadata_headers`: CR/LF-separated `Name: value` lines, the libavformat
  HTTP protocol AVOption, seen in `http_input#on_connect_metadata_map`.
- `Unrecognized options:` rendering by `Ffmpeg_format.string_of_options`
  (`name=value` joined by `,`).
- `video.testsrc.ffmpeg` lavfi source string `pattern=size=WxH:rate=R[:duration=D]`.

## 6. Tests pinning behaviour

- `tests/streams/ffmpeg-errors.liq`: `on_error` of `input.ffmpeg("invalid")`.
- `tests/regression/LS268.liq`, `GH3276.liq`, `GH4840.liq`: `input.http`
  connection and error paths.
- `tests/regression/ffmpeg-copy-input-http.liq`: `input.http` with copy
  content.
- `tests/regression/GH5389.liq`: `input.ffmpeg` usage.
- `tests/regression/GH2850.liq`: `output.url` `on_error`.
- `tests/streams/harbor_dynamic_failed_connect.liq`: failed open on
  `input.harbor.dynamic`.
- `tests/harbor/icecast_server_*.liq`: `icecast.server` over
  `input.harbor.dynamic` and `copy_encoder`.
