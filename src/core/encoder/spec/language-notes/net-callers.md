# Network callers: OCaml notes (Part B)

_Part B of the as-built snapshot: maps the implementation the specification was extracted from. Not normative, and not a design to follow; the normative files govern._

Traces for [output-icecast.md](../output-icecast.md),
[output-harbor.md](../output-harbor.md), [output-srt.md](../output-srt.md),
[output-url.md](../output-url.md), [output-hls.md](../output-hls.md). Lines
are at 02fa52985.

## Shared

- Encoded-output base: `class virtual ['a] encoded` in
  `src/core/outputs/output.ml:310`. `send_frame` (338) cuts at
  `0 :: sorted (metadata positions @ [position])`, skips `start = stop`;
  `send_metadata` (326) takes the first metadata with `start <= pos < stop`
  via `List.find_map`; `encoding_order` defaults to `` `Metadata_first ``,
  set by `set_encoding_order`. `do_output` (229) calls `send_frame` only when
  `Frame.position data > 0`. The type parameter `'a` is what `encode`
  hands to `send`: `Strings.t` (icecast), `unit -> unit` (harbor: the send
  step is a closure built by encode), `unit` (srt, url).
- Streaming-format step and content-type table: `Icecast_utils`
  (`src/core/outputs/icecast_utils.ml`): `streaming_format` (34),
  `Icecast_v` functor (67) with `format_of_encoder` (84) and `encoder_data`
  (101). The functor is instantiated by icecast (`Icecast`, string ↔
  `Cry.content_type`) and by harbor (`Mime`, identity strings,
  `info_of_encoder` = unit) in `src/core/outputs/harbor/operator.ml:1`.
- `Encoder.get_factory` (`src/core/encoder/encoder.ml:243`) wraps each
  instance in `Encoder.protect` (one `Mutex.t` per instance, every field).
  `get_hls_factory` (272) wraps with `protect_hls`.

## output.icecast (`src/core/outputs/icecast2.ml`)

- `class output p` (329); the `Cry.t` connection is created once per output
  (`Cry.create`, 470) and reused across reconnections; the instance is
  `val mutable encoder : Encoder.encoder option` (497).
- Data-first order: `initializer` (477).
- `encode` (535) matches `(Cry.get_status connection, encoder)`;
  `send` (592) compares `Unix.time ()` (whole seconds, as a float) with
  `restart_time`, which `call_on_error` (525) sets from
  `Unix.gettimeofday ()`.
- `icecast_start` (603): `assert (encoder = None)`, factory with
  `Frame.Metadata.Export.empty`, dump opened with `open_out_bin`,
  `data.info enc` (the `info_of_encoder` table, 63–192), `Cry.connection`,
  extra headers copied into `handler.Cry.headers` except `User-Agent`,
  `Cry.connect`, last-metadata replay through `self#encode_metadata` under
  `try … with _ -> ()`.
- `icecast_stop` (666): `ignore ((Option.get encoder).Encoder.stop ())`
  under `try … with _ -> ()` (the `Option.get` raises when there is no
  instance; swallowed), then `Cry.close`.
- `on_error` is a single mutable handler; `register_on_error` (520)
  replaces it and returns a release closure that restores the previous one.
- `blocking_io = true` (501) makes the clock animate it by a thread.
- Cry (`src/modules/cry/cry.ml`): `write_data` (318), `close` (331, chunk
  terminator), `connect_http` (575), `connect_icy` (614), `send` (771), `update_metadata` (754),
  `connect` (port + 1 for ICY), `http_meta_request` /
  `manual_update_metadata` / `update_metadata` (metadata on a new socket),
  `send` (chunk framing with `Printf.sprintf "%X\r\n%s\r\n"`).

## output.harbor (`src/core/outputs/harbor/`)

- Modes record `mode` in `operator.ml:50`; `shared_mode` (70): `encode`
  returns `fun () -> Shared.send t encoded`; `dedicated_mode` (90).
- `shared.ml`: `start` (82) factory under `t.lock`; `encode` (113) reads
  `restart_offset` right after `encode`; `stop` (102) appends `stop ()` with
  its `restart_offset`; `append` (63) publishes the snapshot (`Atomic.t`)
  before `Store.append`; `join` (376) calls `encoder.Encoder.header ()`
  under `t.lock`.
- `dedicated.ml`: `join` (360) creates the instance and calls `header ()`
  outside any output lock (the instance is not yet published);
  `send_one` (218) prefers `encode_into`; `send` (248) batches of
  `batch_size = 32` on `Scheduler.Task` with `` `Blocking `` priority and a
  `Duppy.Condition.wait`; chunks of `chunk_size = 65536`, `spare_chunks = 4`.
- Teardown failure log: `operator.ml:536`.

## output.srt (`src/core/optionals/srt/output_operator.ml`)

- `running_encoder` (292) creates the instance and marks every stream
  `joined`; `distribute` (302) uses `Strings.sub data offset …` for a
  restart, `Strings.append (header ()) data` otherwise.
- `stop_encoder ~deliver` (241); `end_stream` (266) delivers on a clean end
  and stops undelivered when `streams = []`.
- `push` (70) builds payloads in a `Buffer.t`; `peer_backlog_limit`
  (53) = 8 MiB.
- All under `encoder_m` (`Mutex.t`).

## output.url (`src/core/optionals/ffmpeg/lib/io/url_output.ml`)

- `build` (76): `Encoder.protect (Instance.create ~interrupt … format)` with
  `output = `Url url`; no registry.
- `Instance.create` with `` `Url url `` and no append fd opens with
  `Av.open_output ?interrupt` (`lib/encoder/instance.ml:163`): FFmpeg owns
  the I/O. `` `Stream `` writes into the instance's `Buffer.t` (150).
- State `connection : state Atomic.t` (46); `built_metadata` compared with
  `!=` (177).

## HLS

- `build_variant` (`src/core/outputs/hls/hls_operator.ml:53`): stream-face
  `get_factory` for validation (58), `get_hls_factory` (73), probe `init`
  (84) and `hls_stop` (89).
- `begin_run` (785), `stop` (839), `abort_run` (878, no stop),
  `encode` (1028), `apply_pieces` (1005).
- Packed audio: `src/core/encoder/hls_packed.ml`, `make` (7), registration
  (39) building the inner instance with `Encoder.get_factory format ~pos
name Frame.Metadata.Export.empty`.

## Strings

`Strings.t` (`src/core/utils/strings.ml`) is a reversed list of string
views `{string; offset; length}`; `Strings.iter f` calls
`f string offset length` per view in order. Encoder outputs built through
`Strings.Mutable` are copied (`to_strings` does `Bytes.sub`), so their
views have offset 0; views with a non-zero offset arise from
`Strings.sub` / `Strings.drop` (SRT `distribute`, HLS segmenter).
