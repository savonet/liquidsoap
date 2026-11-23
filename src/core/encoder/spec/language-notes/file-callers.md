# Part B: the file and pipe callers in OCaml

_Part B of the as-built snapshot: maps the implementation the specification was extracted from. Not normative, and not a design to follow; the normative files govern._

Maps [output-encoded.md](../output-encoded.md),
[output-file.md](../output-file.md) and [output-pipe.md](../output-pipe.md)
onto the code. Not normative; Part A governs. Paths are under `src/core/`.

## Class structure

```
Output.output                     (outputs/output.ml:57)
  └─ ['a] Output.encoded          (outputs/output.ml:310)   cutting, export, order
       └─ Pipe_output.base        (outputs/pipe_output.ml:43)  ['a = Strings.t], encoder slot
            └─ piped_output       (pipe_output.ml:208)      reopen triggers, error retry
                 ├─ file_output        = piped + [out_channel] chan_output + file_output_base
                 ├─ ffmpeg_file_output = piped + [string] chan_output + [string] file_output_base
                 └─ external_output    = piped + [out_channel] chan_output
```

- `chan_output` (pipe_output.ml:307) is a mixin parameterised over the channel
  type: `ffmpeg_file_output` instantiates it with `string` (the filename) and
  makes `output_substring`, `flush`, `close_out` no-ops (pipe_output.ml:483-485).
- `new_file_output` (pipe_output.ml:487) dispatches on `Lang.to_format`:
  `Encoder.Ffmpeg` → ffmpeg variant. `request.dump` calls it directly with
  `Pipe_output.file_proto` defaults (builtins/builtins_request.ml:439-453).
- `base` passes `~output_kind:"output.file"` to `Output.encoded`
  (pipe_output.ml:56) for every subclass, `output.external` included. That
  string becomes the `active_operator` name, hence the default id; the later
  `set_id ~force:false name` in the wake-up initializer (output.ml:154) does
  not override it.

## Encoded base (outputs/output.ml:310-360)

- `send_frame` builds `0 :: List.sort compare (metadata positions @
[Frame.position frame])` and walks it with `output_chunks`. The `[i]` case
  asserts `i = Lazy.Mutexed.force Frame.size || not infallible` (output.ml:350).
- `send_metadata` uses `List.find_map` over `Frame.get_all_metadata frame`:
  the first match in list order wins (output.ml:326-336).
- Export: `Frame.Metadata.Export.from_metadata ~cover:export_cover_metadata`
  (stream/metadata_base.ml:51); lists in stream/metadata_settings.ml:38-64
  (`settings.encoder.metadata.cover` / `.export`). The comparison lower-cases
  the key only.
- `encoding_order` is a mutable field; `set_encoding_order` is only called by
  outputs/icecast2.ml:480.
- `Output.encoded` has no `insert_metadata`; it was removed in d176deb6a
  ("Core cleanup (#5305)"). The source-level `insert_metadata` is
  source/source.ml:920-923, applied in `get_frame` (source.ml:1068-1090).

## Encoder slot (pipe_output.ml:58-115)

- `encoder : [ `None | `Pending | `Created of Encoder.encoder ]`.
- `get_encoder` (l.70) creates from `current_metadata`, which is declared at
  l.61 and never assigned anywhere in the tree (`git grep current_metadata`).
  The assignment `current_metadata <- Some m` was in `insert_metadata` and was
  deleted by 2ba7ba739 "Clarify reloading logic in piped output (#2924)",
  February 2023. Both the factory's initial metadata and `interpolate`
  (l.228-236) read it.
- `close_encoder` (l.87) swallows every exception of `stop` (`with _ ->
Strings.empty`). `cleanup_pipe` (l.244) wraps `base#send self#close_encoder`
  in `try … with _ -> ()`.
- `reset` is overridden to `()` (l.96; the string dates back at least to 648380d79), so the generic
  `Start_stop.base#reset` (stop then start, source/start_stop.ml) never runs.
- `encode` (l.98) checks `0 < Frame.position frame`; `encode_metadata`
  (l.108) checks `Frame.Metadata.Export.is_empty`.

## Reopen and error retry (pipe_output.ml:208-300)

- `need_reopen` is an `Atomic.t`, set by the script method `reopen`
  (`pipe_meth`, l.192-205).
- `open_date` is a plain mutable float; it means "last open time" while open
  and "retry not before" while closed after an error.
- `output` override (l.268): guard `self#is_open || open_date <= now`, then
  `try base#output with exn -> (try cleanup_pipe with _ -> ()); reopen_on_error`.
  The guard is why the source is not pulled during the delay (regression
  #2437, `tests/regression/output_reopen_on_error_delay.liq`).
- `reopen_time_on_error` (l.135) is also used by the HLS output
  (outputs/hls/hls_operator.ml:931).
- `encode` override (l.278): the `match` on `self#is_open`; the third case's
  guard is `open_date +. reopen_delay () <= now && reopen_when ()`, so OCaml's
  left-to-right `&&` gives the "delay first, then ask" order.
- `encode_metadata` override (l.290) calls `self#reopen` unconditionally when
  the callback says so; `reopen` (l.255) logs, `cleanup_pipe`, `prepare_pipe`,
  then runs `on_reopen` callbacks. `prepare_pipe` (l.238) does not run them.
- Default callbacks are compiled script functions (`Lang.eval … "fun (_) ->
null"`, l.117-155).

## Files (pipe_output.ml:338-485)

- `filename` (l.346): getter → `Lang_string.home_unrelate`
  (lang/prelude/lang_string.ml:418) → `self#interpolate ~subst` where `subst`
  replaces `/` by `-`. `Utils.interpolate` (utils/utils.ml:116) maps
  `Not_found` to `""` and logs and returns the input on any other exception.
- `prepare_filename` (l.360): flags `Open_wronly :: Open_creat :: (Open_append
| Open_trunc)`; `Utils.ensure_dir ~perm:dir_perm` (utils/utils.ml:285-296:
  recursive `mkdir`, `ENOTDIR` on a non-directory component, check-then-create
  so concurrent creation can raise `EEXIST`); sets `current_filename` before
  the open.
- `open_chan` / `close_chan` (l.377-399): `Sys_error | Unix_error` are turned
  into `Lang.raise_as_runtime ~kind:"system"`. `close_chan` runs `on_close`
  before `Atomic.set current_filename None`.
- `file_output` (l.401): `open_out_gen` + `set_binary_mode_out`; Stdlib
  channel buffer is 65536 bytes.
- `ffmpeg_file_output` (l.451): `encoder_factory` builds `{ format with output
= `Url ("file:" ^ filename); opts = set opts "truncate" (`Int 0|1) }` and
  calls `Encoder.get_factory (Encoder.Ffmpeg format)`; `open_out_gen` is
  `Unix.close (Unix.openfile filename [O_WRONLY; O_CREAT] perm)`.
- FFmpeg side of the append protocol: `appended_path` and `open_container`
  in optionals/ffmpeg/lib/encoder/instance.ml:94-138 (open with `O_WRONLY;
O_APPEND; O_CREAT` and mode `0o644`, `truncate` removed from the options).

## External (pipe_output.ml:503-543)

- `Unix.open_process_out` (runs `/bin/sh -c`), `Unix.close_process_out`
  (closes the channel then `waitpid`), `Sys_error` swallowed.
- `self_sync`: `(`Static, self#source_sync self_sync)`;
`source_sync false = None` (source/source.ml:263).
- Liquidsoap ignores and blocks `SIGPIPE` (bin/runtime/main.ml:715-716), so a dead reader shows as
  `Sys_error "Broken pipe"` from `output_substring`/`flush`, wrapped by
  `write_pipe` (l.317) as a `system` runtime error.

## `pipe` operator (operators/pipe.ml)

- WAV header: `Wav_aiff.wav_header ~channels ~sample_rate:samplerate ?len
~sample_size:16` (pipe.ml:69-72, media/wav_aiff.ml:257). `samplerate` is a
  mutable field initialised to `Frame.audio_of_seconds 1.` (pipe.ml:63) and
  overwritten in `on_stdout` from the process's header (pipe.ml:86).
  `int_string` masks the top byte with `0x7f` (wav_aiff.ml:254), so a
  `data_length` ≥ 2^31 is truncated.
- `get_to_write` (pipe.ml:140): `slen_of_len len = 2 * len * channels`
  applied to a metadata position in main ticks (l.160), then `track_mark =
Some pos` compares the main-tick track mark with that byte offset (l.163).
- `on_stdin` (l.175): `wlen = min Utils.buflen len`; `Utils.buflen` is
  `LIQ_BUFLEN` or 1024 (utils/utils.ml:104).
- `Process_handler.run ~priority:`Threaded`(l.244);`mutex`guards`replay_pending`, `next_stop`, `samplesize`, `samplerate`, `converter`.

## Hooks (lang/hooks_implementations.ml:100-123)

- `Hooks.make_encoder` := `Lang_encoder.make_encoder` (encoder/lang_encoder.ml:65):
  `find_encoder e p`, then `Encoder.get_factory` as an existence check;
  `Not_found` → `raise_error ~pos "unsupported format"`.
- `Hooks.has_encoder`: `Encoder.get_factory (Lang_encoder.V.of_value fmt)`,
  `with _ -> false`.
- Consumers: lang/runtime/evaluation.ml:195 (encoder terms),
  lang/reducer/term_reducer.ml:63-69 (`%ifencoder`, built with `(name, [])`).
