# `%ffmpeg` at the seam: OCaml and binding notes (Part B)

_Part B of the as-built snapshot: maps the implementation the specification was extracted from. Not normative, and not a design to follow; the normative files govern._

Maps [ffmpeg.md](../ffmpeg.md) onto the code at `02fa52985`. Paths are from
the repository root. Not normative.

## Modules

| Concern                                        | Where                                                                               |
| ---------------------------------------------- | ----------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| Format record, `output : [ `Stream             | `Url of string ]`                                                                   | `src/core/optionals/ffmpeg/formats/encoder_format.ml:74-82` |
| Parser: always `output = `Stream`              | `src/core/optionals/ffmpeg/lib/encoder/format_parser.ml:333-377`                    |
| Instance, containers, restarts, both plug-ins  | `src/core/optionals/ffmpeg/lib/encoder/instance.ml`                                 |
| HLS split bookkeeping (shares the byte buffer) | `src/core/optionals/ffmpeg/lib/encoder/hls_split.ml`                                |
| Per-packet hook origin                         | `encoded_stream.ml:25-30` (`report`), `copy_encoder.ml:220-235` (`write_packet`)    |
| Audio re-framing (no I/O; pts only)            | `audio_writer.ml`                                                                   |
| `output.file` %ffmpeg variant                  | `src/core/outputs/pipe_output.ml:449-491` (`ffmpeg_file_output`, `new_file_output`) |
| `output.url`                                   | `src/core/optionals/ffmpeg/lib/io/url_output.ml:71-87`                              |
| mp3 rewrite for harbor/icecast                 | `src/core/outputs/icecast_utils.ml:34-45`, used by `encoder_data` (`:103-110`)      |
| SRT factory (no rewrite)                       | `src/core/optionals/srt/output_operator.ml:181-186`, header to peers `:297-313`     |
| HLS probe instance                             | `src/core/outputs/hls/hls_operator.ml:53-89`                                        |
| `copy_encoder` format value                    | `src/core/optionals/ffmpeg/lib/io/harbor_dynamic.ml:126-163`                        |
| Bindings                                       | `src/modules/synced/ffmpeg/av/av.ml:395-416`, `av_stubs.c:1451-1515`                |

## Instance record (`instance.ml`)

- `type container` (`:30-38`): `output : Avutil.output Avutil.container`,
  `appended : Unix.file_descr option` (the appending descriptor), `muxer`
  (the guessed muxer's name, `""` when none), the copy group, the tracks,
  `streams_created`, `wait_start`.
- `type t` (`:57-71`): `buffer : Buffer.t` (the byte buffer), `header :
[ `Pending of int | `Captured of Strings.t ]`, `link_start` and
  `returned_link_start` (restart offsets before and after a hand-over),
  `interrupt : (unit -> bool) option`.
- The packet hook needs the record before it exists: `make` (`:481-512`)
  closes over `self : t option ref` and fills it after building the record;
  the hook ignores packets while it is `None` (none can occur: streams are
  created at the first `encode`).
- Byte buffer: `Buffer.create 4096` (`:486`), or the HLS split's buffer.
  `take` (`:236-239`) copies it to a string; `take_into` (`:243-253`) hands a
  `blit` over `Buffer.blit` and resets in `Fun.protect ~finally`. `reset`
  (`:230-234`) also moves `link_start` into `returned_link_start` and rebases a
  pending header capture to 0.

## Opening (`open_container`, `:111-222`)

- `appended_path ~append format` (`:97-104`): `String.starts_with
~prefix:"file:"` and `append || List.assoc_opt "truncate" opts = Some (`Int 0)`. Only the exact `` `Int 0 ``value counts:`truncate="0"`(a string) or`truncate=0.` (a float) is not appending; it goes to FFmpeg's file protocol.
- `Opts.of_list` builds the mutable option table the bindings consume;
  `Opts.unused_message [("container", opts)]` (`:169`) reads what is left.
- Branches (`:140-164`):
  - `` `Url _, Some fd ``: `Av.open_output_stream ~opts ~interleaved (fun bytes
offset length -> Unix.write fd bytes offset length) muxer`. No `?seek`.
  - `` `Stream, _ ``: `Av.open_output_stream ~opts ~interleaved write muxer`
    with `write` = `Buffer.add_subbytes`; returns `length`. No `?seek`.
  - `` `Url url, None ``: `Av.open_output ?interrupt ?format:(guess ())
~interleaved ~opts url`.
- `guess ()` = `Av.Format.guess_output_format ?short_name ?filename ?mime`.
  With a `Url`, FFmpeg's `av_guess_format` scores the name, the file
  extension and the MIME, so an unknown `format` still yields the extension's
  muxer (observed, findings of the snapshot); the `muxer` field is
  re-guessed at `:175-177` for the log and chaining tests.
- Release on failure: `close_appended` (`:137`) on an open failure;
  `release_and_raise` (`:178-182`) after; both close the descriptor.
- `close_output` (`:106-109`): `Fun.protect ~finally:(Unix.close fd)` around
  `Av.close`, so the descriptor is closed even when the close raises.

## Bindings used (`av.mli`, `av_stubs.c`)

- One C entry, `ocaml_av_open_output` (`av_stubs.c:1451`), behind three OCaml
  functions; the target variant decides (`av.ml:395-416`):
  - `open_output` → `To_url { url; interrupt }`:
    `avformat_alloc_output_context2(format, url)`, `install_interrupt`
    (`:455-461`, sets `AVFormatContext.interrupt_callback` to a C trampoline
    that takes the runtime lock and calls the OCaml closure), then
    `av_opt_set_dict2(context, opts, AV_OPT_SEARCH_CHILDREN)` (generic and
    muxer private options), then, unless the muxer has `AVFMT_NOFILE`,
    `avio_open2(&pb, url, AVIO_FLAG_WRITE, &interrupt_callback, &opts)` with
    the runtime lock released (protocol options, e.g. the file protocol's
    `truncate`, `blocksize`). `opened_io` remembers who closes `pb`.
  - `open_output_stream` → `To_custom { write; seek }`: fails "the format
    needs no file" for `AVFMT_NOFILE` muxers; `create_custom_io` (`:470-491`):
    `avio_alloc_context(buffer, CUSTOM_IO_BUFFER_SIZE = 32768, write=1,
record, NULL, write_callback, seek ? seek_callback : NULL)`. A NULL seek
    callback makes the I/O non-seekable for FFmpeg. Flag
    `AVFMT_FLAG_CUSTOM_IO`; no interrupt is installed. Options go to
    `av_opt_set_dict2` only, so protocol options are left over.
  - `open_output_format` → `No_file`: unused by the encoder.
- `write_callback`/`locked_write` (`:354-380`): takes the runtime lock, copies
  at most 32768 bytes into a rooted OCaml `bytes`, and calls the closure until
  it consumed the chunk; a closure returning 0 is `AVERROR_EXTERNAL`; an OCaml
  exception from it propagates as the I/O error.
- Release (`release_container`, `:135-160`): `avio_closep(&pb)` only when the
  binding opened it, `avformat_free_context`, then `free_custom_io`.
- `Av.write_header` (`:2041-2056`): `ensure_header` (`avformat_write_header`
  once, state `CONTAINER_STARTED`) then `avio_flush`. This is why the header
  bytes are in the byte buffer when `on_packet` captures them.
- `Av.write_frame ~on_packet` (`av.ml:579-608`): `receive_packet`, then
  `on_packet`, then `write_encoded`; an exception from `on_packet` is held,
  the packet is still written, and the exception re-raised after. A header
  write failure inside the hook is therefore followed by a second header
  attempt in `write_encoded` (`write_owned_packet` → `ensure_header`), whose
  error is the one raised.
- `Av.output_started` reads the container state (`CONTAINER_STARTED`), used by
  `encode_metadata` (`instance.ml:463`).
- `Av.set_output_option ... "mpegts_flags" "+resend_headers"` and `Av.flush`
  in the HLS hook (`:258-261`).
- Thread rule (av.mli): one modifying operation per container at a time,
  else `Error (`Failure "Object in use!")` at once. Calls release the runtime
  lock while FFmpeg blocks.

## Faults (`encoded_stream.ml`, `src/core/utils/faults.ml`)

- `guarded faults ~kind f` (`encoded_stream.ml:75-79`): `Avutil.Error` →
  `item_fault`; success → `Faults.delivered` (counter to 0).
- `Faults.item` (`faults.ml:62-68`): counts to `max_consecutive_faults = 100`,
  then `` `Source_fault `` → `failwith "<kind>: too many consecutive faults"`
  (`encoded_stream.ml:69-73`). Report rate: `fault_log_interval = 10.` s.
- `Instance.encode_frame` (`:339-366`) catches only `Copy_encoder.Incompatible`
  (→ `Clock.Unrecoverable`, `Failed`) and `Avutil.Error` (→ `Failed`); a
  `Failure` passes through with the state unchanged.

## Restarts and chaining

- `chains` (`:379-384`): `muxer = "ogg"` and (`` `Stream `` or a `` `Url `` of
  length > 5 whose first 5 characters are `file:`). `appended_path` uses
  `String.starts_with`; the two tests agree except for the URL `"file:"`
  itself.
- `restart` (`:414-423`): `close_container`, `open_container ~append:true`,
  `header <- Pending (Buffer.length buffer)`, `link_start <- Some (same)`,
  `position <- 0`, state `` `Built ``, `Av.set_output_metadata`.
- `hls_restart` (`:398-411`): no `~append`; `continued_format` (`:387-395`)
  appends `+frag_discont` to a string `movflags` for `F.iso_family`.
- `encode_frames` (`:428-440`): looks for a copy switch only when `chains t ||
t.hls <> None`.

## Callers

- `ffmpeg_file_output` (`pipe_output.ml:451-485`): `open_out_gen _ perm
filename` = `Unix.close (Unix.openfile filename [O_WRONLY; O_CREAT] perm)`
  and returns the name as its "channel"; `output_substring`, `flush`,
  `close_out` are no-ops. `encoder_factory` reads `current_filename` (an
  `Atomic.t`) and rewrites `output` and `truncate` with
  `Encoder_format.set` (replaces in place or appends). It is called from
  `base#get_encoder` (`:67-79`), at the first `encode` of a piece of positive
  length after `create_encoder`.
- `url_output#build` (`url_output.ml:76-87`): `Encoder.protect
(Instance.create ~interrupt:(self#interrupt attempt) …)`, so the
  directly-built instance is serialised like registry ones. `interrupt`
  (`:71-74`): abort flag, or shutdown unless closing, or
  `Blocking_call.exceeded attempt.write ~timeout`.
- `Icecast_utils.streaming_format` (`icecast_utils.ml:34-45`): only for
  `format = Some "mp3"`; adds `id3v2_version` and `write_xing` as `` `Int 0 ``
  unless present. Introduced 2023-11-29 (`39a3914a7`, "Add override in
  icecast utils to disable all id3v2/xing additional frames when encoding in
  mp3 with the ffmpeg encoder").

## History

- Writing files through FFmpeg's own I/O (`ffmpeg_file_output`,
  `appended_path`, the URL interrupt) arrived with the FFmpeg-layer rewrite
  `4bf0f1d12` (2026-10-26, #5541). Before it, `output.url` used
  `Encoder.with_url_output` and pipe outputs wrote the returned bytes.
- `+frag_discont` and `+resend_headers` arrived with the HLS rewrite
  `86253d4da` (2026-11-08, #5613).
- No version of `instance.ml` since the rewrite passes `?seek` to
  `open_output_stream`.
