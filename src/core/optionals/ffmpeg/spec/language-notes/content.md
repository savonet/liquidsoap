# Language notes: content

OCaml structure behind [content.md](../content.md) and
[stream-description.md](../stream-description.md).

## 1. Libraries

| Library                                                                            | Dir              | Links libav | Holds                                                                                                                                     |
| ---------------------------------------------------------------------------------- | ---------------- | ----------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `liquidsoap_ffmpeg_formats` (`liquidsoap.ffmpeg_formats`), not optional, unwrapped | `formats/`       | no          | `Ffmpeg_content_type` (kind/name layer, implementation guard), `Ffmpeg_encoder_type`                                                      |
| `liquidsoap_ffmpeg_formats_dummy` (`liquidsoap.ffmpeg_formats_dummy`), `-linkall`  | `formats/dummy/` | no          | stand-in params, registers the three kinds                                                                                                |
| `liquidsoap_ffmpeg_base`, optional, unwrapped                                      | `base/`          | yes         | `Ffmpeg_content_base`, `Ffmpeg_copy_content`, `Ffmpeg_raw_content`, `Ffmpeg_frame_checksum`, `Ffmpeg_stream_description`, `Ffmpeg_inline` |
| `liquidsoap_typecheck_dummy` (`src/core/typecheck/dummy`), `-linkall`, no modules  | —                | no          | links `liquidsoap.typecheck` + the stand-in; used by the playground and the language server analysis                                      |

`formats/` is not `(optional)` so a machine without the FFmpeg bindings can
still typecheck scripts that name FFmpeg tracks. The dummy needs `-linkall`
because linking it is what runs the registering initialisers.

## 2. Functors

- `Ffmpeg_content_type.Make (P : Params) (N : Names)`: calls `implement
P.implementation` (the guard, a global `string option ref`), builds a
  `Content_base.FormatSpecs` (`serialize_params _ = ""`, `parse_params ""`
  → default, `content_lang_typ = string_t`), applies
  `Content_base.MkFormatBase`, and registers `Type.register_type N.kind_name`
  with a `Format_type.kind_handler (kind, Lang.univ_t ())`.
  `Params` = `implementation, default, to_string, parse, merge, compatible`.
  `Names` = `kind` (polymorphic variant `` `Copy `` or `` `Raw ``), `name`
  (content name), `kind_name`.
- Libav side: `Ffmpeg_copy_content.Format = Make (Specs) (Names)`, then
  `Content.MkDataBase (Format.Format) (Data)`; `Ffmpeg_raw_content.Audio` /
  `.Video` = `MkDataBase (X_format.Format) (X_data)` with `include
X_format` on top.
- Stand-in: `Liquidsoap_ffmpeg_formats_dummy.{Copy, Raw_audio, Raw_video}` =
  `Make (Params) (…)` only; no `MkDataBase`, so `format_make_fns` keeps its
  default, which raises `Content_base.Invalid`.
- `implementation` strings: `"libav"` (all three libav kinds) and
  `"the stand-in for libav"`.

`MkDataBase` ends with `include D`, so the exported `copy`, `blit`, `make`,
`length`, `params` of e.g. `Ffmpeg_copy_content` are the run-layer ones
(`Ffmpeg_content_base.*`); the view-layer versions are reached through the
`Content` dispatch tables (`Content.copy`, `Content.sub`, …).

## 3. Types

- `Ffmpeg_content_base.data` / `content` are records with mutable `params`
  and `chunks`; `blit` mutates `dst` in place.
- `stream_idx` is `Int64.t`, counter a global `ref 0L`, not atomic.
- Copy params type: `codec_params option` where `codec_params` is a
  polymorphic variant over `Avcodec.params` of each media.
- Raw param records use `option` per field; formats live in a
  `Unifier.t`, which is what makes merges visible to every holder.
- Formats are dispatched by an integer `_type` id into fixed arrays of size
  `max_contents = 16` in `Content_base`.
- `Content_base.merge` wraps any exception of the specs merge into
  `Incompatible_format`; at type level `Format_type.FormatSpecs.subtype` is
  `Content_base.merge`.

## 4. Formats seen in the code

- Dump format string `"<kind>:<params>"` from `Content_base.serialize_format`,
  with `""` params for all FFmpeg kinds. Seen in the code at
  `Content_base.serialize_format` / `parse_format` and in
  `tests/core/format_dump.expected` (`ffmpeg.copy:`, `ffmpeg.audio.raw:`,
  `ffmpeg.video.raw:`).
- The dump is written by the jsoo-safe environment stripper
  (`src/lang/types/jsoo_safe_env.ml`, `handler.serialize`); a reader without
  the kind keeps an opaque custom that prints its name.
- Script cache: Marshal of the typed term; carries the `Unifier` cells with
  libav params as values (cache round-trip of
  `ffmpeg.audio.raw(sample_rate=48000)` checked).

## 5. Logging and settings

- `Ffmpeg_content_base.conf_ffmpeg_content` = `settings.ffmpeg.content`
  (void), child of `Ffmpeg_utils.conf_ffmpeg`;
  `Ffmpeg_copy_content.conf_ffmpeg_copy_relaxed` is a `Dtools.Conf.bool`
  read on every `compatible` call (no caching).
- `Ffmpeg_stream_description` logger: `Log.make ["decoder"; "ffmpeg";
"description"]`; failures logged through `Utils.log_exception` with the
  raw backtrace.

## 6. Stream description

- `container` folds with `List.fold_left` prepending, so the per-media lists
  are reversed; `List.rev` restores order when building `streams`. The field
  number is `List.length streams` of the accumulator, i.e. count of
  successful streams so far.
- Field names come from `Frame.Fields.{audio_n,video_n,subtitles_n,data_n}`,
  which register `<base>_<n+1>` on first use.
- `get_type` passes `codec_params` into
  `Ffmpeg_copy_content.lift_params (Some (`Audio p))`etc.; the raw audio
branch catches with`with \_ when Content.compatible …`.
- `Ffmpeg_utils.set_format_alpha` is the video-internal branch.

## 7. Tests that pin behaviour

- `tests/core/ffmpeg_content_base_test.ml`: make/length/copy/sub/blit/collapse;
  in particular "the original chunk overlaps so it gets entirely removed"
  (blit drops a partially overlapped dst run whole).
- `tests/core/ffmpeg_dummy_content_test.ml`: stand-in kinds.
- `tests/core/ffmpeg_stream_description_test.ml` with
  `all_media_file_descriptions.json`: description over media fixtures.
- `tests/core/ffmpeg_get_type_test.ml`: `get_type`.
