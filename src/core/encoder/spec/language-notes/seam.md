# Part B: the encoder seam in OCaml

_Part B of the as-built snapshot: maps the implementation the specification was extracted from. Not normative, and not a design to follow; the normative files govern._

Maps [encoder.md](../encoder.md) and [formats.md](../formats.md) onto the
code. Paths are relative to the repository root.

## Types

- `src/core/encoder/encoder.mli:25-35` `type format`: the ten kinds. FFmpeg's
  payload type lives in the `liquidsoap_ffmpeg_formats` library so that
  `encoder.ml` does not depend on the FFmpeg bindings.
- `encoder.mli:62` `type output = { length; blit }`: the `encode_into` view.
- `encoder.mli:85-93` `type encoder`: the instance record; bytes are
  `Strings.t` (a rope of `string`s, `src/core/utils/strings.ml`), metadata
  is `Frame.Metadata.Export.t` (a string map filtered by
  `Metadata_base.from_metadata`, `src/core/stream/metadata_base.ml:51`:
  keeps keys in `settings.encoder.metadata.export`, minus
  `settings.encoder.metadata.cover` when `~cover:false`).
- `encoder.mli:95` `type factory = pos:Pos.t option -> string ->
Frame.Metadata.Export.t -> encoder`. Partial application is used:
  `output.srt` stores `factory ~pos` (`src/core/optionals/srt/output_operator.ml:184`).
  The Ogg plug-in does work when applied to `~pos` and the name
  (`src/core/optionals/ogg/ogg_encoder.ml:86-95`: looks up its stream
  encoders), so a partial application to `~pos` alone does nothing.
- `encoder.mli:109-133`: the HLS types (owned by the HLS tree).
- The comment above `type encoder` differs between `encoder.ml:152-160` and
  `encoder.mli:64-84` (the `.mli` documents `bitrate`, `restart_offset`,
  `encode_into`; the `.ml` copy does not).

## Registry

- `Plug.t` (`src/core/utils/plug.ml`): an association list, prepended on
  `register` (duplicate name: `failwith`, `plug.ml:63-64`), reversed on
  enumeration so iteration is in registration order (`plug.ml:80-84`).
  Enumeration before `Plug.mark_complete` (called by `Lifecycle.load`,
  `src/core/utils/lifecycle.ml:39`) raises `Plug.Incomplete`.
- Registration order is the dune link order of the modules; it does not
  matter today because each plug-in matches one constructor (see the table in
  encoder.md §2.1 of the snapshot). Registration sites:
  `wav_encoder.ml:87`, `avi_encoder.ml:124`, `ndi_encoder.ml:43`,
  `external_encoder.ml:173`, `optionals/lame/liquidsoap_lame.ml:128`,
  `optionals/shine/liquidsoap_shine.ml:76`,
  `optionals/fdkaac/liquidsoap_fdkaac.ml:121`,
  `optionals/flac/flac_encoder.ml:77`, `optionals/ogg/ogg_encoder.ml:153`,
  `optionals/ffmpeg/lib/encoder/instance.ml:793`; HLS:
  `hls_packed.ml:40`, `instance.ml:797`.
- `get_factory` (`encoder.ml:243-248`) iterates with `Plug.iter` and escapes
  with a local exception `Found`; the result is `fun ~pos name m -> protect
(factory ~pos name m)`, so `protect` runs once per instance.
- Ogg's stream encoders register into a second, private table
  (`ogg_encoder.ml:31`, `audio_encoders` keyed by name; `theora_encoder` a
  `ref`), filled by `optionals/{vorbis,opus,speex,ogg_flac,theora}`.
  `prepare_encoder` (`ogg_encoder.ml:79-83`) raises the "Could not find any
  ..." error; Theora uses `Option.get !theora_encoder` (`ogg_encoder.ml:89`).

## `protect`

- `encoder.ml:207-240`: one `Mutex.t` per instance; every field wrapped with
  `Mutex.protect` (stdlib, OCaml ≥ 5.1: releases on exception). Not
  re-entrant: calling the same instance from an `encode_into` consumer
  locks the mutex twice from one thread (in OCaml 5 believed to raise
  `Sys_error` rather than hang; to verify, see findings).
- `protect_hls` (`encoder.ml:255-270`) is not exported; `get_hls_factory`
  applies it.
- `output.url` calls `Encoder.protect` on `Instance.create`
  (`src/core/optionals/ffmpeg/lib/io/url_output.ml:85`). The FFmpeg test
  producer (`optionals/ffmpeg/test/copy_producer.ml:438-440`) uses an
  unprotected instance from one thread.

## Language plumbing

- Lexer: `src/lang/parser/lexer.ml:120` (`'%', Plus (var_char | digit | '.' |
'_')`, `var_char` = alphabetic), `%ifencoder`/`%ifnencoder` at
  `lexer.ml:168-169`; grammar `src/lang/parser/parser.mly:231,680-697`.
- `%ifencoder`: `src/lang/reducer/term_reducer.ml:63-69`: calls
  `Hooks.make_encoder ~pos:None (name, [])` then `Hooks.has_encoder`; `with _
-> false`.
- Typing: `src/lang/runtime/typechecking.ml:194-212` checks labelled
  sub-terms, then `Hooks.type_of_encoder`; a `Not_found` there becomes
  `Term.Unsupported_encoder` (error 12, `runtime.ml:205-217`), but the
  implementation (`src/core/encoder/types/encoder_types.ml:115-124`) never
  raises `Not_found`: an unknown name raises `Runtime_error` "unsupported
  format: <name>". Runtime errors with an empty position list are re-raised
  with the term's position (`encoder_types.ml:121-122`); typers that pass
  `~pos:None` therefore still get one, those raising from
  `channels_of_params` carry the value's.
- Typers: `encoder_types.ml:106-212`, a `Hashtbl` keyed by name;
  `ffmpeg` registers at `optionals/ffmpeg/formats/encoder_typing.ml:175`.
  `Audio_format.kind` is used as PCM kind; `Encoder.type_of_format`
  (`encoder.ml:42-78`) uses `Content_audio.kind`.
- Evaluation: `src/lang/runtime/evaluation.ml:183-195` evaluates labelled
  parameter terms and calls `Hooks.make_encoder`, implemented by
  `Lang_encoder.make_encoder` (`src/core/encoder/lang_encoder.ml:65-70`,
  wired at `src/core/lang/hooks_implementations.ml:110`): `List.assoc` on the
  builder list, then `Encoder.get_factory` for existence, `Not_found` from
  either → "unsupported format".
- `has_encoder` (`hooks_implementations.ml:117-122`): `get_factory` under
  `with _ -> false`.
- The value: `Lang_encoder.V` (`lang_encoder.ml:26-38`), a
  `Lang_core.MkCustom` with `compare = Stdlib.compare` and a raising
  `to_json`. Formats hold `Lazy.Mutexed.t` values (closures until forced), so
  structural comparison can raise `Invalid_argument "compare: functional
value"`.
- Evaluation-time type check: `src/core/source/lang_source.ml:1077-1083`
  (`Encoder.type_of_format` of the value, checked against the static type).
- Unused: `Lang_error.Encoder_error` (`src/lang/runtime/lang_error.ml:25`)
  is handled by `runtime.ml:198` (error 8) but never raised.
- Builders: `Lang_encoder.register` (a list, latest first); the shared
  rejection helper `raise_generic_error` (`lang_encoder.ml:48-55`);
  `raise_error` = `Encoder_types.raise_error` (`Runtime_error` kind
  `"encoder"`).
- Script helpers: `src/core/builtins/builtins_sys.ml:56-72`.

## Deferred values

- `Lazy.Mutexed.t` (a lazy value guarded by a mutex). Defaults point at
  `Frame.audio_rate` / `Frame.video_dimensions ()` themselves; forcing them
  also fixes the frame settings. Explicit values are `Lazy.Mutexed.from_val`.
  MP3's check wraps its rate in `Lazy.Mutexed.from_fun`
  (`lang_mp3.ml:30-38`).
