# Part B: `%wav`, `%avi`, `%ndi`, `%external` in OCaml

Maps [wav.md](../wav.md), [avi.md](../avi.md), [ndi.md](../ndi.md) and
[external.md](../external.md) onto the code at 02fa52985. Not normative.

## Two typings of each format

A format is typed twice:

- **Script level**, from the term, before evaluation:
  `src/core/encoder/types/encoder_types.ml`. `wav` is in the
  `pcm_audio_type_of_encoder` list (`channels_of_params`, :65–98, default
  2); `avi` (:149) uses `channels_of_params` too (so it accepts
  `mono`/`stereo`, which `lang_avi.ml` then rejects); `external` (:154)
  adds video only for a literal `` `Bool true `` and raises for any other
  `video=` value, `Bool false` included (the second match arm catches
  everything); `ndi` (:172) folds `%audio`/`%video`/`.none`, ignores other
  items, defaults `(true, true)`, uses `!Audio_format.channels ()`, and
  raises "Invalid %%ndi encoder parameter!" for `(false,false)`.
- **Value level**, after `Lang_<fmt>.make` built the record:
  `Encoder.type_of_format` (`encoder.ml:42–78`). `External` gives video when
  the record's `video` is `Some _`, which `width=`/`height=` set
  (`lang_external_encoder.ml:67–82`) without the script-level type
  knowing.

`Encoder_types.raise_error` takes a plain string, so `"%%"` in messages is
printed doubled (`lang_ndi.ml:38,43`, `encoder_types.ml:189`).

Defaults read frame settings through `Lazy.Mutexed.t`
(`samplerate`, `width`, `height`) so that building a format does not
force `frame.*` settings (#933); `channels` is a plain `int` because the
type needs it, hence the hard-coded 2.

`Lang_encoder.make_encoder` (`lang_encoder.ml:65`) calls
`Encoder.get_factory` once at evaluation only to check a plug answers;
`lang_external_encoder.ml:103` raises the bare exception
`External_encoder_format.No_process`, which nothing translates.

## Registration and the seam

Each encoder registers on `Encoder.plug` (`wav_encoder.ml:87`,
`avi_encoder.ml:124`, `ndi_encoder.ml:43`, `external_encoder.ml:173`); the
factory is `fun ~pos name metadata -> …`. `%wav` uses `pos` only for the
unreachable sample-size error; `%avi` and `%external` ignore it; `%external`
uses `name` for `Log.make [id]`. `Encoder.get_factory` wraps every instance
in `protect` (one `Mutex.t` per instance, `encoder.ml:207`). None registers
on `Encoder.hls_plug` (only `hls_packed.ml` and ffmpeg do).

Output values are `Strings.t` (rope of strings). `%wav` returns
`Strings.of_list [header; s]` / `Strings.of_string s` with
`Bytes.unsafe_to_string` on a fresh buffer; `%avi` builds video with
`Strings.Mutable` and `Strings.dda header ans` (prepend). `encode_into` is
`None` everywhere.

## `%wav` (`wav_encoder.ml`, `lang_wav.ml`, `wav_format.ml`)

- Header: `Wav_aiff.wav_header` (`src/core/media/wav_aiff.ml:257`).
  `int_string` (:249) masks the top byte with `0x7f000000`, clearing bit 31;
  `short_string` (:241) raises `Invalid_argument` via `char_of_int` above 65535. Placeholders are the literals `"\255\255\255\239"` and
  `"\219\255\255\239"` (since commit c6cfe3a2e, no rationale given).
- Header state: `need_header : bool ref` initialised to `wav.header`;
  `header ()` returns it only when `wav.header && not !need_header`
  (`wav_encoder.ml:76–79`).
- Conversion: `Audio_converter.Samplerate.resample converter ratio b 0 len`
  then `Audio.{U8,S16LE,S24LE,S32LE}.of_audio` from mm.
- Other users of `Wav_aiff.wav_header`: `operators/pipe.ml:69–72`,
  `external_encoder.ml:57`.

## `%avi` (`avi_encoder.ml`, `lang_avi.ml`, `media/avi.ml`)

- `Avi.header` (`avi.ml:63`): `dword_max ()` is `word 0xffff ^ word 0xffff`
  because 0xffffffff is not an OCaml int on 32-bit; `dword` masks bit 31 like
  `int_string`. `Configure.vendor` fills `ISFT`. The frame rate is
  `Frame.video_rate`, not a format parameter.
- `encode_frame` (`avi_encoder.ml:31`) is shared with `%external`
  (`external_encoder.ml:126`). It creates a `Video_converter.scaler ()` per
  call, uses `Video.Canvas.Image.resize ~proportional:true` then
  `render ~transparent:false`, and iterates `vbuf.Content.Video.data`
  (the `(position, image) list` of the frame) into one
  `Strings.Mutable.t`, wrapped by one `Avi.video_chunk_strings` (:87).
  `failwith` on size mismatch (:60).
- `Avi.Read` (`avi.ml:163`) is the reader used by `input.external.avi`
  (`sources/external_input_video.ml:96,156`); it requires `auds` handler 1
  (:254) and maps only `00dc` to video (:292).

## `%ndi` (`ndi_encoder.ml`, `lang_ndi.ml`, `optionals/ndi/liquidsoap_ndi.ml`)

- The encoder record's closures all call `Runtime_error.raise ~message:"The
NDI encoder can only be used with `output.ndi`!" "invalid"`.
- `output.ndi` (`liquidsoap_ndi.ml:159`) matches `Lang.to_format` against
  `NDI n` directly. Library: `Ndi.init ~filename` (ctypes-foreign; dune
  `(optional)`), exceptions `Ndi.Library_not_found`,
  `Ndi.Library_initialized f`.
- Sender: `Ndi.Send.init ~clock_audio ~clock_video ?groups ?name`; the
  `{handler; mutable position : int64}` record lives in
  `val mutable sender`. `Clock.Sync_source.make ~name:"ndi" `Self_paced`is
set/unset with`self#set_sync_source`.
- `timecode_base = Int64.div 10_000_000L main_rate` (:41), integer.
- Audio: `Bigarray.float32` buffer, `Audio.FLTP.of_audio … ~stride:samples`,
  `` `Fltp {data; stride = samples * 4} ``. Video:
  `Video.Canvas.Image.viewport` then `render`, `Bigarray.int8_unsigned`
  buffer, `` `I420 {data; stride = y_stride} ``.

## `%external` (`external_encoder.ml`, `lang_external_encoder.ml`)

- State: `is_metadata_restart`, `is_stop : bool ref`; `buf :
Strings.Mutable.t`; one `bytes` scratch of `Utils.buflen` (1024, or
  `LIQ_BUFLEN`, `utils.ml:105`); `mutex`, `condition`.
- `Process_handler.run ~on_start ~on_stop ~on_stdout ~on_stderr ~log`
  (`utils/process_handler.ml:155`). `on_start` runs synchronously inside
  `run` for the first process (:374–375) and inside the scheduler's
  `spawn` for respawns (:240–244). `log` is `log#important "%s"`.
- `Restart_after_delay` (`Delay of int`) is parsed
  (`lang_external_encoder.ml:92`) but `external_encoder.ml` never matches
  on it; the delayed restart was dropped by ee2e412b0 (2016, "Rewrite
  external operators using factored process_handler").
- `encode` (:118): `Process_handler.on_stdin process (fun push -> Strings.iter
… really_write …)` inside `Mutex.protect mutex`; handler `Finished when
ext.restart_on_crash || !is_metadata_restart -> ()`. Then
  `Strings.Mutable.flush buf` outside the lock-protected block but after
  it, so skipped on exception.
- `stop` (:155): `Mutex.protect mutex (… Process_handler.stop process;
Condition.wait condition mutex; Strings.Mutable.flush buf)`. The
  condition is signalled by `on_stdout` on a zero read while `is_stop`
  (under the mutex) and by the `on_stop` wrapper (:88–90, without the
  mutex).
- Runner details relied on: `stop` writes `'0'` on a control pipe
  (:89); the scheduler maps it to `` `Stop `` → `send_stop` closes stdin
  (:96); a waiter thread per process `waitpid`s and writes `'2'` (:180–192);
  the handler raises `Finished`, `read_remaining_pipes` drains stdout and
  stderr through the callbacks (:250), closes, logs, sets
  `t.process <- None`, then `restart_decision (on_stop status)` (:357):
  negative → `cleanup`; else a `` `Delay d `` task that respawns.
  `on_stdin` raises `Finished` if `process = None` or `stopped` (:390).
  `pusher` is `Unix_utils.write` on the blocking stdin fd.
- SIGPIPE: writes to a dead process surface as `Unix_error (EPIPE, …)`
  ("Broken pipe in write()"), not `Finished`, so the restart handler does
  not catch them.
