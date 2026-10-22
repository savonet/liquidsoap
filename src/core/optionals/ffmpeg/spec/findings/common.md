# Findings: side data, converters, shared utilities

## Defects

### Duration chunks drift against timestamps

`base/ffmpeg_utils.ml:481-482`: each chunk length is
`convert_time_base (ts - old_ts)`, truncated, instead of the difference of
the two converted timestamps. The truncation error accumulates: packets 23
units apart in `1/1000` give 1014 ticks at 44100 Hz instead of 1014.3, about
0.03 %, roughly one second per hour of content. Content length then lags the
timestamps written into the same packets (`convert_ts`), and any consumer
that compares the two sees growing drift. Read only.

### `convert_time_base` truncates and can overflow

`base/ffmpeg_utils.ml:268-272`: `(t × num) / den` truncates toward zero
(a negative pts rounds up, e.g. `-1` in a fine base becomes `0`) and the
64-bit product wraps silently on overflow. FFmpeg's `av_rescale_q` rounds to
nearest and avoids the overflow. Every stream ↔ main-ticks conversion of the
layer goes through it, so timestamps differ from FFmpeg's own by up to one
unit. Read only.

### `Duration.flush` keeps its pending list

`base/ffmpeg_utils.ml:492-503`: `flush` returns the pending items but does
not clear them. A `push` after `flush` that closes a chunk returns those
items again. Current callers flush at end of stream only, so this is latent.
Read only.

### Hardware frame context with an unnamed pixel format raises `Not_found`

`base/ffmpeg_utils.ml:374-382`: the frame-context arm is taken when the
config's pixel format is `` `None `` (format fits), then logs
`(descriptor pixel_format).name`, which raises `Not_found` for `` `None ``
before the context is created; and `Option.get hwaccel_pixel_format` at line
394 raises when no hardware pixel format is set. Read only (whether FFmpeg
ever reports such a config is unchecked).

### Internal hardware configs shadow device and frame methods

`base/ffmpeg_utils.ml:300-339`: a config whose methods contain `Internal`
always matches one of arms 1-4, so its `Hw_device_ctx`/`Hw_frames_ctx`
methods are never tried for that config. With `hwaccel="device"` or
`"frame"` on a codec whose only config is internal-plus-device, the search
ends with "No suitable hardware acceleration method". Read only.

### `ffmpeg` samplerate converter loses samples

`base/ffmpeg_audio_converter.ml:31-38`: the resampler is never flushed, and
it is replaced (dropping buffered samples) whenever the integer output rate
changes. `time_warp`-style varying ratios rebuild it often, each rebuild a
discontinuity. The output rate is `int_of_float (in × ratio)`, so ratios are
quantised to 1 Hz. Read only.

### `ffmpeg` video converter ignores the scaling settings

`base/ffmpeg_video_converter.ml:83-85`: flags are hard-coded to
`[Bilinear; Print_info]`, single-threaded, while
`settings.ffmpeg.scaling_algorithm` (default bicubic) and
`settings.ffmpeg.scaling_threads` apply to every other scaler of the layer.
Read only.

## Asymmetries

### Rotation direction differs between builtins

`track.ffmpeg.side_data` / `track.ffmpeg.raw.side_data` count
counter-clockwise; `ffmpeg.autorotate.on_undecided` reports clockwise. A file
given `-display_rotation 33` reads `rotation()` ≈ 33 and asks `on_undecided`
with `rotation=327.0`. Checked.

### Copy `remove` leaves packet side data

`filter/builtins_ffmpeg_side_data.ml:64-88`: on copy tracks `remove` edits
the codec parameters only; per-packet entries of the same kind pass. On raw
tracks `remove` drops every entry of the kind from every frame, global or
not. Read only.

### Copy rotation getter read twice per frame

`filter/builtins_ffmpeg_side_data.ml:127` and `:38`: the copy operator reads
the `rotation` getter once to key its cache and again inside the edit, so a
getter whose value changes between the two reads caches a result under the
wrong key. The raw operator snapshots it once. Read only.

### `YUVJ420` maps to limited range, `YUVJ422`/`YUVJ444` to full range

`base/ffmpeg_video_converter.ml:58-62`: `YUVJ420 → yuv420p` but
`YUVJ422 → yuvj422p`, `YUVJ444 → yuvj444p`. Read only.

### Inline encoders drop global side data

`encoder/ffmpeg_internal_encoder.ml:383-399` gives the raw frames' global
side data to the stream; `encoder/builtins_ffmpeg_encoder.ml` (inline
`ffmpeg.encode.video`) passes none. A rotation set on a raw track survives
`%ffmpeg(%video.raw)` but not `ffmpeg.encode.video`. Read only.

### FFmpeg severity is flattened

`base/ffmpeg_utils.ml:195-197`: every captured message is logged at the
single `settings.ffmpeg.log.level`, whatever FFmpeg's severity: an FFmpeg
error and an FFmpeg info line look the same in the Liquidsoap log.
Checked (an FFmpeg warning appears as `[ffmpeg:3]`).

### The `ffmpeg` samplerate converter has no quality setting

`native` and `libsamplerate` each have `settings.audio.converter.samplerate.<name>.quality`;
`ffmpeg` has none and its plug description is empty. Read only.

## Gaps

### In-tree side-data document is normative and partly stale

`base/FFMPEG_SIDE_DATA.md` uses MUST/SHOULD, names
`Ffmpeg_avfilter_utils.Display.layout`, which the `.mli` does not export
(only `expected_size` and `pixel_aspect`), and numbers C8 before C7. Checked
(by reading the `.mli`).

### `unpack_image` asserts on strides

`base/ffmpeg_utils.ml:258-266`: a plane count other than 3 or 4, or unequal
chroma strides, is `assert false` / a failed assertion, not an error. The
planes come from swscale output buffers, so this holds today. Read only.

### No settings reach the video converter seam's choice

Only `ffmpeg` registers a video converter; without FFmpeg,
`Video_converter.scaler ()` raises `Not_found` and the operators that call
it at creation (`track.video.tile`, image decoder, AVI encoder) fail with an
unhelpful exception. Read only.

## To verify

### FFmpeg messages before start-up

The log level and callback are set in `Lifecycle.on_start`. Messages FFmpeg
emits while the script is typechecked and evaluated (filter registration,
probing at evaluation time) go to stderr at FFmpeg's default level,
uncaptured and unfiltered by `settings.ffmpeg.log.verbosity`. Read only.

### Exceptions raised by an `on_undecided` handler

A script exception raised by the handler runs inside the binding's
`convert_display`; whether it propagates to the decoder and what the decoder
then does is not established here. Read only.

### `track.ffmpeg.side_data` before parameters

Copy content with `params = None` leaves the stored entries of an earlier
stream in place; `side_data()` may then describe a previous stream. Read
only.
