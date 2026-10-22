# Converters plugged in as "ffmpeg"

This file owns the two converters the FFmpeg layer registers into
Liquidsoap's generic media converters: the audio samplerate converter named
`ffmpeg` and the video format converter named `ffmpeg`. It states the
contract of each generic seam only as far as the ffmpeg implementation relies
on it (selection, call shape, failure). It relies on the bindings' swresample
and swscale specs (`src/modules/synced/ffmpeg/spec/swresample.md`,
`swscale.md`). The scalers that decoders and encoders build for themselves
are not these converters; they are described in the decoder and encoder
files, with the settings of [common.md, Scaling](common.md#10-scaling).

## 1. Samplerate converter seam

| Setting                                          | Type       | Default                                 |
| ------------------------------------------------ | ---------- | --------------------------------------- |
| `settings.audio.converter.samplerate.converters` | `[string]` | `["libsamplerate", "ffmpeg", "native"]` |

- A **converter plug** is a function from a channel count to a converter. A
  **converter** takes `(ratio, data, offset, length)` and returns
  `(data', offset', length')`. `ratio` is output rate / input rate. `data` is
  planar float audio (`float array array`, one array per channel); `offset`
  and `length` count samples per channel.
- `create channels` walks the setting's list in order and takes the first
  name that is registered. `native` is always registered.
- `resample t ratio data offset length`:
  1. raises `Invalid_data` when `data` does not have `channels` channels;
  2. when `ratio = 1.`, returns a copy of the range and does not call the
     converter;
  3. otherwise calls the converter.
- At start-up the seam logs `Using samplerate converter: <name>.` at level 3
  under `audio.converter`.
- Users of this seam are generic: decoders that are not FFmpeg's, the WAV,
  AVI and external encoders, `time_warp` operators, ALSA and JACK I/O. The
  FFmpeg decoders and encoders resample with swresample directly
  ([decoder.md](decoder.md), [encoder-internal.md](encoder-internal.md)).
  With the default list and libsamplerate present, the `ffmpeg` converter is
  not selected (`Using samplerate converter: libsamplerate.`, from the
  implementation).

## 2. The `ffmpeg` samplerate converter

Registered under the name `ffmpeg` with an empty description. It has no
setting of its own.

Created for `channels`:

1. The channel layout is FFmpeg's default layout for `channels`
   (`Channel_layout.get_default`), used on both sides. A channel count below
   1 raises (`avutil.md` §4.8).
2. The input rate `in` is `settings.frame.audio.samplerate`, read when the
   converter is created.
3. No resampler exists yet.

Each call `(ratio, data, offset, length)`:

1. `out = int_of_float (float in *. ratio)`: truncated toward zero.
2. When no resampler exists, or the existing one was built for another
   `out`, build a new one: swresample, planar float in and out, layout of
   step 1 on both sides, `in` → `out`, every other option at FFmpeg's
   default (`swresample.md` §4.4 `create`). The previous resampler is
   dropped with the samples it held.
3. Convert the range `offset`, `length` of `data` (`swresample.md` §4.4
   `convert`).
4. Return the produced samples with offset `0` and their length. The
   length may be 0.

- The resampler is never flushed: the samples it holds back when the
  converter is dropped are lost.
- A call with `ratio = 1.` never reaches the converter (§1), so the
  resampler keeps whatever it held across such calls.
- Failures of the binding (rates not positive, a bad range) raise to the
  caller.
- A NaN in the input becomes 0 (`swresample.md` §4.4).

## 3. Video converter seam

| Setting                              | Type     | Default    |
| ------------------------------------ | -------- | ---------- |
| `settings.video.converter.preferred` | `string` | `"ffmpeg"` |

- A **converter plug** is `(input formats, output formats, create)`, where
  `create ()` returns a converter `src -> dst -> unit` that writes the
  conversion of image `src` into image `dst`. Formats are Liquidsoap's
  generic image pixel formats.
- `find_converter src dst`:
  1. When the preferred plug is registered and lists `src` as an input and
     `dst` as an output, return its `create ()`. When it is registered but
     does not list them, log `Default video converter <name> cannot do <src>-><dst>.`
     at level 3.
  2. Try every registered plug in the registry's order, logging
     `Trying <name> video converter...` at level 4 for each; return the
     first that lists both.
  3. Log `Couldn't find a video converter from format <src> to format <dst>.`
     at level 3 and raise `Not_found`.
- `scaler ()` finds a converter from `YUVJ420` to `YUVJ420` once, and
  returns a function `src dst` on YUV420 images: when `src` has an alpha
  plane it uses Liquidsoap's native YUV420 scaling; otherwise it calls the
  converter on the generic views of both images.
- At start-up the seam logs `Using preferred video converter: ffmpeg.` at
  level 3 under `video.converter` when the preferred plug is registered, and
  `Couldn't find preferred video converter: <name>.` otherwise.
- `ffmpeg` is the only video converter registered in the tree. Users of
  `scaler ()`: `track.video.tile`, the generic image
  decoder, the AVI encoder, generic decoders, the external video input.

## 4. The `ffmpeg` video converter

Registered under the name `ffmpeg`, description `FFmpeg image format converter.`.
Its input and output lists are the same:

| Liquidsoap format | FFmpeg pixel format |
| ----------------- | ------------------- |
| `RGB24`           | `rgb24`             |
| `BGR24`           | `bgr24`             |
| `RGB32`           | `rgba`              |
| `BGR32`           | `bgra`              |
| `RGBA32`          | `rgba`              |
| `YUV422`          | `yuv422p`           |
| `YUV444`          | `yuv444p`           |
| `YUV411`          | `yuv411p`           |
| `YUV410`          | `yuv410p`           |
| `YUVJ420`         | `yuv420p`           |
| `YUVJ422`         | `yuvj422p`          |
| `YUVJ444`         | `yuvj444p`          |

`YUVJ420` maps to Liquidsoap's internal frame pixel format
([common.md, Pixel formats](common.md#7-pixel-formats)), `yuv420p`.

`create ()` returns a converter with its own scaling context, initially none.
A call `convert src dst`:

1. Compute the key `((src format, src width, src height), (dst format, dst width, dst height))`
   with the FFmpeg formats of the table.
2. When the converter holds a context for that key, use it. Otherwise create
   a swscale context with flags `Bilinear` and `Print_info`, from the source
   size and format to the destination size and format, and keep it with its
   key, replacing the previous one.
3. Planes: an RGB image is one plane (data, stride); a YUV image is three
   planes (Y with its stride, U and V with the shared chroma stride). An
   alpha plane is not passed.
4. Scale all `src height` rows from row 0 into `dst` starting at row 0
   (`swscale.md` §4.4 `scale`).

- The scaling flags are fixed: `settings.ffmpeg.scaling_algorithm` and
  `settings.ffmpeg.scaling_threads` do not apply. Scaling runs on the
  calling thread.
- Each converter owns its context. A swscale context is not reentrant, and
  two converters driven by two clocks run at the same time; one shared
  context crashed inside swscale.
- `Print_info` makes swscale log its set-up at FFmpeg level `info`, which
  the default `settings.ffmpeg.log.verbosity` (`warning`) filters out.
- Errors of context creation or scaling raise to the caller.

## 5. Without FFmpeg

When Liquidsoap is built without the FFmpeg bindings:

- the samplerate list skips `ffmpeg`; with libsamplerate also absent,
  `native` is used;
- no video converter is registered: start-up logs
  `Couldn't find preferred video converter: ffmpeg.`, and every `scaler ()`
  call raises `Not_found` after the log lines of §3.

[common.md, Builds without FFmpeg](common.md#12-builds-without-ffmpeg) lists the rest.
