# Converters plugged in as "ffmpeg"

This file owns the two converters the FFmpeg layer registers into
Liquidsoap's generic media converters: the audio samplerate converter named
`ffmpeg` and the video format converter named `ffmpeg`. It states the
contract of each generic seam only as far as the `ffmpeg` converters rely on
it (selection, call shape, draining, failure). It relies on the bindings'
[swresample.md](../../../../modules/synced/ffmpeg/spec/swresample.md) and
[swscale.md](../../../../modules/synced/ffmpeg/spec/swscale.md). The scalers
and resamplers that decoders and encoders build for themselves are not these
converters; they are described in the decoder and encoder files, with the
settings of [common.md, Scaling](common.md#10-scaling).

## 1. Samplerate converter seam

| Setting                                          | Type       | Default                                 |
| ------------------------------------------------ | ---------- | --------------------------------------- |
| `settings.audio.converter.samplerate.converters` | `[string]` | `["libsamplerate", "ffmpeg", "native"]` |

- A **converter plug** is a function from a channel count to a converter. A
  **converter** has two operations:
  - `convert (ratio, data, offset, length)` returns
    `(data', offset', length')`. `ratio` is output rate / input rate. `data`
    is planar float audio (one array per channel); `offset` and `length`
    count samples per channel;
  - `flush ()` returns every sample the converter holds back, and leaves
    the converter empty.
- `create channels` walks the setting's list in order and takes the first
  name that is registered. `native` is always registered.
- `resample t ratio data offset length`:
  1. raises an invalid-data error when `data` does not have `channels`
     channels. This is a programming error of the seam user, not a fault of
     the data: the user built the converter for its own channel count;
  2. calls the converter, for every ratio, `1.` included. A converter MAY
     copy the range through at ratio `1.` when it holds no samples.
- A seam user drains its converter ([draining.md](draining.md)) at every
  event that ends it: it calls `flush` and passes the samples on, after the
  last converted range, as ordinary output.
- At start-up the seam logs the selected converter at level 3 under
  `audio.converter` (example: `Using samplerate converter: libsamplerate.`).
- Users of this seam are generic: decoders that are not FFmpeg's, the WAV,
  AVI and external encoders, `time_warp` operators, ALSA and JACK I/O. The
  FFmpeg decoders and encoders resample with swresample directly
  ([decoder-internal.md](decoder-internal.md),
  [encoder-internal.md](encoder-internal.md)). With the default list and
  libsamplerate present, the `ffmpeg` converter is not selected.

Reason for calling the converter at ratio `1.`: a converter that has
resampled at another ratio holds samples. Copying newer samples past it
would put them before the held ones.

## 2. The `ffmpeg` samplerate converter

Registered under the name `ffmpeg`, with a description saying it resamples
with FFmpeg's swresample. It has no setting of its own: swresample's default
options apply. Reason: it is the fallback after libsamplerate, and a quality
knob is a matter of the converter a user picks.

Created for `channels`:

1. The channel layout is FFmpeg's default layout for `channels`, used on both
   sides. A channel count below 1 is an error
   ([avutil.md §4.8](../../../../modules/synced/ffmpeg/spec/avutil.md#48-channel-layout)).
2. The input rate `in` is `settings.frame.audio.samplerate`, read when the
   converter is created.
3. No resampler exists yet.

Each `convert (ratio, data, offset, length)`:

1. `out = round(in × ratio)`, to the nearest integer, and at least 1.
2. When no resampler exists, or the current one was built for another `out`,
   a new resampler replaces it: swresample, planar float in and out, the
   layout of creation step 1 on both sides, `in` → `out`, every other option
   at FFmpeg's default
   ([swresample.md](../../../../modules/synced/ffmpeg/spec/swresample.md),
   `create`). A replaced resampler is drained first: its `flush` output
   comes first in the result of this call.
3. Convert the range `offset`, `length` of `data`
   ([swresample.md](../../../../modules/synced/ffmpeg/spec/swresample.md),
   `convert`).
4. Return the drained samples followed by the produced samples, with offset
   `0` and their total length. The length may be 0.

`flush ()` drains the current resampler, when one exists, and returns its
samples ([swresample.md](../../../../modules/synced/ffmpeg/spec/swresample.md),
`flush`). The resampler stays usable.

- Each rebuild starts a fresh filter state, so a ratio that changes on every
  call (`time_warp`) costs a small transient at each change and loses no
  samples. The ratio is honoured to within one part in `in`.
- A failure of the binding during `convert` or `flush` is an item fault
  ([faults.md §4](faults.md#4-item-faults-escalate)): the samples of that
  call and the resampler are dropped, with a warning, and the next call
  builds a fresh resampler. The converter returns the samples it has
  (possibly none), does not raise, and reports the drop to its caller. The
  caller counts it against `max_consecutive_faults`
  ([faults.md §4](faults.md#4-item-faults-escalate)); past the bound, the caller's
  source has a source fault
  ([faults.md §3](faults.md#3-source-faults-reach-the-script)).
- A NaN in the input becomes 0
  ([swresample.md](../../../../modules/synced/ffmpeg/spec/swresample.md)).

Conformance:

- Converting `n` samples at a fixed ratio `r`, then flushing, yields
  `round(n × r)` samples to within the resampler's rounding of one sample.
- A ratio change between two calls loses no samples: the output count over
  both rates, after a flush, matches the sum of the per-rate counts to
  within one sample per rate change.
- A call at ratio `1.` after calls at another ratio returns the held samples
  before the new ones.

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
     does not list them, log at level 3 that the preferred converter cannot
     convert `src` to `dst`.
  2. Try every registered plug in the registry's order and return the first
     that lists both.
  3. Otherwise raise an error naming `src` and `dst`, and saying that no
     registered video converter handles them. When no video converter is
     registered at all, the error says so.
- `scaler ()` finds a converter from `YUVJ420` to `YUVJ420` once, and returns
  a function `src dst` on YUV420 images: when `src` has an alpha plane it
  uses Liquidsoap's native YUV420 scaling; otherwise it calls the converter
  on the generic views of both images.
- At start-up the seam logs at level 3 under `video.converter` whether the
  preferred converter is registered.
- `ffmpeg` is the only video converter registered in the tree. Users of
  `scaler ()`: `track.video.tile`, the generic image decoder, the AVI
  encoder, generic decoders, the external video input. An operator that needs
  a converter and gets the error of step 3 fails at creation with that
  error, at its script position: a missing converter is a configuration
  error ([faults.md §2](faults.md#2-classes)), known before streaming
  starts.

## 4. The `ffmpeg` video converter

Registered under the name `ffmpeg`, with a description saying it converts
image formats with FFmpeg. Its input and output lists are the same:

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

Each Liquidsoap format maps to the FFmpeg pixel format of its colour range.
`YUVJ420` is Liquidsoap's internal frame format
([common.md, Pixel formats](common.md#7-pixel-formats)), `yuv420p`, limited
range like every frame the encoders receive; the `J` in its name does not
mean full range. `YUVJ422` and `YUVJ444` are full range. A conversion between
`YUVJ420` and `YUVJ422` or `YUVJ444` therefore converts the range.

`create ()` returns a converter that owns its scaling context, initially
none. A call `convert src dst`:

1. The key is `((src format, src width, src height), (dst format, dst width, dst height))`,
   with the FFmpeg formats of the table.
2. When the converter holds a context for that key, it uses it. Otherwise it
   creates a swscale context from the source size and format to the
   destination size and format, with the algorithm and threads of
   [common.md, Scaling](common.md#10-scaling), read at that creation, and
   keeps it with its key, replacing the previous one.
3. Planes: an RGB image is one plane (data, stride); a YUV image is three
   planes (Y with its stride, U and V with the shared chroma stride). An
   alpha plane is not passed.
4. It scales all `src height` rows from row 0 into `dst` from row 0
   ([swscale.md](../../../../modules/synced/ffmpeg/spec/swscale.md), `scale`).

- Each converter owns its context. A swscale context is not reentrant, and
  two converters driven by two clocks run at the same time, so a context
  MUST NOT be shared between converters.
- The converter produces each image before it accepts the next and holds no
  image between calls: it has nothing to drain
  ([draining.md §1](draining.md#1-stages)). Replacing its context on a key
  change loses nothing.
- A failure of context creation or scaling raises a conversion error to the
  caller, which catches it: the image is an item fault
  ([faults.md §4](faults.md#4-item-faults-escalate)), dropped with a warning
  and counted against `max_consecutive_faults`
  ([faults.md §4](faults.md#4-item-faults-escalate)). Past the bound, the caller's
  source has a source fault
  ([faults.md §3](faults.md#3-source-faults-reach-the-script)). The error
  never leaves the caller's source.

Conformance:

- With `settings.ffmpeg.scaling_algorithm` set to `"fast_bilinear"`, a
  converter created afterwards scales with that algorithm.
- Two converters used at the same time from two threads produce correct
  images.

## 5. Without FFmpeg

When Liquidsoap is built without the FFmpeg bindings:

- the samplerate list skips `ffmpeg`; with libsamplerate also absent,
  `native` is used;
- no video converter is registered: start-up logs that the preferred video
  converter is missing, and every `scaler ()` call raises the error of §3
  step 3, which says that no video converter is available.

[common.md, Builds without FFmpeg](common.md#12-builds-without-ffmpeg) lists
the rest.

## 6. Faults

The two converters own no source; a seam user's source contains their
faults as [faults.md](faults.md) requires.

| Fault                                            | Class               | Outcome                                                                       |
| ------------------------------------------------ | ------------------- | ----------------------------------------------------------------------------- |
| no video converter for the formats               | configuration error | the operator fails at creation, at its script position                        |
| samplerate binding failure in a call             | item fault          | samples of the call dropped; counted by the caller (`max_consecutive_faults`) |
| video context creation or scaling failure        | item fault          | image dropped by the caller; counted (`max_consecutive_faults`)               |
| `max_consecutive_faults` consecutive item faults | source fault        | the caller's source ends its track (faults.md §3)                             |
