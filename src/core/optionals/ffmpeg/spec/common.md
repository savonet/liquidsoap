# Shared utilities and settings

This file owns what every part of the FFmpeg layer shares: the
`settings.ffmpeg.*` index (each key is described by its owner), the bridge
from FFmpeg's log to Liquidsoap's log, start-up actions, the Liquidsoap time
bases, time-base conversion, the duration helper that turns timestamped
packets or frames into chunks of content, pixel-format choices, YUV420 image
packing, the definition of a format change, the scaling settings, hardware
contexts for encoders, and what exists when Liquidsoap is built without
FFmpeg. It owns one principle for the whole layer: every length or position
derived from timestamps is computed against an absolute origin
([§6.0](#60-absolute-origin)). Every fault this file detects is placed in a
class of [faults.md](faults.md) (§13 lists them), and every helper states
what its caller does with it. The helpers own no source: they report item
and source faults to their caller, which contains them as
[faults.md §3](faults.md#3-source-faults-reach-the-script) and
[§4](faults.md#4-item-faults-escalate) require. Nothing here raises out of a
source while it streams. It relies on the bindings' specs
([avutil.md](../../../../modules/synced/ffmpeg/spec/avutil.md) §4.7, §4.11,
§4.13, §4.18, §7.1 and
[avcodec.md](../../../../modules/synced/ffmpeg/spec/avcodec.md) hardware
configurations) and on Liquidsoap's frame settings (`settings.frame.*`).

## 1. Settings

Every key and default below is part of the compatibility surface.

| Key                                                        | Type     | Default     | Owner                                                                  |
| ---------------------------------------------------------- | -------- | ----------- | ---------------------------------------------------------------------- |
| `settings.ffmpeg.log.verbosity`                            | `string` | `"warning"` | §2                                                                     |
| `settings.ffmpeg.log.level`                                | `int`    | `3`         | §2                                                                     |
| `settings.ffmpeg.log.capture`                              | `bool`   | `true`      | §2                                                                     |
| `settings.ffmpeg.autorotate`                               | `bool`   | `true`      | [side-data.md §3](side-data.md#3-autorotate-on-decoding)               |
| `settings.ffmpeg.format_change.color`                      | `bool`   | `true`      | §9                                                                     |
| `settings.ffmpeg.format_change.pixel_aspect`               | `bool`   | `true`      | §9                                                                     |
| `settings.ffmpeg.scaling_algorithm`                        | `string` | `"bicubic"` | §10                                                                    |
| `settings.ffmpeg.scaling_threads`                          | `int`    | `1`         | §10                                                                    |
| `settings.ffmpeg.filter_max_buffer`                        | `float`  | `10.`       | [filters.md §2](filters.md#2-settings)                                 |
| `settings.ffmpeg.content.copy.relaxed_compatibility_check` | `bool`   | see owner   | [content.md §13](content.md#13-settings)                               |
| `settings.ffmpeg.content.copy.max_keyframe_wait`           | `float`  | `20.`       | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start) |

`settings.ffmpeg`, `settings.ffmpeg.log`, `settings.ffmpeg.format_change`,
`settings.ffmpeg.content` and `settings.ffmpeg.content.copy` are groups with
no value. FFmpeg decoder settings live under `settings.decoder.…`
([decoder.md §2](decoder.md#2-settings)).

A component reads a setting when it is built, unless the key's owner states
otherwise. A value set after that read has no effect on a component already
built.

Conformance:

- Each key, read from a script that does not set it, has the default of the
  table.

## 2. Log bridge

| Key         | Effect                                                                                   |
| ----------- | ---------------------------------------------------------------------------------------- |
| `verbosity` | FFmpeg's process-wide log level: FFmpeg discards messages less severe than it            |
| `capture`   | `true`: FFmpeg messages go to the Liquidsoap log; `false`: FFmpeg prints them to stderr  |
| `level`     | the least severe Liquidsoap level a captured message is logged at, read for each message |

**Set-up.** Messages that FFmpeg emits while a script is typechecked and
evaluated (filter registration, probing at evaluation time) belong in the
Liquidsoap log like any other. The bridge is therefore set up in two steps:

1. When the FFmpeg layer is loaded, the bridge applies the defaults of the
   three keys: FFmpeg's level is `warning` and the log callback is installed
   ([avutil.md §7.1](../../../../modules/synced/ffmpeg/spec/avutil.md#71-log-messages)).
2. At Liquidsoap's start phase, after the script is evaluated, the bridge
   applies the script's values: it sets FFmpeg's level from `verbosity`
   ([avutil.md §4.7](../../../../modules/synced/ffmpeg/spec/avutil.md#47-logging)),
   and it clears the callback when `capture` is `false`.

**Verbosity values.** `"quiet"`, `"panic"`, `"fatal"`, `"error"`,
`"warning"`, `"info"`, `"verbose"`, `"debug"` and `"trace"` map to FFmpeg's
levels of the same names. Any other value is a configuration error
([faults.md §2](faults.md#2-classes)) at start-up, naming the value and the accepted ones.

**Each captured message.** The bridge strips a captured message of leading
and trailing whitespace and logs it under the label `ffmpeg`. Its Liquidsoap
level is the more severe (numerically smaller) of `level` and the level its
FFmpeg severity maps to:

| FFmpeg severity                     | Mapped Liquidsoap level |
| ----------------------------------- | ----------------------- |
| `panic`, `fatal`                    | 1                       |
| `error`                             | 2                       |
| `warning`                           | 3                       |
| `info`, `verbose`, `debug`, `trace` | 4                       |

With the default `level` of 3, an FFmpeg error appears at level 2 and every
other message at level 3. Reason: an FFmpeg error is often the only account
of why a stream failed, and it must stand out from FFmpeg's routine output.
The bindings deliver each message's severity with its text.

- A message is what FFmpeg formatted, its `[name @ 0x…]` prefix included.
  One FFmpeg log call is one Liquidsoap log line, possibly a partial line.
- Delivery is asynchronous, on the bindings' log thread
  ([avutil.md §7.1](../../../../modules/synced/ffmpeg/spec/avutil.md#71-log-messages)).
- Before Liquidsoap's log stops, the bridge clears the callback. FFmpeg goes
  on logging while finalisers free its contexts, and those messages go to
  stderr.

Example (wording not binding): with the defaults, an FFmpeg warning appears
as `[ffmpeg:3] [mp3 @ 0x…] Estimating duration from bitrate, this may be inaccurate`.

Conformance:

- A message FFmpeg emits during script evaluation reaches the Liquidsoap log.
- With the defaults, an FFmpeg error is logged at level 2 and an FFmpeg
  warning at level 3.
- With `capture` false, no FFmpeg message emitted after the start phase
  reaches the Liquidsoap log.

## 3. Start-up actions

- When the FFmpeg layer is loaded, it registers FFmpeg's input and output
  devices ([avdevice.md](../../../../modules/synced/ffmpeg/spec/avdevice.md)).
- At Liquidsoap's init phase, the layer SHOULD log at level 3 under `ffmpeg`
  the versions of libavutil, libavformat, libavcodec, libswresample and
  libswscale, each as `major.minor.micro`. Example:
  `libavutil 61.7.100, libavformat 63.7.100, libavcodec 63.14.100, libswresample 7.3.100, libswscale 10.2.100`.
- An encoder that is not found is a configuration error ([faults.md §2](faults.md#2-classes)),
  raised when the format is evaluated. The error names the requested codec
  and says that the FFmpeg in use was built without it.

## 4. Liquidsoap time bases

| Name                   | Value                               |
| ---------------------- | ----------------------------------- |
| main ticks time base   | `1/main_rate`                       |
| audio sample time base | `1/settings.frame.audio.samplerate` |
| video sample time base | `1/settings.frame.video.framerate`  |

`main_rate` is the least common multiple of the audio rate and the video
rate. When one of the two rates is 0, `main_rate` is the other. Both 0 is a
configuration error. Positions and lengths in Liquidsoap content are in main ticks.

| Audio rate | Video rate | Main rate | Main ticks time base |
| ---------- | ---------- | --------- | -------------------- |
| 44100      | 25         | 44100     | `1/44100`            |
| 44100      | 24         | 88200     | `1/88200`            |

The rates are fixed once the script is parsed. The time bases MUST NOT be
read before that point.

## 5. Time-base conversion

`rescale(t, src, dst)` converts the integer `t`, counted in `src` units, into
`dst` units. It is the only conversion between time bases in the layer:
every conversion between a stream or chunk time base and main ticks uses it,
on the decoding side, on the encoding side, in filters and in the duration
helper. The bindings convert into a muxer's stream time base with the same
rounding
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
so every timestamp the layer computes agrees with FFmpeg's own.

1. A time base is a rational `num/den` with `num > 0` and `den > 0`. A time
   base read from media with a zero or negative component is invalid media:
   the reader that obtains it (stream opening, chunk reception) treats it as
   a source fault ([faults.md §3](faults.md#3-source-faults-reach-the-script)):
   it ends that track, and the output keeps streaming.
   Readers never pass such a time base to `rescale`; `rescale` given one is
   a programming error and raises.
2. The result is the exact rational value
   `t × src.num × dst.den / (src.den × dst.num)`, rounded to the nearest
   integer, halves rounded away from zero. Rounding is symmetric:
   `rescale(−t) = −rescale(t)`.
3. The computation is exact for every `t` and pair of time bases whose
   result fits in a signed 64-bit integer: no intermediate product overflows.
4. A result outside the signed 64-bit range is not an integer: `rescale`
   returns "out of range" as a distinct result, never a wrapped value. The
   caller drops the item that carries the timestamp: an item fault
   ([faults.md §4](faults.md#4-item-faults-escalate)), logged with the value
   and the time bases and counted against `max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate)).

Reason for rounding to nearest: truncation biases every converted value in
one direction, so two conversions of one timestamp along different paths
disagree, and a value converted back and forth moves by a unit each time.

Conformance:

- `rescale(23, 1/1000, 1/44100) = 1014`; `rescale(−1, 1/90000, 1/44100) = 0`;
  `rescale(−2, 1/90000, 1/44100) = −1`.
- `rescale(2^62, 1/1, 1/90000)` is "out of range"; `rescale(2^40, 1/90000, 1/44100)`
  equals the exactly rounded value.
- A stream whose time base is `0/1` ends that input with a warning; the
  output keeps streaming.

## 6. Duration helper

FFmpeg gives no reliable packet or frame duration. The helper takes the step
between consecutive timestamps as the length of the earlier items, and groups
items so that every chunk of content has a non-zero length in main ticks:
files exist with two successive packets of the same DTS. Its users are the
copy and raw decoders, bitstream filters, inline encoders, and filter graph
inputs and outputs. Each user states its mode, `convert_ts`, `offset` and
`start`, and what it does with the chunks.

### 6.0 Absolute origin

Every length or position the layer derives from timestamps MUST be computed
from the absolute timestamp, as the conversion of the timestamp minus the
conversion of an origin. A component MUST NOT accumulate converted or
rounded per-step differences. This rule binds the duration helper below,
decoding-side positions, encoder-side timestamps and filter outputs; their
owners link here.

Reason: a step is rarely a whole number of main ticks. Rounding each step on
its own leaves an error under one tick per step, and that error adds up
without bound and differs per stream. The streams of one source then run
apart, and a consumer that keeps them together buffers the longer one until a
buffer limit fails, hours later. Converting absolute timestamps bounds the
error to one tick, whatever the run length.

### 6.1 State

Created with:

| Parameter    | Meaning                                                                    |
| ------------ | -------------------------------------------------------------------------- |
| `mode`       | `PTS` or `DTS`: which timestamp of an item is read and written             |
| `src`        | the time base of the items' timestamps                                     |
| `convert_ts` | whether the timestamps written back into items are converted to main ticks |
| `offset`     | added to every timestamp read, in `src`, default `0`                       |
| `start`      | the position of the first timestamped item, in main ticks, default `0`     |

`dst` is the main ticks time base (§4). The state holds:

- `origin`: the first timestamp read (offset included), absent until then;
- `last_ts`: the last timestamp read (offset included), absent until then;
- the **group**: the items held, in arrival order, and the group's position
  `group_pos`, absent until an item of the group has a timestamp;
- `last_length`: the length of the last chunk emitted, absent until then;
- whether the helper is ended.

The position of a timestamp `ts` is
`pos(ts) = start + rescale(ts, src, dst) − rescale(origin, src, dst)`.
`pos` is monotonic in `ts`, so a later item never has an earlier position.

### 6.2 `push t item`

Pushing to an ended helper is a programming error and raises.

1. The item has no timestamp: the item is appended to the group. Nothing is
   written back. `last_ts` is unchanged.
2. The item has timestamp `ts`: let `ts = ts + offset`.
   1. When `last_ts` is present and `ts < last_ts`, the helper drops the
      item, logs a warning naming the mode and both timestamps, and leaves
      its state unchanged; the result is "dropped". Equal timestamps are
      accepted. After `max_consecutive_faults` consecutive dropped items, the
      result is "discontinuity" instead (below).
   2. When `origin` is absent, `origin = ts`.
   3. The helper writes back `rescale(ts, src, dst)` when `convert_ts` is
      set, `ts` otherwise.
   4. Let `p = pos(ts)`. When `group_pos` is absent, `group_pos = p` and the
      item is appended to the group. When `p = group_pos`, the item is
      appended. When `p > group_pos`, the helper emits the chunk
      `(p − group_pos, group)`, then the group becomes `[item]` and
      `group_pos = p`.
   5. `last_ts = ts`, and the count of consecutive dropped items is reset.

The result of a push is the emitted chunk, nothing, "dropped" or
"discontinuity". Within a chunk, items
keep their arrival order, and every item is at position 0 of its chunk.
Items whose positions are equal (timestamps less than one main tick apart)
and items without a timestamp travel in one group.

The helper holds the last group until a later position closes it, so its
output lags its input by one group. Reason: the length of a group is known
only once the next position is.

The lengths of the chunks emitted by `push` add up to
`group_pos − start` exactly, where `group_pos` is that of the group held.

**Bounds.** A group that would grow past `max_group_items` items does not
take the item: the result is "discontinuity". Recommended value: 1024. The
bound protects memory against a stream whose timestamps never advance.
Holding a group is a wait on data
([faults.md §5](faults.md#5-waiting)), and `max_group_items` is its bound.

`max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate))
bounds consecutive item faults of one source. The helper counts its
own drops against it. A user that drops items for other reasons (an
out-of-range timestamp, §5; an image that fails to unpack, §8; a converter
failure, [converters.md](converters.md)) counts them against the same bound
and resets the count on the next item it delivers. Past the bound, the run is
a source fault. The first drop of a run is logged as a warning; repeats are
counted and logged with their count every `fault_log_interval`
([faults.md §4](faults.md#4-item-faults-escalate)), which owns both
parameters and their values.

**What the caller does.** "Dropped" is an item fault: the caller delivers
nothing for it and streaming goes on. "Discontinuity" is a source fault
([faults.md §3](faults.md#3-source-faults-reach-the-script)): the input's
timestamps no longer describe a timeline. The caller flushes the helper
(§6.3), passes the chunk on, and ends its track at that point, logged at
error level. Its source then follows faults.md §3: a fallible source is not
ready until it can produce data again, an infallible source that cannot produce content is a hard error and the script fails. No source produces filler.

### 6.3 `flush t`

`flush` drains the helper ([draining.md](draining.md)) and ends it.

1. When the group is empty, `flush` returns nothing.
2. Otherwise `flush` empties the group and returns the chunk
   `(length, group)`. `length` is the first of these that is positive:
   1. `end − group_pos`, where `end` is the largest `pos(ts + duration)` over
      the items of the group that have both a timestamp (offset included)
      and a duration in `src`;
   2. `last_length`;
   3. 1.
3. The helper is ended.

A group with no timestamped item takes `group_pos = start` for this
computation.

Reason for falling back on the previous length: when the final items give no
duration, the previous step is the best estimate of how long they play.

### 6.4 `last_ts t`

- `last_ts`: the last timestamp read, in `src`, offset included.
- `end_position`: on an ended helper, the position that follows the last
  chunk it emitted, in main ticks. A user that replaces a helper mid-stream
  (for instance on a time base change) flushes the old helper and creates the
  new one with `start` set to the old one's `end_position`, so positions
  continue without a gap or an overlap.

### 6.5 Draining and discarding

The helper is a stage in the sense of [draining.md](draining.md). At every
event that ends the helper (end of input, replacement, end of its stream,
output stop or restart), its user first drains the stage that feeds it and
pushes what that stage releases, then calls `flush` and passes the returned
chunk downstream as ordinary output. At a seek, or after an error of the
stage that feeds it ([draining.md §5](draining.md#5-discarded-on-purpose)), the user drops the
helper without flushing it.

### 6.6 Conformance

- Packets of a 10-hour stream with time base `1/1000` and steps of 23 and
  24 ms (AAC at 44.1 kHz), pushed then flushed: the total length differs from
  `rescale(last_ts + last duration, 1/1000, 1/44100) − rescale(first_ts, 1/1000, 1/44100)`
  by at most one tick.
- Two streams of one source (audio as above, 30 fps video, both in `1/1000`):
  after 10 hours the difference of their total lengths equals the difference
  of their timestamp spans to within two ticks.
- Inline AAC at 48 kHz in packets of 1024 samples, main rate 44100: after
  `n` packets the emitted length is within one tick of `n × 940.8`.
- Two items with equal timestamps form one chunk, in arrival order.
- A decreasing timestamp drops that item with a warning and leaves the
  state unchanged; the next increasing timestamp is accepted.
- `max_consecutive_faults` decreasing timestamps in a row give "discontinuity".
- `flush` on an empty helper returns nothing. `flush` returns each held item
  once. `push` after `flush` raises.

## 7. Pixel formats

| Name                                   | Value      |
| -------------------------------------- | ---------- |
| internal frame pixel format            | `yuv420p`  |
| internal frame pixel format with alpha | `yuva420p` |

- **Has alpha**: a pixel format has alpha when its descriptor's flags contain
  `Alpha`
  ([avutil.md §4.11](../../../../modules/synced/ffmpeg/spec/avutil.md#411-pixel-format)).
  A pixel format with no descriptor has no alpha.
- **Internal format for** `f`: `yuva420p` when `f` has alpha, `yuv420p`
  otherwise. Scalers between FFmpeg frames and internal images use it on the
  internal side.
- **Encoder pixel format** for a codec, given an optional configured name
  and `alpha` (default `false`):
  1. a configured name is parsed as a pixel format name. An unknown name is
     an `encoder` error at the script position of the format, naming the
     value. `alpha` is not consulted;
  2. otherwise, when the codec's supported formats contain the preferred
     internal format (`yuva420p` if `alpha`, else `yuv420p`), that one;
  3. otherwise the first supported format without the `Hwaccel` flag, and
     with the `Alpha` flag when `alpha`;
  4. otherwise `yuv420p`, also when the codec lists no format. Hardware
     codecs list hardware formats and accept the regular one.
- **Set format alpha**: given codec parameters and a Liquidsoap video format
  whose alpha is still undetermined, the format's alpha becomes `true` when
  the parameters' pixel format is known and has alpha, `false` otherwise. A
  format whose alpha is already determined is left alone. Decoders use it
  when they describe a stream ([stream-description.md](stream-description.md)).

## 8. Image packing

Liquidsoap's YUV420 image and the plane arrays of the bindings' scalers
convert into each other:

- **Pack** image → planes: `[| (Y, y_stride); (U, uv_stride); (V, uv_stride) |]`,
  plus `(A, y_stride)` as a fourth plane when the image has alpha. The planes
  are the image's own buffers.
- **Unpack** `~width ~height` planes → image:
  - three planes with equal U and V strides: the image is built over the
    buffers, without alpha;
  - four planes with equal U and V strides and an alpha stride equal to the
    Y stride: the image has alpha;
  - any other plane count or stride layout raises an error that names the
    plane count and the strides. The caller catches it, drops that image as
    an item fault ([faults.md §4](faults.md#4-item-faults-escalate)) counted
    against `max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate)), and goes on with the next one.

## 9. Video format comparison

This section defines, for the whole layer, when two frame formats are the
same. Every consumer built for one format (display stages, decoder scalers
and resamplers, filter inputs, frame fitting, encoders of raw frames) uses
this comparison to decide whether to rebuild. What each consumer does on a
change is its owner's.

**Video.** A video frame format is the binding's record
([avutil.md §4.13](../../../../modules/synced/ffmpeg/spec/avutil.md#413-video-frames)):
size, pixel format, pixel aspect, colour space, range, primaries, transfer
characteristic, chroma location.

- Size and pixel format always count.
- `Color` (colour space, range, primaries, transfer characteristic, chroma
  location) counts when `settings.ffmpeg.format_change.color` is true.
- Pixel aspect counts when `settings.ffmpeg.format_change.pixel_aspect` is
  true.
- Both settings are read on every comparison.
- When `Color` does not count, a consumer keeps the colour values of the
  first frame it was built for.

**Audio.** An audio frame format is its sample format, sample rate and
channel layout. All three always count.

A format change of decoded content is not a track boundary
([content.md](content.md)).

**Describing** a format for logs (wording not binding):

- video: `<w>x<h> <pixel format>, pixel aspect <num/den>, colour space <…>, range <…>, primaries <…>, transfer <…>, chroma location <…>`,
  with `unknown` for an unnamed pixel format or an absent pixel aspect;
- audio: `<sample format>, <rate> Hz, <channel layout description>`, with
  `unknown` for an unnamed sample format.

Conformance:

- Two frames that differ only in colour space compare equal with
  `format_change.color` false and differ with it true.
- Two frames that differ in size always differ.

## 10. Scaling

| Key                                 | Accepted values                              |
| ----------------------------------- | -------------------------------------------- |
| `settings.ffmpeg.scaling_algorithm` | `"fast_bilinear"`, `"bilinear"`, `"bicubic"` |
| `settings.ffmpeg.scaling_threads`   | any integer; values below 0 count as 0       |

Every scaler the layer creates reads both keys when it is created: encoder
and inline encoder scalers, decoder scalers, frame fitting and the `ffmpeg`
video converter
([converters.md §4](converters.md#4-the-ffmpeg-video-converter)).

- **Algorithm**: the swscale flag of the same name. An unknown value is a
  configuration error ([faults.md §2](faults.md#2-classes)), raised at start-up, before any scaler
  exists. A value set while streaming is checked when it is set; an unknown
  one is rejected and the previous value stays. Frame fitting passes the
  value as the `flags` option of a `scale` filter
  ([filters.md §16.4](filters.md#164-fit)).
- **Threads**: `max 0 value`. `1` scales on the calling thread; `0` uses one
  thread per core; `n` splits each frame over `n` threads. Reason for the
  default of 1: splitting a frame costs a fan-out and a join on every frame,
  which a stream held to real time pays on every frame and rarely earns back.

## 11. Hardware contexts

Encoders that may use hardware acceleration call this once per video stream
with: `hwaccel` (`none`, `auto`, `internal`, `device`, `frame`), an optional
hardware pixel format `hpf`, an optional device name, options, the target
pixel format `tpf` (§7), the target width and height, and the codec. It
returns an optional hardware context and the pixel format of the stream. The
encoder parameters themselves are
[encoder-internal.md](encoder-internal.md)'s.

1. `hwaccel = none`: return (no context, `tpf`).
2. **Candidates.** For each hardware configuration of the codec, in FFmpeg's
   order
   ([avcodec.md §4.1](../../../../modules/synced/ffmpeg/spec/avcodec.md#41-top-level)),
   with methods `M`, pixel format `p` (possibly unnamed) and device type `d`,
   and for each method `m` of `internal`, `device`, `frame`, in that order,
   with `m ∈ M`: the pair (configuration, `m`) is a candidate when
   `hwaccel = auto` or `hwaccel = m`. Every method of a configuration is a
   candidate of its own: a configuration that lists `internal` and `device`
   gives a `device` candidate too.
3. **Fit.** A candidate fits when its format fits (`p` is unnamed, or `hpf`
   is absent, or `p = hpf`) and:
   - `internal`: no device name is given;
   - `device`: always;
   - `frame`: the hardware pixel format `hw` (`hpf` when given, else `p`) is
     named.
4. **Selection.** The fitting candidates are tried in order:
   - `internal`: return (no context, `hpf` when given, else `p` when named,
     else `tpf`);
   - `device`: create a device context of type `d` with the device name and
     options
     ([avutil.md §4.18](../../../../modules/synced/ffmpeg/spec/avutil.md#418-hardware-contexts));
     return (device context, `hpf` when given, else `p` when named, else
     `tpf`);
   - `frame`: create a device context of type `d`, then a frame context of
     the target size from `tpf` to `hw`; return (frame context, `hw`).

   When creating a context fails, the failure is logged at level 3 naming
   the codec and the method, and the next candidate is tried.

5. **None returned.** When `hwaccel` is not `auto`, the requested method
   cannot be met. This follows from the configuration and the host, so it is
   a configuration error ([faults.md §2](faults.md#2-classes)) whenever it
   shows, at the first build or at a later rebuild: the script fails with an
   `encoder` error naming `hwaccel`, the codec and, when a context creation failed, the last
   failure. With `auto`, return (no context, `tpf`); when the codec lists at
   least one configuration, log at level 3 that the codec encodes in
   software.

A candidate that does not fit, and a configuration whose only method is
`ad_hoc`, SHOULD be logged at level 4, naming the codec and the reason. An
unnamed pixel format is logged as `none`.

Conformance:

- A codec whose only configuration lists `internal` and `device` accepts
  `hwaccel="device"`.
- A configuration with an unnamed pixel format and no `hpf` is not a `frame`
  candidate, and logging it does not fail.
- `hwaccel="auto"` on a codec without hardware configurations returns
  (no context, `tpf`).

## 12. Builds without FFmpeg

- `liquidsoap.build_config.optionals.ffmpeg` is `true` when the FFmpeg
  bindings were found at build time, `false` otherwise. The build summary
  says whether FFmpeg support is present.
- Without them, none of the FFmpeg layer exists: no `ffmpeg.*`,
  `track.ffmpeg.*`, `input.ffmpeg`, `settings.ffmpeg.*`, no FFmpeg decoders
  or encoders, no `ffmpeg` converters
  ([converters.md, Without FFmpeg](converters.md#5-without-ffmpeg)). The
  script library guards its FFmpeg definitions with `%ifdef` on an FFmpeg
  builtin (for instance `%ifdef input.ffmpeg`, or
  `%ifdef ffmpeg.filter.ebur128` in autocue, which then logs that autocue is
  disabled).
- A process that only typechecks scripts (playground, language server
  analysis) registers stand-ins for the FFmpeg content kinds; the kinds are
  [content.md](content.md)'s. Stand-ins and real kinds MUST NOT be registered
  in one process.

## 13. Faults detected here

| Fault                                            | Class                             | Section | Outcome                                                        |
| ------------------------------------------------ | --------------------------------- | ------- | -------------------------------------------------------------- |
| invalid `settings.ffmpeg.log.verbosity`          | configuration error               | §2      | error at start-up                                              |
| invalid `settings.ffmpeg.scaling_algorithm`      | configuration error               | §10     | error at start-up; a later invalid set is rejected             |
| audio and video rates both 0                     | configuration error               | §4      | error at start-up                                              |
| encoder not found                                | configuration error               | §3      | error at evaluation of the format                              |
| unknown configured pixel format                  | configuration error               | §7      | `encoder` error at the format's script position                |
| explicit `hwaccel` that cannot be met            | configuration error               | §11     | the script fails, whenever it shows                            |
| a hardware context creation fails                | none: the next candidate is tried | §11     | logged at level 3                                              |
| time base with a component ≤ 0 in media          | source fault                      | §5      | the reader ends its track                                      |
| timestamp whose conversion is out of range       | item fault                        | §5      | the item is dropped, counted against `max_consecutive_faults`  |
| decreasing timestamp                             | item fault                        | §6.2    | the item is dropped, counted against `max_consecutive_faults`  |
| `max_consecutive_faults` consecutive item faults | source fault                      | §6.2    | the caller ends its track                                      |
| group past `max_group_items`                     | source fault (bounded wait)       | §6.2    | the caller ends its track                                      |
| image that fails to unpack                       | item fault                        | §8      | the image is dropped, counted against `max_consecutive_faults` |

Classes are those of [faults.md §2](faults.md#2-classes). A push to an ended
helper, a time base with a component ≤ 0 given to `rescale`, and stand-ins
registered beside the real kinds are programming errors of the layer, not
faults of the data or the configuration: they raise and are fixed in the
code.
