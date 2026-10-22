# Shared utilities and settings

This file owns what every part of the FFmpeg layer shares: the
`settings.ffmpeg.*` tree (index here, each key described by its owner), the
bridge from FFmpeg's log to Liquidsoap's log, start-up actions, the
Liquidsoap time bases, time-base conversion, the duration helper that turns
timestamped packets or frames into chunks of content, pixel-format choices,
YUV420 image packing, video format comparison, hardware contexts for
encoders, and what exists when Liquidsoap is built without FFmpeg. It relies
on the bindings' specs (`src/modules/synced/ffmpeg/spec/`, chiefly
`avutil.md` §4.7, §4.11, §4.13, §4.18, §7.1 and `avcodec.md` hardware
configs) and on Liquidsoap's frame settings (`settings.frame.*`).

## 1. Settings

All keys, defaults from the implementation (`settings.ffmpeg.…()` printed by
a script).

| Key                                                        | Type     | Default     | Owner                                                    |
| ---------------------------------------------------------- | -------- | ----------- | -------------------------------------------------------- |
| `settings.ffmpeg.log.verbosity`                            | `string` | `"warning"` | §2                                                       |
| `settings.ffmpeg.log.level`                                | `int`    | `3`         | §2                                                       |
| `settings.ffmpeg.log.capture`                              | `bool`   | `true`      | §2                                                       |
| `settings.ffmpeg.autorotate`                               | `bool`   | `true`      | [side-data.md §3](side-data.md#3-autorotate-on-decoding) |
| `settings.ffmpeg.format_change.color`                      | `bool`   | `true`      | §9                                                       |
| `settings.ffmpeg.format_change.pixel_aspect`               | `bool`   | `true`      | §9                                                       |
| `settings.ffmpeg.scaling_algorithm`                        | `string` | `"bicubic"` | §10                                                      |
| `settings.ffmpeg.scaling_threads`                          | `int`    | `1`         | §10                                                      |
| `settings.ffmpeg.filter_max_buffer`                        | `float`  | `10.`       | [filters.md §2](filters.md#2-settings)                   |
| `settings.ffmpeg.content.copy.relaxed_compatibility_check` | `bool`   | see owner   | [content.md §13](content.md#13-settings)                 |

`settings.ffmpeg`, `settings.ffmpeg.log`, `settings.ffmpeg.format_change`,
`settings.ffmpeg.content` and `settings.ffmpeg.content.copy` are groups with
no value. FFmpeg decoder settings live under `settings.decoder.…`
([decoder.md §2](decoder.md#2-settings)).

Settings are read where stated in each section: some once at start-up, most
each time the value is needed. A value set after the read has no effect on
what was already built.

## 2. Log bridge

| Key         | Effect                                                                                  |
| ----------- | --------------------------------------------------------------------------------------- |
| `verbosity` | FFmpeg's process-wide log level, set once at start-up                                   |
| `capture`   | `true`: FFmpeg messages go to the Liquidsoap log; `false`: FFmpeg prints them to stderr |
| `level`     | the Liquidsoap log level every captured message is logged at, read for each message     |

At start-up (Liquidsoap's start phase, after the script is evaluated):

1. Map `verbosity`: `"quiet"`, `"panic"`, `"fatal"`, `"error"`,
   `"warning"`, `"info"`, `"verbose"`, `"debug"`, `"trace"` to FFmpeg's
   levels of the same names. Any other value logs
   `Invalid value for "ffmpeg.log.verbosity"!` at level 2 under `ffmpeg` and
   uses `quiet` (from the implementation:
   `[ffmpeg:2] Invalid value for "ffmpeg.log.verbosity"!`).
2. Set FFmpeg's log level ([avutil.md §4.7](../../../../modules/synced/ffmpeg/spec/avutil.md#47-logging)).
3. When `capture` is `true`, install the log callback
   ([avutil.md §7.1](../../../../modules/synced/ffmpeg/spec/avutil.md#71-log-messages)). Each delivered message is
   stripped of leading and trailing whitespace and logged under the label
   `ffmpeg` at the current value of `level`.

- The FFmpeg severity of a message is not kept: with the defaults, an FFmpeg
  warning appears as `[ffmpeg:3] …` (from the implementation). FFmpeg
  filters by `verbosity` before delivery (`avutil.md` §7.1 N2).
- A message is what FFmpeg formatted, its `[name @ 0x…]` prefix included;
  one FFmpeg log call is one Liquidsoap log line, possibly a partial line.
- Delivery is asynchronous, on the bindings' log thread (`avutil.md` §7.1).
- Before start-up, FFmpeg uses its own default level and prints to stderr.
- Before Liquidsoap's log stops, when `capture` is `true`, the callback is
  cleared ([avutil.md §7.1](../../../../modules/synced/ffmpeg/spec/avutil.md#71-log-messages)): FFmpeg goes on logging while
  finalisers free its contexts, and those messages go to stderr.

## 3. Start-up actions

- On load of the FFmpeg layer: `Avdevice.init ()` (registers input and
  output devices).
- In Liquidsoap's init phase: log at level 3 under `ffmpeg`
  `libavutil <v>, libavformat <v>, libavcodec <v>, libswresample <v>, libswscale <v>`,
  each `<v>` as `major.minor.micro` (from the implementation:
  `libavutil 61.7.100, libavformat 63.7.100, libavcodec 63.14.100, libswresample 7.3.100, libswscale 10.2.100`).
- The exception `Avutil.Error `Encoder_not_found`prints as`The requested ffmpeg encoder was not found, please make sure that ffmpeg was compiled with support for it`.

## 4. Liquidsoap time bases

| Name                   | Value                               |
| ---------------------- | ----------------------------------- |
| main ticks time base   | `1/main_rate`                       |
| audio sample time base | `1/settings.frame.audio.samplerate` |
| video sample time base | `1/settings.frame.video.framerate`  |

`main_rate` is the least common multiple of the audio rate and the video
rate; when one of them is 0 it is the other; both 0 is an error. Positions
and lengths in Liquidsoap content are in main ticks.

| Audio rate | Video rate | Main rate | Main ticks time base |
| ---------- | ---------- | --------- | -------------------- |
| 44100      | 25         | 44100     | `1/44100`            |
| 44100      | 24         | 88200     | `1/88200`            |

(from the implementation: `Using 44100Hz audio, 25Hz video, 44100Hz main.`
and `Using 44100Hz audio, 24Hz video, 88200Hz main.`)

Each time base is built on each call from the frame rates. The rates are
evaluated once, after the script is parsed, and fixed from then on; the time
bases are read only after that.

## 5. Time-base conversion

`convert_time_base ~src ~dst t` converts the integer `t` counted in `src`
units into `dst` units:

1. `num = src.num × dst.den`, `den = src.den × dst.num`, in native (63-bit)
   integers.
2. When `den = 0`, the result is `Int64.max_int`.
3. Otherwise the result is `(t × num) / den` in 64-bit integers: the
   product wraps on overflow, the division truncates toward zero (a negative
   `t` rounds up).

It is not FFmpeg's `av_rescale_q`, which rounds to nearest and does not
overflow. Every conversion of the layer between a stream time base and main
ticks goes through it; where each one happens is the owning file's
([timestamps.md](timestamps.md), the decoder files, [filters.md](filters.md)).

## 6. Duration helper

FFmpeg gives no reliable packet or frame duration. The helper takes the
timestamp increment between consecutive items as the duration of the earlier
ones, and groups items so that a chunk of content always has a non-zero
length: files exist with two successive packets of the same DTS.

### 6.1 State

Created with:

| Parameter                          | Meaning                                                                    |
| ---------------------------------- | -------------------------------------------------------------------------- |
| `mode`                             | `` `PTS `` or `` `DTS ``: only used in the log message of §6.2             |
| `src`                              | time base of the items' timestamps                                         |
| `convert_ts`                       | whether the timestamps written back into items are converted to main ticks |
| `get_ts`, `set_ts`, `get_duration` | accessors on an item (timestamp and duration in `src`)                     |
| `offset`                           | added to every timestamp read, default `0`                                 |
| `last_ts`                          | initial last timestamp (in `src`, offset included), default none           |

`dst` is the main ticks time base (§4), read at creation. The state also
holds the pending list, initially empty.

### 6.2 `push t item`

Let `ts = get_ts item`.

1. No `last_ts`, `ts` present: `ts := ts + offset`; write back
   `set_ts item (Some ts')` where `ts'` is `ts` converted from `src` to `dst`
   (§5) when `convert_ts`, `ts` otherwise; `last_ts := ts`; duration 0.
2. `last_ts` present, `ts` present: `ts := ts + offset`. When
   `ts < last_ts`, log at level 3
   `Invalid ffmpeg content: non-monotonic <PTS|DTS>: <ts> < <last_ts>` and
   raise `Content.Invalid` (state unchanged). Otherwise write back as in
   step 1, `last_ts := ts`, and duration = `convert_time_base (ts − old last_ts)`
   converted to a native integer.
3. `ts` absent: duration 0; nothing written back; `last_ts` unchanged.
4. Duration > 0: the pending list becomes `[(0, item)]` and the result is
   `Some (duration, previous pending list)`.
5. Duration 0: the item is appended to the pending list with position 0;
   the result is `None`.

Every item is at position 0 of its chunk. Equal timestamps are accepted and
grouped. The duration of each chunk is converted from the difference, not
from the two converted timestamps.

### 6.3 `flush t`

Returns `(length, pending list)` where `length` is the largest
`get_duration` among the pending items, each converted from `src` to main
ticks, and at least 1. The pending list is not emptied.

### 6.4 `last_ts t`

The last timestamp kept, in `src`, offset included.

Callers: the copy and raw decoders, bitstream filters, inline encoders and
filter outputs. Each owner states its mode, `convert_ts` and
what it does with the result.

## 7. Pixel formats

| Name                                   | Value      |
| -------------------------------------- | ---------- |
| internal frame pixel format            | `yuv420p`  |
| internal frame pixel format with alpha | `yuva420p` |

- **Has alpha**: a pixel format has alpha when its descriptor's flags
  contain `Alpha`. A format without descriptor raises `Not_found`
  (`avutil.md` §4.11).
- **Internal format for** `f`: `yuva420p` when `f` has alpha, `yuv420p`
  otherwise. Scalers between FFmpeg frames and internal images use it on the
  internal side.
- **Encoder pixel format** for a codec, given an optional configured name
  and `alpha` (default `false`):
  1. a configured name is parsed with `Pixel_format.of_string`; an unknown
     name raises the binding's failure. `alpha` is not consulted;
  2. otherwise, when the codec's supported formats contain the preferred
     internal format (`yuva420p` if `alpha`, else `yuv420p`), that one;
  3. otherwise the first supported format without the `Hwaccel` flag, and
     with the `Alpha` flag when `alpha`;
  4. otherwise `yuv420p`, also when the codec lists no format. Hardware
     codecs list hardware formats and accept the regular one.
- **Set format alpha**: given codec parameters and a Liquidsoap video
  format whose alpha is still undetermined, determine it: `true` when the
  parameters' pixel format is known and has alpha, `false` otherwise. A
  format whose alpha is already determined is left alone. Decoders use it
  when they describe a stream ([decoder.md](decoder.md)).

## 8. Image packing

Liquidsoap's YUV420 image and the plane arrays of the bindings' scalers
convert into each other:

- **Pack** image → planes: `[| (Y, y_stride); (U, uv_stride); (V, uv_stride) |]`,
  plus `(A, y_stride)` as a fourth plane when the image has alpha. The
  planes are the image's own buffers.
- **Unpack** `~width ~height` planes → image:
  - three planes: U and V strides must be equal; the image is built over the
    buffers, without alpha;
  - four planes: also the alpha stride must equal the Y stride; the image
    has alpha;
  - a broken stride condition or another plane count is an assertion
    failure.

## 9. Video format comparison

A video frame format is the binding's record (`avutil.md` §4.13): size,
pixel format, pixel aspect, colour space, range, primaries, transfer
characteristic, chroma location.

- **Ignored properties**: `Color` unless `settings.ffmpeg.format_change.color`,
  then `Pixel_aspect` unless `settings.ffmpeg.format_change.pixel_aspect`.
  Both settings are read on every call.
- **Same video format**: the binding's `same_frame_format` with the ignored
  properties. Size and pixel format always count. Every consumer built for
  one video format (display stages, decoder scalers, filter inputs, frame
  fitting) uses this comparison to decide whether to rebuild.
- `Color` covers colour space, range, primaries, transfer characteristic and
  chroma location; with it ignored, the values of the first frame are kept.
- **Describing** a format for logs:
  - video: `<w>x<h> <pixel format>, pixel aspect <num/den>, colour space <…>, range <…>, primaries <…>, transfer <…>, chroma location <…>`,
    with `unknown` for an unnamed pixel format or an absent pixel aspect;
  - audio: `<sample format>, <rate> Hz, <channel layout description>`, with
    `unknown` for an unnamed sample format.

## 10. Scaling

| Key                                 | Accepted values                              |
| ----------------------------------- | -------------------------------------------- |
| `settings.ffmpeg.scaling_algorithm` | `"fast_bilinear"`, `"bilinear"`, `"bicubic"` |
| `settings.ffmpeg.scaling_threads`   | any integer; values below 0 count as 0       |

- **Algorithm** reader: the swscale flag of the same name, or "unknown" for
  any other value. Each caller reports an unknown value its own way: the
  container encoder raises an `encoder` error at the script position, the
  inline encoder fails. Frame fitting passes the setting's raw string as
  the `flags` option of a `scale` filter ([filters.md §16.4](filters.md#164-fit)).
  Decoder scalers pass no flag.
- **Threads** reader: `max 0 value`. `1` scales on the calling thread; `0`
  is one thread per core; `n` splits each frame over `n` threads. Default 1.
  Reason (setting's description): splitting a frame costs a fan-out and a
  join on every frame, which a stream held to real time pays over and over
  and rarely earns back.
- Both are read when a scaler is created.

## 11. Hardware contexts

Encoders that may use hardware acceleration call this once per video stream
with: `hwaccel` (`none`, `auto`, `internal`, `device`, `frame`), an optional
hardware pixel format `hpf`, an optional device name, options, the target
pixel format `tpf` (§7), the target width and height, and the codec. It
returns an optional hardware context and the pixel format of the stream.
The encoder parameters themselves are [encoder-internal.md](encoder-internal.md)'s.

1. `hwaccel = none`: return (no context, `tpf`).
2. For each hardware configuration of the codec, in FFmpeg's order
   (the codec's hardware configurations, [avcodec.md §4.1](../../../../modules/synced/ffmpeg/spec/avcodec.md#41-top-level)), with methods `M`, pixel format `p` and device
   type `d`, take the first matching row; a row that only logs moves to the
   next configuration, a row that returns ends the search. "Method fits m"
   means `hwaccel = auto` or `hwaccel = m`. "Format fits" means `p` is
   `none`, or `hpf` is absent, or `p = hpf`. `default(p)` is `tpf` when `p`
   is `none`, else `p`.

   | #   | Condition                                                      | Action                                                                                                                                                                              |
   | --- | -------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
   | 1   | `internal ∈ M`, method fits `internal`, format fits, no device | log; return (no context, `hpf` or `default(p)`)                                                                                                                                     |
   | 2   | `internal ∈ M`, method does not fit `internal`                 | log                                                                                                                                                                                 |
   | 3   | `internal ∈ M`, a device is set                                | log                                                                                                                                                                                 |
   | 4   | `internal ∈ M`, `hpf` set                                      | log                                                                                                                                                                                 |
   | 5   | `hw_device_ctx ∈ M`, method fits `device`, format fits         | log; create a device context of type `d` (device, options); return (device context, `hpf` or `default(p)`)                                                                          |
   | 6   | `hw_device_ctx ∈ M`, method does not fit `device`              | log                                                                                                                                                                                 |
   | 7   | `hw_device_ctx ∈ M`, `hpf` set                                 | log                                                                                                                                                                                 |
   | 8   | `hw_frames_ctx ∈ M`, method fits `frame`, format fits          | log naming `p`; create a device context of type `d`, then a frame context of the target size from `tpf` to `p`; return (frame context, `hpf` when `p` is `none`, else `hpf` or `p`) |
   | 9   | `hw_frames_ctx ∈ M`, `hpf` set                                 | log                                                                                                                                                                                 |
   | 10  | `ad_hoc ∈ M`                                                   | log `unsupported ad-hoc hardware acceleration`                                                                                                                                      |

   Every log line is at level 3 under `ffmpeg` and names the codec.

3. No row returned: when `hwaccel` is not `auto`, raise an `encoder` error
   with no position:
   `No suitable hardware acceleration method "<hwaccel>" found for codec <name>!`.
   With `auto`, return (no context, `tpf`).

Device and frame context creation failures raise from the binding
(`avutil.md` §4.18) to the encoder.

## 12. Builds without FFmpeg

- `liquidsoap.build_config.optionals.ffmpeg` is `true` when the FFmpeg
  bindings were found at build time, `false` otherwise (from the
  implementation: `true` on this build). The build summary prints
  `Ffmpeg : yes` or `no (requires ffmpeg)`.
- Without them, none of the FFmpeg layer exists: no `ffmpeg.*`,
  `track.ffmpeg.*`, `input.ffmpeg`, `settings.ffmpeg.*`, no FFmpeg decoders
  or encoders, no `ffmpeg` converters ([converters.md, Without FFmpeg](converters.md#5-without-ffmpeg)).
  The script library guards its FFmpeg definitions with `%ifdef` on an FFmpeg
  builtin (for instance `%ifdef input.ffmpeg`, `%ifdef ffmpeg.filter.ebur128`
  in autocue, which then logs that autocue is disabled).
- A process that only typechecks scripts (playground, language server
  analysis) links a stand-in for the FFmpeg content kinds; the kinds it
  registers are [content.md](content.md)'s.
