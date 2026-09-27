# Encoding Formats

When you send audio or video to an output, such as a file, an Icecast server or an HLS playlist, you need to specify how to encode it. Liquidsoap uses _encoder values_ for this. You pass an encoder value to the output operator. The encoder value defines the codec, bitrate, number of channels and other format details.

```liquidsoap
output.file(%mp3(bitrate=128), "/tmp/archive.mp3", source)
output.icecast(%opus(bitrate=96), host="...", mount="stream.opus", source)
```

This page describes the built-in encoders, their parameters, and how to choose among them.

## Encoder Syntax

Encoders are written with a `%` prefix followed by optional parameters in parentheses:

```liquidsoap
%encoder_name(parameter1=value1, parameter2=value2)
```

All parameters are optional and have defaults. With the default settings, the following are equivalent:

```liquidsoap
%mp3
%mp3()
%mp3(bitrate=128, samplerate=44100, stereo=true)
```

Parameters can appear in any order. Audio encoders accept `mono=true`, `stereo=false` or the bare `mono` flag to encode mono audio. Most of them also accept `channels`, see the reference below.

## Format Availability

Some encoders require optional libraries, so not every encoder is available in every build. If you use an unavailable encoder, you get the following error:

```
Error 12: Unsupported encoder: %xyz().
You must be missing an optional dependency.
```

To check what is available in your build, run `liquidsoap --build-config`.

## Choosing an Encoder

The right encoder depends on what you want to do.

For live streaming to Icecast or Shoutcast, use a format that your listeners' players support. `%mp3` has the widest compatibility. `%opus` and `%vorbis` are supported by modern browsers and give good quality at lower bitrates. Use `%fdkaac` when you need AAC. If your listeners have enough bandwidth, you can also stream lossless audio with `%ogg(%flac)`.

For HLS, use `%ffmpeg`. It is the only encoder that produces the MPEG-TS and fMP4 containers used for HLS segments. See the [FFmpeg encoder](./ffmpeg_encoder.md) page for details.

For archiving, use `%flac` for compressed lossless audio and `%wav` for raw PCM. If you need to save storage, use `%mp3` or `%opus` at a high bitrate.

For video, and for any codec or container that the other encoders do not cover, use `%ffmpeg`. It gives access to all FFmpeg codecs and containers. See the [FFmpeg encoder](./ffmpeg_encoder.md) page for details.

On systems without a hardware floating-point unit (FPU), such as some ARM boards, use `%shine`, a fixed-point MP3 encoder. For low-latency applications such as voice, use `%opus` with a small frame size, for instance `frame_size=10.`.

## Format Reference

### MP3

MP3 encoding is provided by `libmp3lame` and comes in three flavors:

- `%mp3` or `%mp3.cbr`: Constant bitrate
- `%mp3.vbr`: Variable bitrate, quality-based
- `%mp3.abr`: Average bitrate with optional min/max bounds

Common parameters (all three flavors):

| Parameter          | Default          | Description                                                                                                 |
| ------------------ | ---------------- | ----------------------------------------------------------------------------------------------------------- |
| `stereo`           | `true`           | Encode as stereo. Use `mono=true` or `stereo=false` for mono.                                               |
| `stereo_mode`      | `"joint_stereo"` | One of `"stereo"`, `"joint_stereo"`, `"default"`. Default lets lame choose based on bitrate.                |
| `samplerate`       | `44100`          | Output sample rate in Hz: `8000`, `11025`, `12000`, `16000`, `22050`, `24000`, `32000`, `44100` or `48000`. |
| `internal_quality` | `2`              | Lame's internal quality setting, `0` (best) to `9` (worst).                                                 |
| `id3v2`            | `false`          | Prepend an ID3v2 tag. `true` uses ID3v2 version 3. Set it to an integer to choose the version.              |

`%mp3` / `%mp3.cbr` parameters:

| Parameter | Default | Description               |
| --------- | ------- | ------------------------- |
| `bitrate` | `128`   | Constant bitrate in kbps. |

`%mp3.vbr` parameters:

| Parameter | Default | Description                               |
| --------- | ------- | ----------------------------------------- |
| `quality` | `4`     | Quality level, `0` (best) to `9` (worst). |

`%mp3.vbr` also accepts the `min_bitrate`, `max_bitrate` and `hard_min` parameters of `%mp3.abr`.

`%mp3.abr` parameters:

| Parameter     | Default | Description                           |
| ------------- | ------- | ------------------------------------- |
| `bitrate`     | `128`   | Target average bitrate in kbps.       |
| `min_bitrate` | —       | Minimum bitrate in kbps.              |
| `max_bitrate` | —       | Maximum bitrate in kbps.              |
| `hard_min`    | `false` | Strictly enforce the minimum bitrate. |

Bitrates must be one of `8`, `16`, `24`, `32`, `40`, `48`, `56`, `64`, `80`, `96`, `112`, `128`, `144`, `160`, `192`, `224`, `256` or `320`.

Examples:

```{.liquidsoap include="enc-mp3.liq" from="BEGIN" to="END"}

```

### Shine

Shine is a fixed-point MP3 encoder. Use it on systems without a hardware floating-point unit (FPU), such as some embedded ARM devices, or when `libmp3lame` is unavailable. Shine only produces constant-bitrate MP3. `%mp3.fxp` is an alias for `%shine`.

```liquidsoap
%shine(channels=2, samplerate=44100, bitrate=128)
```

| Parameter    | Default | Description         |
| ------------ | ------- | ------------------- |
| `channels`   | `2`     | Number of channels. |
| `samplerate` | `44100` | Sample rate in Hz.  |
| `bitrate`    | `128`   | Bitrate in kbps.    |

### WAV

The WAV encoder writes PCM audio with a standard RIFF/WAV header. Use it to archive uncompressed audio or to send audio to external tools.

```liquidsoap
%wav(stereo=true, channels=2, samplesize=16, header=true, duration=10.)
```

| Parameter    | Default | Description                                                               |
| ------------ | ------- | ------------------------------------------------------------------------- |
| `stereo`     | `true`  | Encode as stereo.                                                         |
| `channels`   | `2`     | Number of channels (overrides `stereo`).                                  |
| `samplesize` | `16`    | Bit depth: `8`, `16`, `24`, or `32`.                                      |
| `samplerate` | `44100` | Sample rate in Hz.                                                        |
| `header`     | `true`  | Include the WAV header. Set to `false` for raw PCM.                       |
| `duration`   | —       | Expected duration in seconds. Used to set the length field in the header. |

Liquidsoap encodes a potentially infinite stream, so the WAV header length fields are set to their maximum value by default. If you know the duration in advance and need an accurate header, set `duration`.

Examples:

```{.liquidsoap include="enc-wav.liq" from="BEGIN" to="END"}

```

### FLAC

FLAC is a lossless compressed format. It comes in two variants:

- `%flac`: Native FLAC file format. Suitable for file output, not for streaming.
- `%ogg(%flac)`: FLAC encapsulated in an Ogg container. Use this variant to stream FLAC to Icecast.

```liquidsoap
%flac(samplerate=44100, channels=2, compression=5, bits_per_sample=16)
```

| Parameter         | Default | Description                                                                                    |
| ----------------- | ------- | ---------------------------------------------------------------------------------------------- |
| `samplerate`      | `44100` | Sample rate in Hz.                                                                             |
| `channels`        | `2`     | Number of channels.                                                                            |
| `compression`     | `5`     | Compression level, `0` (fastest, largest) to `8` (smallest, slowest). Does not affect quality. |
| `bits_per_sample` | `16`    | Bit depth: `8`, `16`, `24`, or `32`.                                                           |

Examples:

```{.liquidsoap include="enc-flac.liq" from="BEGIN" to="END"}

```

### FDK-AAC

FDK-AAC is an AAC encoder from Fraunhofer. It supports AAC-LC and HE-AAC (AAC+).

```liquidsoap
%fdkaac(channels=2, samplerate=44100, bitrate=64, bandwidth="auto",
        afterburner=false, aot="mpeg4_he_aac_v2", transmux="adts", sbr_mode=false)
```

| Parameter     | Default             | Description                                                                              |
| ------------- | ------------------- | ---------------------------------------------------------------------------------------- |
| `channels`    | `2`                 | Number of channels.                                                                      |
| `samplerate`  | `44100`             | Sample rate in Hz, from `8000` to `96000`.                                               |
| `bitrate`     | `64`                | Constant bitrate in kbps. Mutually exclusive with `vbr`.                                 |
| `vbr`         | —                   | Variable bitrate mode, `1` (lowest) to `5` (highest). Mutually exclusive with `bitrate`. |
| `bandwidth`   | `"auto"`            | Audio bandwidth in Hz, or `"auto"`.                                                      |
| `afterburner` | `false`             | Enable afterburner quality enhancement (higher CPU usage).                               |
| `aot`         | `"mpeg4_he_aac_v2"` | Audio object type. See below.                                                            |
| `transmux`    | `"adts"`            | Output container. See below.                                                             |
| `sbr_mode`    | `false`             | Enable spectral band replication for HE-AAC.                                             |

`aot` values:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| `"mpeg4_aac_lc"`    | AAC-LC (MPEG-4), most compatible |
| `"mpeg4_he_aac"`    | HE-AAC / AAC+ (MPEG-4)           |
| `"mpeg4_he_aac_v2"` | HE-AAC v2 (MPEG-4), stereo only  |
| `"mpeg4_aac_ld"`    | AAC-LD, low delay                |
| `"mpeg4_aac_eld"`   | AAC-ELD, enhanced low delay      |
| `"mpeg2_aac_lc"`    | AAC-LC (MPEG-2)                  |
| `"mpeg2_he_aac"`    | HE-AAC (MPEG-2)                  |
| `"mpeg2_he_aac_v2"` | HE-AAC v2 (MPEG-2)               |

`transmux` values: `"raw"`, `"adif"`, `"adts"`, `"latm"`, `"latm_out_of_band"`, `"loas"`.

See the [Hydrogenaudio knowledge base](https://wiki.hydrogenaud.io/index.php?title=Fraunhofer_FDK_AAC) for details on configuration values.

Examples:

```{.liquidsoap include="enc-fdkaac.liq" from="BEGIN" to="END"}

```

### Ogg Container

Ogg is a free, open container format. It can carry `%vorbis`, `%opus`, `%flac` and `%speex` audio, and `%theora` video:

```liquidsoap
%ogg(codec1, codec2, ...)
```

As a shorthand, you can write `%vorbis(...)` instead of `%ogg(%vorbis(...))`. Both produce an Ogg stream.

All Ogg encoders share a `bytes_per_page` parameter that limits the size of Ogg logical pages:

```liquidsoap
# Limit page size to 4096 bytes
%vorbis(bytes_per_page=4096)
```

#### Vorbis

Vorbis is a free, open lossy audio codec. The Vorbis encoder has three bitrate modes:

```liquidsoap
# Variable bitrate (default)
%vorbis(samplerate=44100, channels=2, quality=0.3)

# Average bitrate
%vorbis.abr(samplerate=44100, channels=2, bitrate=128, max_bitrate=192, min_bitrate=64)

# Constant bitrate
%vorbis.cbr(samplerate=44100, channels=2, bitrate=128)
```

| Parameter     | Description                                                  |
| ------------- | ------------------------------------------------------------ |
| `samplerate`  | Sample rate in Hz (default: `44100`).                        |
| `channels`    | Number of channels (default: `2`).                           |
| `quality`     | VBR quality, `-0.2` (worst) to `1.0` (best). Default: `0.3`. |
| `bitrate`     | Target bitrate in kbps (ABR/CBR modes). CBR default: `128`.  |
| `min_bitrate` | Minimum bitrate in kbps (ABR only).                          |
| `max_bitrate` | Maximum bitrate in kbps (ABR only).                          |

Examples:

```{.liquidsoap include="enc-vorbis.liq" from="BEGIN" to="END"}

```

#### Opus

Opus is a lossy codec designed for both voice and music. It gives good quality at low bitrates and low latency, and is supported by browsers and modern players. The `%opus` encoder always encapsulates Opus in Ogg.

```liquidsoap
%opus(samplerate=48000, channels=2, bitrate="auto",
      vbr="constrained", complexity=9, frame_size=20.)
```

| Parameter         | Default         | Description                                                                                                                            |
| ----------------- | --------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `channels`        | `2`             | `1` or `2` only. Use `mono=true` / `stereo=true` as shorthand.                                                                         |
| `samplerate`      | `48000`         | Must be one of: `8000`, `12000`, `16000`, `24000`, `48000`.                                                                            |
| `bitrate`         | `"auto"`        | Bitrate in kbps, from `5` to `512`, or `"auto"` or `"max"`.                                                                            |
| `vbr`             | `"constrained"` | `"none"` (CBR), `"constrained"`, or `"unconstrained"` (VBR).                                                                           |
| `application`     | —               | `"audio"` (music/general), `"voip"` (voice calls), `"restricted_lowdelay"` (low-latency). Not set by default (libopus uses `"audio"`). |
| `complexity`      | —               | Encoder complexity, `0` (fastest) to `10` (best quality). Not set by default (libopus uses `9`).                                       |
| `frame_size`      | `20.`           | Frame duration in ms: `2.5`, `5.`, `10.`, `20.`, `40.`, or `60.`. Smaller frames give lower latency.                                   |
| `signal`          | —               | Hint to the encoder: `"music"` or `"voice"`. Not set by default (encoder auto-detects).                                                |
| `max_bandwidth`   | —               | `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"`, or `"full_band"`. Not set by default (full band).                |
| `dtx`             | `false`         | Enable discontinuous transmission: the encoder produces minimal output during silence.                                                 |
| `phase_inversion` | `true`          | Enable phase inversion for stereo. Disabling improves mono compatibility at a slight quality cost.                                     |

See the [Opus documentation](https://opus-codec.org/docs/) for full details.

Examples:

```{.liquidsoap include="enc-opus.liq" from="BEGIN" to="END"}

```

### FFmpeg

The `%ffmpeg` encoder gives access to all FFmpeg codecs and containers, including video. Use it for HLS output.

See the dedicated [FFmpeg encoder](./ffmpeg_encoder.md) page for full documentation.

```{.liquidsoap include="enc-ffmpeg.liq" from="BEGIN" to="END"}

```

### Other encoders

Liquidsoap also provides the following encoders:

- `%avi`: uncompressed audio and video in an AVI container.
- `%ndi`: audio and video for the NDI protocol, used with `output.ndi`.
- `%external`: pipes PCM audio to an external program. See [external encoders](./external_programs.md#external-encoders).
