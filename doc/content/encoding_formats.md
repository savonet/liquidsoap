# Encoding Formats

When sending audio or video to an output — a file, an Icecast stream, an HLS endpoint — you need to specify how it should be encoded. Liquidsoap uses **encoder values** for this. They are passed directly to output operators and define the codec, bitrate, channels, and other format details.

```liquidsoap
output.file(%mp3(bitrate=128), "/tmp/archive.mp3", source)
output.icecast(%opus(bitrate=96), host="...", mount="stream.opus", source)
```

This page covers all built-in encoders, their parameters, and how to choose among them.

## Encoder Syntax

Encoders are written with a `%` prefix followed by optional parameters in parentheses:

```liquidsoap
%encoder_name(parameter1=value1, parameter2=value2)
```

All parameters are optional and have defaults. The following are equivalent:

```liquidsoap
%mp3
%mp3()
%mp3(bitrate=128, samplerate=44100, stereo=true)
```

You can write `mono=true` or `channels=1` interchangeably. Parameters can appear in any order.

## Format Availability

Not every encoder is available in every build. Some require optional libraries. If you try to use an unavailable encoder, you will see:

```
Error 12: Unsupported encoder: %xyz().
You must be missing an optional dependency.
```

To check what is available in your build, run `liquidsoap --build-config`.

## Choosing an Encoder

The right encoder depends on your use case:

**Live streaming** (Icecast, SHOUTcast): Use a format your listeners' players support. `%mp3` has the widest compatibility. `%opus` and `%vorbis` are well-supported in modern browsers and offer good quality at lower bitrates. `%fdkaac` is a good choice when AAC is required for Icecast. Lossless formats like `%flac` (via `%ogg(%flac)`) are an option if your listeners can handle the bandwidth.

**HLS**: Use `%ffmpeg` — it is the only encoder that can produce the container formats (MPEG-TS, fMP4) that HLS segments require. See the [FFmpeg encoder](ffmpeg_encoder.html) page for details.

**Archiving**: Use lossless formats when quality matters: `%flac` for compressed lossless, `%wav` for raw PCM. If storage is a concern, `%mp3` or `%opus` at high bitrate are good lossy choices.

**Re-encoding / transcoding**: Use `%ffmpeg`, which gives access to all FFmpeg codecs and containers. It is the most flexible option and the only one supporting video. See the [FFmpeg encoder](ffmpeg_encoder.html) page for details.

**Real-time / embedded**: `%shine` (fixed-point MP3) is designed for architectures without a hardware FPU (e.g. some ARM boards). `%opus` at small frame sizes (`frame_size=10.`) is well-suited for low-latency applications like voice.

## Format Reference

### MP3

MP3 encoding is provided by `libmp3lame` and comes in three flavors:

- `%mp3` or `%mp3.cbr`: Constant bitrate
- `%mp3.vbr`: Variable bitrate, quality-based
- `%mp3.abr`: Average bitrate with optional min/max bounds

**Common parameters** (all three flavors):

| Parameter          | Default          | Description                                                                                  |
| ------------------ | ---------------- | -------------------------------------------------------------------------------------------- |
| `stereo`           | `true`           | Encode as stereo. Use `mono=true` or `channels=1` for mono.                                  |
| `stereo_mode`      | `"joint_stereo"` | One of `"stereo"`, `"joint_stereo"`, `"default"`. Default lets lame choose based on bitrate. |
| `samplerate`       | `44100`          | Output sample rate in Hz.                                                                    |
| `internal_quality` | `2`              | Lame's internal quality setting, `0` (best) to `9` (worst).                                  |
| `id3v2`            | `false`          | Prepend an ID3v2 tag. Can also be set to an integer specifying the ID3v2 version (e.g. `3`). |

**`%mp3` / `%mp3.cbr` parameters:**

| Parameter | Default | Description               |
| --------- | ------- | ------------------------- |
| `bitrate` | `128`   | Constant bitrate in kbps. |

**`%mp3.vbr` parameters:**

| Parameter | Default | Description                               |
| --------- | ------- | ----------------------------------------- |
| `quality` | `4`     | Quality level, `0` (best) to `9` (worst). |

**`%mp3.abr` parameters:**

| Parameter     | Default | Description                           |
| ------------- | ------- | ------------------------------------- |
| `bitrate`     | `128`   | Target average bitrate in kbps.       |
| `min_bitrate` | —       | Minimum bitrate in kbps.              |
| `max_bitrate` | —       | Maximum bitrate in kbps.              |
| `hard_min`    | `false` | Strictly enforce the minimum bitrate. |

**Examples:**

```{.liquidsoap include="enc-mp3.liq" from="BEGIN" to="END"}

```

### Shine

Shine is a fixed-point MP3 encoder. It is useful on systems without a hardware floating-point unit (FPU), such as some embedded ARM devices, or anywhere `libmp3lame` is unavailable. It produces constant-bitrate MP3 only.

```liquidsoap
%shine(channels=2, samplerate=44100, bitrate=128)
```

| Parameter    | Default | Description         |
| ------------ | ------- | ------------------- |
| `channels`   | `2`     | Number of channels. |
| `samplerate` | `44100` | Sample rate in Hz.  |
| `bitrate`    | `128`   | Bitrate in kbps.    |

### WAV

WAV output writes PCM audio with a standard RIFF/WAV header. Useful for archiving uncompressed audio or piping to external tools.

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

Because Liquidsoap encodes a potentially infinite stream, the WAV header length field is set to its maximum value by default. If you know the duration in advance and need an accurate header, set `duration` explicitly.

**Examples:**

```{.liquidsoap include="enc-wav.liq" from="BEGIN" to="END"}

```

### FLAC

FLAC is a lossless compressed format. It comes in two variants:

- `%flac`: Native FLAC file format. Suitable for file output, not for streaming.
- `%ogg(%flac)`: FLAC encapsulated in an Ogg container. Can be streamed via Icecast.

```liquidsoap
%flac(samplerate=44100, channels=2, compression=5, bits_per_sample=16)
```

| Parameter         | Default | Description                                                                                    |
| ----------------- | ------- | ---------------------------------------------------------------------------------------------- |
| `samplerate`      | `44100` | Sample rate in Hz.                                                                             |
| `channels`        | `2`     | Number of channels.                                                                            |
| `compression`     | `5`     | Compression level, `0` (fastest, largest) to `8` (smallest, slowest). Does not affect quality. |
| `bits_per_sample` | `16`    | Bit depth: `8`, `16`, `24`, or `32`.                                                           |

**Examples:**

```{.liquidsoap include="enc-flac.liq" from="BEGIN" to="END"}

```

### FDK-AAC

FDK-AAC is a high-quality AAC encoder from Fraunhofer, supporting both AAC-LC and HE-AAC (AAC+). It is well-suited for Icecast streaming.

```liquidsoap
%fdkaac(channels=2, samplerate=44100, bitrate=64, bandwidth="auto",
        afterburner=false, aot="mpeg4_he_aac_v2", transmux="adts", sbr_mode=false)
```

| Parameter     | Default             | Description                                                                              |
| ------------- | ------------------- | ---------------------------------------------------------------------------------------- |
| `channels`    | `2`                 | Number of channels.                                                                      |
| `samplerate`  | `44100`             | Sample rate in Hz.                                                                       |
| `bitrate`     | `64`                | Constant bitrate in kbps. Mutually exclusive with `vbr`.                                 |
| `vbr`         | —                   | Variable bitrate mode, `1` (lowest) to `5` (highest). Mutually exclusive with `bitrate`. |
| `bandwidth`   | `"auto"`            | Audio bandwidth in Hz, or `"auto"`.                                                      |
| `afterburner` | `false`             | Enable afterburner quality enhancement (higher CPU usage).                               |
| `aot`         | `"mpeg4_he_aac_v2"` | Audio object type. See below.                                                            |
| `transmux`    | `"adts"`            | Output container. See below.                                                             |
| `sbr_mode`    | `false`             | Enable spectral band replication for HE-AAC.                                             |

**`aot` values:**

| Value               | Description                       |
| ------------------- | --------------------------------- |
| `"mpeg4_aac_lc"`    | AAC-LC (MPEG-4) — most compatible |
| `"mpeg4_he_aac"`    | HE-AAC / AAC+ (MPEG-4)            |
| `"mpeg4_he_aac_v2"` | HE-AAC v2 (MPEG-4) — stereo only  |
| `"mpeg4_aac_ld"`    | AAC-LD — low delay                |
| `"mpeg4_aac_eld"`   | AAC-ELD — enhanced low delay      |
| `"mpeg2_aac_lc"`    | AAC-LC (MPEG-2)                   |
| `"mpeg2_he_aac"`    | HE-AAC (MPEG-2)                   |
| `"mpeg2_he_aac_v2"` | HE-AAC v2 (MPEG-2)                |

**`transmux` values:** `"raw"`, `"adif"`, `"adts"`, `"latm"`, `"latm_out_of_band"`, `"loas"`.

See the [Hydrogenaudio knowledge base](http://wiki.hydrogenaud.io/index.php?title=Fraunhofer_FDK_AAC) for details on configuration values.

**Examples:**

```{.liquidsoap include="enc-fdkaac.liq" from="BEGIN" to="END"}

```

### Ogg Container

Ogg is a free, open container format. Several codecs can be encapsulated in it:

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

Vorbis is a free, open lossy audio codec. It comes in three bitrate modes:

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
| `bitrate`     | Target bitrate in kbps (ABR/CBR modes).                      |
| `min_bitrate` | Minimum bitrate in kbps (ABR only).                          |
| `max_bitrate` | Maximum bitrate in kbps (ABR only).                          |

**Examples:**

```{.liquidsoap include="enc-vorbis.liq" from="BEGIN" to="END"}

```

#### Opus

Opus is a modern, versatile lossy codec optimised for both voice and music. It excels at low bitrates and low latency and is well-supported in browsers and modern players. Opus is always encapsulated in Ogg.

```liquidsoap
%opus(samplerate=48000, channels=2, bitrate="auto",
      vbr="unconstrained", complexity=9, frame_size=20.)
```

| Parameter         | Default           | Description                                                                                                             |
| ----------------- | ----------------- | ----------------------------------------------------------------------------------------------------------------------- |
| `channels`        | `2`               | `1` or `2` only. Use `mono=true` / `stereo=true` as shorthand.                                                          |
| `samplerate`      | `48000`           | Must be one of: `8000`, `12000`, `16000`, `24000`, `48000`.                                                             |
| `bitrate`         | `"auto"`          | Bitrate in kbps (`5`–`512`), or `"auto"`.                                                                               |
| `vbr`             | `"unconstrained"` | `"none"` (CBR), `"constrained"`, or `"unconstrained"` (VBR).                                                            |
| `application`     | `"audio"`         | `"audio"` (music/general), `"voip"` (voice calls), `"restricted_lowdelay"` (low-latency).                               |
| `complexity`      | —                 | Encoder complexity, `0` (fastest) to `10` (best quality). Not set by default (libopus uses `9`).                        |
| `frame_size`      | `20.`             | Frame duration in ms: `2.5`, `5.`, `10.`, `20.`, `40.`, or `60.`. Smaller = lower latency.                              |
| `signal`          | —                 | Hint to the encoder: `"music"` or `"voice"`. Not set by default (encoder auto-detects).                                 |
| `max_bandwidth`   | —                 | `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"`, or `"full_band"`. Not set by default (full band). |
| `dtx`             | `false`           | Enable discontinuous transmission — silence periods produce minimal output.                                             |
| `phase_inversion` | `true`            | Enable phase inversion for stereo. Disabling improves mono compatibility at a slight quality cost.                      |

See the [Opus documentation](http://www.opus-codec.org/docs/) for full details.

**Examples:**

```{.liquidsoap include="enc-opus.liq" from="BEGIN" to="END"}

```

### FFmpeg

The `%ffmpeg` encoder provides access to all FFmpeg codecs and containers, including video. It is the most powerful and flexible option, and the only one suitable for HLS output.

See the dedicated [FFmpeg encoder](ffmpeg_encoder.html) page for full documentation.

```{.liquidsoap include="enc-ffmpeg.liq" from="BEGIN" to="END"}

```
