# Findings: inline encoding and decoding operators

Paths relative to `src/core/optionals/ffmpeg/` unless stated.

## Defects

### Chunk lengths truncate per packet, so encoded audio drifts

What: `Ffmpeg_utils.Duration.push` converts each dts difference to main ticks
with truncating integer division (`base/ffmpeg_utils.ml:481-482`). With AAC
at 48 kHz and main rate 44100, each 1024-sample packet is 940.8 ticks and is
counted as 940. The producer emits 0.085 % less length than the media it
carries, so it pulls input faster than real time.
Why: the inline stream runs ahead of wall time; over an hour about 3 s more
media than clock time reaches the copy outputs. Any target rate whose packet
duration is not a whole number of main ticks is affected (video too, when
`main_rate` is not a multiple of the encoder time base).
**Checked**: free-running clock, `samplerate=48000`: 12139 input frames for
12126 output frames (ratio 1.00107, growing); `samplerate=44100`: 13360 for
13357 (constant 3-frame delay).

### Decoders produce the global channel count, not the output type's

What: `decode_audio_frame` resamples to `Frame.audio_channels`
(`decoder/builtins_ffmpeg_decoder.ml:39-41`), the global
`settings.frame.audio.channels`, while the operator's output type is
`pcm('b)`. When the output is typed with another channel count the content
does not match its type.
Why: crash downstream. **Checked**: `settings.frame.audio.channels := 1`,
`(ffmpeg.decode.audio(e) : source(audio=pcm(stereo)))` into
`output.file(%wav, …)`: `Invalid_argument("Array.iter2: arrays must have the
same length")` in `Content_audio.Data.blit`. Video decode likewise uses the
global frame size (`decoder/builtins_ffmpeg_decoder.ml:225-227`), read only.

### Encoder tail is never delivered

What: the only `` `Flush `` comes from `Child_support.base#on_sleep`
(`src/core/operators/child_support.ml:200-204`), after `child#sleep`; what it
writes goes into `child_buffer`, replaced at the next wake-up and never read.
In addition the chunker is never drained (`Duration.flush` is unused by
inline encoders), the video fps filter never gets EOF, and raw mode does
nothing on flush.
Why: the last encoded packets (encoder delay, lookahead, the chunker's
held-back item) of a finite stream never reach outputs; a file written
from `ffmpeg.encode.*` ends short. Read only.

**Verification** (verification pass): Confirmed by run: 1 s sine, AAC: 44 packets through the container encoder, 41 through `ffmpeg.encode.audio` then `%audio.copy`.

### Missing codec is an assertion failure at the first frame

What: `%ffmpeg(%audio(b="128k"))` reaches `| _ -> assert false`
(`encoder/builtins_ffmpeg_encoder.ml:545`) when the first frame builds the
transform.
Why: a script error is reported as "Assertion failed" from inside a child
clock, after start-up, with no script position. **Checked**.

### Raw-mode stream options are silently ignored

What: in raw mode the copied option table is never used nor checked
(`encoder/builtins_ffmpeg_encoder.ml:548-557`). The comment says raw options
"travel with the content to whichever encoder consumes it later", but
`Ffmpeg_raw_content` params have no option field.
Why: `%audio.raw(foo=12)` runs with no error or warning; a typo in an
intended option is invisible. **Checked** (no error, script ran to
shutdown).

## Asymmetries

### Transform persists across tracks, contrary to the comments

What: `base/ffmpeg_inline.ml:39-41` says "codec state never survives a track
boundary" and `encoder/builtins_ffmpeg_encoder.ml:497-499` says the build
"runs again at every track boundary". Flush happens only at sleep, so
encoders, decoders and timestamp counters survive track boundaries.
Why: the comments and the behaviour disagree; pass 2 must pick one. Packets
of the end of a track are emitted after the output's track mark. Read only.

### Metadata and track marks are not aligned with converted data

What: `relay_metadata` adds marks at the input frame's positions counted from
the generator head (`base/ffmpeg_inline.ml:30-37`, `Generator.default_pos`),
while the converted data lands after what is already buffered plus codec and
chunker delay.
Why: metadata and track ends come early by the buffered remainder plus the
codec delay; with copy outputs that split files on tracks, the first packets
of a new track belong to the previous file. Read only.

### Audio copy decoder ignores codec changes under one stream index

What: the audio copy decoder rebuilds only when `(stream_idx, time_base)`
changes (`decoder/builtins_ffmpeg_decoder.ml:85-95`); the video one also
compares the codec record (`:331-348`).
Why: content with the same index and a new codec (e.g. after an inline
encoder rebuilt with the same index, or a relaxed copy check) is decoded with
the old decoder. Read only.

### Inline audio encoders accept only float PCM

What: input type is `Format_type.audio ()`; the container internal encoder
supports pcm*s16/pcm_f32 via `InternalResampler_pcm*\*`
(`encoder/ffmpeg_internal_encoder.ml:56-64`).
Why: feature gap between the two encoder paths. Read only.

### `audio_video` wrappers drop video metadata

What: `src/libs/ffmpeg.liq:63-97,162-195` take metadata and track marks from
the audio track only. Harmless when both come from the same source, which is
the only case the wrapper builds. Read only.

## Gaps

### Stream index after sleep depends on the GC

What: the weak set entry is kept alive only by transform closures
(`encoder/builtins_ffmpeg_encoder.ml:40-47,506-509`). A rebuild after sleep
gets the old index if the entry was not collected, a new one otherwise.
Why: downstream copy encoders see either "same stream, timestamps back to
0" or "new stream": non-deterministic; see
[timestamps.md](../timestamps.md) for what each causes. Read only.

### Sharing the input moves other readers into the child clock

What: an output reading the same source as an inline encoder is placed in
the encoder's nested clock and animated only by the encoder's reads.
**Checked**: log "Starting nested clock, controlled by output.dummy.1, …
sources: sine (passive), output.dummy (output), sine.child (output)".
Why: undocumented; that output's pacing depends on the encoder's consumer.

### No global header handling

What: inline encoders never set `+global_header`; the container encoder
does when the muxer needs it. Users must pass `flags="+global_header"` or
use `ffmpeg.filter.bitstream.extract_extradata` (`doc/content/ffmpeg.md`).
Read only.

## To verify

### The runtime subtype check cannot fail

What: `Typing.(effective_t <: type_t)` is run against `Type.fresh
output_frame_t` (`encoder/builtins_ffmpeg_encoder.ml:100,145,302,366,493`), a
fresh copy of the generic kind type, not the operator's script type. It
likely constrains nothing and never propagates resolved parameters to the
script type. Read only.

### Raw decoders on content without full parameters

What: `Option.get` on raw audio `channel_layout`/`sample_rate`/
`sample_format` (`decoder/builtins_ffmpeg_decoder.ml:147-149`). Whether any
producer (filter outputs) emits raw audio content with a missing parameter
is to check against [content.md](../content.md). Read only.

### Content-level parameters for multi-chunk content

What: copy decoders build from the content-level `params` for every chunk,
whatever the chunk's index. If one content value can hold chunks of two
streams with different parameters, the second is decoded with the first's
parameters. Depends on [content.md](../content.md) merge rules. Read only.
