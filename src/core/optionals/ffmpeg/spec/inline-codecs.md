# Inline encoding and decoding operators

This file owns the script operators that turn a track into `ffmpeg.copy` or
`ffmpeg.raw` content inside a script and back: `track.ffmpeg.encode.*`,
`track.ffmpeg.raw.encode.*`, `track.ffmpeg.decode.*`,
`track.ffmpeg.raw.decode.*` and their source-level wrappers `ffmpeg.encode.*`,
`ffmpeg.raw.encode.*`, `ffmpeg.decode.*`, `ffmpeg.raw.decode.*`. It also owns
the **inline producer** (§3), the plumbing these operators share with the
bitstream filters. It relies on: the content model of `ffmpeg.copy` and
`ffmpeg.raw` ([content.md](content.md)); the `%ffmpeg` format and its parsed
stream options ([encoder-format.md](encoder-format.md)); the stream-encoder pieces it
reuses — audio frame splitting ([encoder-internal.md](encoder-internal.md)),
hardware contexts and pixel format choice ([common.md](common.md)); the
encoder-side timestamp rules ([timestamps.md](timestamps.md)); the internal
video converter and display stage used by decoders
([decoder-internal.md §2.2](decoder-internal.md#22-internal-video-converter),
[side-data.md §3](side-data.md#3-autorotate-on-decoding)); child clocks
([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)); and the
FFmpeg bindings ([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)).

## 1. Operators

All eight track operators exist only when Liquidsoap is built with FFmpeg.
Each takes `?id : string?` (default `null`) and returns a track. Category
`Track / Conversion`. Descriptions: "Convert a track's content" (encoders),
"Decode a track content" (decoders). Types (from the implementation):

| Operator                        | Type                                                                                           |
| ------------------------------- | ---------------------------------------------------------------------------------------------- |
| `track.ffmpeg.encode.audio`     | `(?id : string?, format(audio=pcm('a), 'b), pcm('a)) -> ffmpeg.copy('c)`                       |
| `track.ffmpeg.raw.encode.audio` | `(?id : string?, format(audio=ffmpeg.audio.raw('a), 'b), pcm('c)) -> ffmpeg.audio.raw('a)`     |
| `track.ffmpeg.encode.video`     | `(?id : string?, format(video=yuv420p('a), 'b), yuv420p('a)) -> ffmpeg.copy('c)`               |
| `track.ffmpeg.raw.encode.video` | `(?id : string?, format(video=ffmpeg.video.raw('a), 'b), yuv420p('c)) -> ffmpeg.video.raw('a)` |
| `track.ffmpeg.decode.audio`     | `(?id : string?, ffmpeg.copy('a)) -> pcm('b)`                                                  |
| `track.ffmpeg.raw.decode.audio` | `(?id : string?, ffmpeg.audio.raw('a)) -> pcm('b)`                                             |
| `track.ffmpeg.decode.video`     | `(?id : string?, ffmpeg.copy('a)) -> yuv420p('b)`                                              |
| `track.ffmpeg.raw.decode.video` | `(?id : string?, ffmpeg.video.raw('a)) -> yuv420p('b)`                                         |

Typing consequences:

- Encoded mode types the format by its **input**: the format's `audio` (or
  `video`) field and the input track share `'a`. The format's channel count
  is therefore the input's channel count, and an encoded inline audio
  encoder never remixes channels.
- Raw mode types the format by its **output**: the input is any `pcm` /
  `yuv420p` track, and the output carries the raw format's parameters.
- The format's row type `'b` is open: a format may describe other streams
  (`%video` next to `%audio`), which this operator ignores. This lets one
  format value serve both halves of an `audio_video` wrapper.
- The stream description is looked up in the format under the name `audio`
  (audio operators) or `video` (video operators), whatever the field name of
  the input track.

The `id` argument names the producer source: `<id>.producer`, with `<id>`
defaulting to `audio` or `video`. Source ids are made unique by the generic
source machinery (`audio.producer`, `audio.producer.1`, … in logs).

## 2. Source wrappers

Twelve script functions wrap the track operators. Each takes `~id` defaulting
to its own name, e.g. `ffmpeg.encode.audio(~id=null("ffmpeg.encode.audio"), f, s)`.
The encoders are defined when `track.ffmpeg.raw.encode.audio` exists, the
decoders when `track.ffmpeg.decode.audio` exists.

| Wrapper                         | Arguments | Builds                                                 |
| ------------------------------- | --------- | ------------------------------------------------------ |
| `ffmpeg.encode.audio`           | `f, s`    | `{audio}` from `track.ffmpeg.encode.audio(f, s.audio)` |
| `ffmpeg.raw.encode.audio`       | `f, s`    | `{audio}` from `track.ffmpeg.raw.encode.audio`         |
| `ffmpeg.encode.video`           | `f, s`    | `{video}` from `track.ffmpeg.encode.video`             |
| `ffmpeg.raw.encode.video`       | `f, s`    | `{video}` from `track.ffmpeg.raw.encode.video`         |
| `ffmpeg.encode.audio_video`     | `f, s`    | `{audio, video}`, same `f` for both                    |
| `ffmpeg.raw.encode.audio_video` | `f, s`    | `{audio, video}`, same `f` for both                    |
| `ffmpeg.decode.audio`           | `s`       | `{audio}` from `track.ffmpeg.decode.audio`             |
| `ffmpeg.raw.decode.audio`       | `s`       | `{audio}`                                              |
| `ffmpeg.decode.video`           | `s`       | `{video}`                                              |
| `ffmpeg.raw.decode.video`       | `s`       | `{video}`                                              |
| `ffmpeg.decode.audio_video`     | `s`       | `{audio, video}`                                       |
| `ffmpeg.raw.decode.audio_video` | `s`       | `{audio, video}`                                       |

Every wrapper builds `source(id=id, {track_marks = track.track_marks(t),
metadata = track.metadata(t), <field> = t, …})` where `t` is the converted
**audio** track when there is one, the video track otherwise. In the
`audio_video` forms the video producer's metadata and track marks are not
used. Other fields of `s` are dropped. Examples of the printed types (from
the implementation):

```
ffmpeg.encode.audio_video :
  (?id : string?, format(audio=pcm('a), video=yuv420p('b), 'c),
   source(audio=pcm('a), video=yuv420p('b), 'd)) ->
  source(audio=ffmpeg.copy('e), video=ffmpeg.copy('f))

ffmpeg.raw.decode.audio_video :
  (?id : string?,
   source(audio=ffmpeg.audio.raw('a), video=ffmpeg.video.raw('b), 'c)) ->
  source(audio=pcm('d), video=yuv420p('e))
```

`liquidsoap -h` reports "This source uses file composition by default" for
every wrapper; composition is generic source machinery.

## 3. The inline producer

Every inline operator (and every bitstream filter, see
[bitstream-filters.md §4](bitstream-filters.md#4-structure)) is a **producer**: a source that reads its input
source in a child clock and converts each input frame through a
**transform**. The producer has the frame type `{<field> = <output type>}`;
the child is typed `{<field> = <input type>}` with an open row.

### 3.1 Construction

At operator evaluation:

1. Wrap the input source in a **child output**, an active output living in
   the child clock, named `<input id>.child`.
2. Create a passive child clock whose parent is the producer's clock, and
   unify it with the input's clock
   ([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)).
3. Install the producer's `process_frame` on the child output (§3.2).
4. The self-sync check of the child-clock reader is **off**: a child with a
   sync source (a hardware input, `input.srt`) is accepted. Inline transforms
   run at roughly real rate, in bursts.

The input source moves into the child clock. Any other output reading the
same source joins that child clock and is animated only when the producer
ticks it. Example log for `output.dummy(s)` next to
`output.dummy(ffmpeg.encode.audio(f, s))` (from the implementation):

```
[clock.audio.producer:3] Starting nested clock, controlled by output.dummy.1,
  sync: passive, sources: sine (passive), output.dummy (output), sine.child (output)
```

Several producers reading the same input (the two halves of an
`audio_video` wrapper, several encoders of one source) share one child
clock: a tick by any of them fills the buffers of all
([clock spec §15, Shared child clocks](../../../clock/spec/clock.md#15-child-clocks)).

### 3.2 Transform lifetime

The producer holds at most one **transform**, a function taking
`` `Frame frame `` or `` `Flush ``. The operator builds it from the buffer
the child output writes to.

On each child-clock tick where the input is ready, the child output pulls one
frame from the input. A frame of position 0 is skipped. Otherwise:

1. If no transform exists, build one with the current buffer.
2. Relay the frame's metadata and track marks into the buffer (§3.3).
3. Call the transform with `` `Frame frame ``.
4. If the buffer then holds more than `settings.clock.child.max_buffer`
   (default `10.` seconds) of media, raise the runtime error of kind
   `clock`: "Operator <producer> has buffered more than <n>s of data from
   <input>. …". This applies to a lone producer too.

The transform receives `` `Flush `` only when the producer goes to sleep
(§13). After `` `Flush `` the transform is discarded; the next frame builds a
new one. **A track boundary does not flush or rebuild the transform**: codec,
resampler, scaler, frame-rate filter and timestamp counters live from the
first frame after a wake-up to the next sleep.

### 3.3 Metadata and track marks

For each input frame, before the transform:

- each `(pos, m)` metadata of the frame is added to the buffer at position
  `pos`;
- each track mark at position `pos` with `pos < frame size` is added to the
  buffer at position `pos`. A mark at exactly the frame size belongs to the
  next frame and is dropped.

Buffer positions count from the buffer's **read head**, not from the end of
the data already buffered. Metadata added at the same position as an
existing entry is merged into it. Neither is shifted by the transform's
delay (§14) ([findings](findings/inline-codecs.md)).

### 3.4 Reading

The producer is ready when its buffer is not empty or the input is ready. To
produce a frame:

1. While the input is ready and the buffer holds less than one frame (the
   minimum length over the buffer's media fields), tick the child clock.
2. Take up to one frame from the buffer.

When the input stops being ready the producer returns what it has, possibly
a partial frame. Data pending inside the transform stays there.

Other source methods:

| Method        | Behaviour                                                                                        |
| ------------- | ------------------------------------------------------------------------------------------------ |
| `fallible`    | The input's.                                                                                     |
| `self_sync`   | The input's static/dynamic flag, with no sync source reported across the child clock.            |
| `remaining`   | Buffer remaining if known; else input remaining plus buffer length; `-1` if input reports `-1`.  |
| `seek len`    | Drop `min(buffer length, len)` from the buffer, seek the input by the rest; return the sum.      |
| `abort_track` | Clear the buffer, add a track mark at its start, abort the input's track. The transform is kept. |

### 3.5 Buffer lifetime

The buffer is created on each wake-up with the producer's content type and
installed into the child output. On sleep, the child clock is deregistered,
the input put to sleep, then the transform is flushed into that buffer. The
next wake-up replaces the buffer.

## 4. Format validation (encoders)

The format argument must be a `%ffmpeg` value. Checks, in order:

| When                     | Condition                                                              | Error                                                                                                 |
| ------------------------ | ---------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| evaluation               | top-level muxer options present, e.g. `%ffmpeg(foo=1, …)`              | `Invalid_value` (Error 7) on the format: "Muxer options are not supported for inline encoders: foo=1" |
| evaluation               | `format=` given                                                        | `Invalid_value`: "Format option is not supported inline encoders"                                     |
| typing                   | stream is `%audio.copy` / `%video.copy`                                | type error (Error 5)                                                                                  |
| transform build          | stream is `%audio` / `%video` without `codec`                          | assertion failure, reported as a source failure in the child clock                                    |
| transform build          | codec name unknown                                                     | `Avutil.Error Encoder_not_found` ("The requested ffmpeg encoder was not found, …")                    |
| transform build, encoded | options left over after the encoder is created                         | `Invalid_value`: "Unrecognized options: foo=12"                                                       |
| transform build          | `settings.ffmpeg.scaling_algorithm` not one of the three names (video) | `Failure "Invalid value set for ffmpeg scaling algorithm!"`                                           |

Errors at evaluation abort the script with their position. Errors at
transform build happen on the first frame, inside the child clock, under the
clock's source error rule
([clock spec §11](../../../clock/spec/clock.md#11-failure)); with no handler
the child clock and then its parent fail. Example (from the implementation):

```
[clock.audio.producer:2] Source sine.child failed (no handler):
  Invalid_value(_, "Unrecognized options: foo=12", 0)
[clock.audio.producer:3] Stopped: failed (…), after 0 ticks (0.00s)
```

Each transform build copies the stream's option table before handing it to
the encoder, which removes the options it consumes; the format's own table
stays intact for later builds. In raw mode the option table is not used and
not checked: `%audio.raw(foo=12)` runs without error or warning.

Defaults of the stream description (channels `2`, samplerate
`settings.frame.audio.samplerate`, video size and rate from the frame
settings, `hwaccel` `auto`, …) are those of the format parser
([encoder-format.md §5](encoder-format.md#5-audio-tracks), [§6](encoder-format.md#6-video-tracks)).

## 5. Stream index

Each transform build asks for a **stream index** for the input source:

1. Allocate a fresh index from the process-wide counter
   ([content.md, stream_idx](content.md#5-stream_idx-and-time_base)).
2. Look the input source itself (not an equal one) up in a process-wide
   table of `(source, index)` entries.
3. If an entry exists, use its index; the fresh one is discarded. Otherwise
   insert `(source, fresh)` and use `fresh`.

Every chunk the transform emits carries that index. Consequences:

- Audio and video encoders of the same source share one index. Reason
  (history): the index served one operator encoding audio and video
  together, and the track operators that replaced it share it through the
  source. What the copy encoder does with it:
  [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).
- Several encoders of one source (three AAC bitrates of one `s`) share one
  index.
- The entry lives while a transform references it, and is forgotten at an
  unspecified time after. After a sleep the next build gets the old index if
  the entry is still there, a new one otherwise.

Decoders do not allocate indexes; they read the index of each chunk (§10,
§11).

## 6. Audio encoding

Inputs fixed at transform build:

| Name                 | Value                                                     |
| -------------------- | --------------------------------------------------------- |
| input layout         | default layout for the input content type's channel count |
| input rate           | `settings.frame.audio.samplerate`                         |
| target layout        | default layout for the format's `channels`                |
| target rate `R`      | format `samplerate`                                       |
| target time base `T` | `1/R`                                                     |
| target sample format | format `sample_format` if given, else `dbl`               |

Encoded mode then:

1. Find the encoder by name; replace the sample format by the codec's best
   match for it.
2. Create the encoder with the copied options, target layout, sample format,
   rate and time base `T`. Its parameters become the content parameters
   `` `Audio params ``; its time base `E` is the chunk time base.
3. Frame size: none if the codec has the variable-frame-size capability,
   else the encoder's frame size.

Raw mode instead sets the content parameters to `{channel_layout = target
layout, sample_format = target sample format, sample_rate = R}`, frame size
none, chunk time base `T`.

Per input frame:

1. Resample the frame's first `position` samples (main ticks converted to
   samples) from planar float at the input layout and rate to the target
   layout, rate and sample format.
2. Pass the result to the audio frame writer of the stream encoder
   ([encoder-internal.md §5.1](encoder-internal.md#51-audio),
   [timestamps.md §3](timestamps.md#3-audio-internal-and-raw)), with time base `T`, rate
   `R` and the frame size: it re-chunks to the frame size when there is one
   and sets each frame's pts (§9).
3. Encoded: send each frame to the encoder; each packet out goes to the
   chunker (§8) in DTS mode with source time base `E`. Raw: send each frame
   to the chunker in PTS mode with source time base `T`.

`` `Flush `` (encoded) flushes the encoder, sending its packets to the
chunker. Raw mode does nothing on flush.

## 7. Video encoding

Inputs fixed at transform build:

| Name                  | Value                                                          |
| --------------------- | -------------------------------------------------------------- |
| input size, rate `F`  | frame settings width, height, `settings.frame.video.framerate` |
| input time base       | `1/F`                                                          |
| target size           | format `width` × `height`                                      |
| target rate `G`       | format `framerate`                                             |
| pixel aspect          | `1/1`                                                          |
| scaling flag, threads | [common.md §10](common.md#10-scaling)                          |

Target pixel format:

- Encoded: alpha is the format's `alpha` if set, else whether the input
  content type declares alpha. The pixel format is the format's
  `pixel_format` if given, else the codec's choice for that alpha
  ([common.md §7](common.md#7-pixel-formats)).
- Raw: the format's `pixel_format` if given, else `yuv420p`.

Build steps:

1. Create the scaler from the input size, in pixel format `yuva420p` if the
   target pixel format has alpha else `yuv420p`, to the target size and
   pixel format.
2. Create the frame-rate converter: the Fps helper graph
   ([filters.md §16.2](filters.md#162-fps)) from the target size and pixel format,
   time base `1/F`, pixel aspect `1/1`, target rate `G`, with no start pts
   and no source rate, so it is always a graph, even when `G = F`. Its output
   time base is `V`.
3. Encoded: resolve the hardware context and stream pixel format from
   `hwaccel`, `hwaccel_device`, `hwaccel_pixel_format`
   ([common.md §11](common.md#11-hardware-contexts)); create the encoder with
   that context, the copied options, frame rate `G/1`, the stream pixel
   format, the target size and time base `V`. Content parameters are
   `` `Video {avg_frame_rate = G/1, codec_params = encoder params} ``; chunk
   time base is the encoder's time base `E`.
4. Raw: content parameters `{width, height, pixel_format = target,
pixel_aspect = 1/1}`; chunk time base `V`.

Per input frame, for each image of the frame in order:

1. Render the image to a YUV420 picture, keeping transparency if the target
   pixel format has alpha.
2. Pack its planes (Y, U, V, and alpha when present) and scale them.
3. Set the scaled frame's pts to the image counter `n` (§9), then increment
   `n`.
4. Push the frame through the frame-rate converter. Each frame out goes to
   the encoder (encoded; packets go to the chunker in DTS mode, source time
   base `E`) or to the chunker (raw; PTS mode, source time base `V`).

`` `Flush `` (encoded) flushes the encoder into the chunker. The frame-rate
converter is not drained. Raw mode does nothing on flush.

## 8. Chunker

The chunker is the duration helper of [common.md §6](common.md#6-duration-helper), created with
source time base `S` (the chunk time base of §6 or §7), the mode given there
(PTS or DTS), `convert_ts` false and offset 0: timestamps are written back
unchanged, and each chunk's length is the timestamp distance to the **next**
item, in main ticks.

An emitted chunk becomes one content value `{params; chunks = [{length;
stream_idx; time_base = S; data}]}`, written into the buffer on the
operator's field ([content.md §4](content.md#4-two-chunk-layers)).

The last pending item is always held back. The chunker is never flushed: the
pending items at `` `Flush `` are not emitted.

Worked example. Defaults `44100` Hz audio, `25` Hz video, so main rate
`44100` and frames of 882 samples. AAC at `R = 48000`: packets carry 1024
samples, `E = 1/48000`, so each chunk is `1024 × 44100 / 48000 = 940.8`,
truncated to `940` ticks; the emitted length runs short of the encoded
duration by 0.085 %. Measured over a free-running clock (from the
implementation): after 12139 input frames the producer had delivered 12126
output frames (ratio 1.00107); with `R = 44100` the same run gives 13360
against 13357, a constant 3-frame delay and no drift.

## 9. Timestamps (inline part)

Rules for what the stream encoders and muxers do with these timestamps are
in [timestamps.md](timestamps.md). The inline encoders produce:

| Stream               | Where pts originate                                                                                | Unit      |
| -------------------- | -------------------------------------------------------------------------------------------------- | --------- |
| audio, before writer | none; the writer sets each frame's pts to the samples written before it (`T = 1/R`, so no scaling) | `T = 1/R` |
| audio packets        | the encoder, from those pts; the chunker reads **dts**                                             | `E`       |
| video, before fps    | image counter `n`, 0, 1, 2, …                                                                      | `1/F`     |
| video after fps      | the fps filter, after `PTS-max(STARTPTS,0)` and `start_time=0`                                     | `V`       |
| video packets        | the encoder; the chunker reads **dts**                                                             | `E`       |

- The counters start at 0 at each transform build and never reset
  otherwise: not at track boundaries, not on source changes upstream, not
  while the input is unavailable. A gap in input therefore produces no gap
  in timestamps; time only advances with data.
- No offset is applied (chunker offset 0) and timestamps are not converted
  to main ticks; chunks keep the encoder or filter time base.
- A transform rebuilt after sleep starts again at 0, usually with the same
  stream index (§5). Downstream, the copy encoder sees the same index with
  timestamps going back to 0: [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what), row "same
  source id, timestamps restart lower".
- Non-monotonic dts from the encoder raises `Content.Invalid` in the child
  clock (§8).

Decoders ignore packet and frame timestamps for audio: output length is the
number of decoded samples after resampling. For video, decoded frame pts in
the chunk's time base drive the internal video converter's frame-rate filter
([decoder-internal.md §2.2](decoder-internal.md#22-internal-video-converter)). Chunk lengths of the
input are not read by any decoder.

## 10. Audio decoding

Output: planar float at `settings.frame.audio.channels` channels (default
layout for that count) and `settings.frame.audio.samplerate`, put on the
operator's field. These come from the global settings, not from the
operator's output type.

### 10.1 Copy (`track.ffmpeg.decode.audio`)

State: the current decoder with its resampler, the current `(stream_idx,
time_base)`, and the parameters it was built from.

For each input frame, take the field's `ffmpeg.copy` content and its
content-level parameters `` `Audio p ``. For each chunk, sort its items by
position (stable), then for each packet:

1. Select the decoder:
   - none yet: build one from `p`;
   - current key differs from the chunk's `(stream_idx, time_base)`: flush
     the current decoder into its resampler, flush the resampler into the
     output, build a new one from `p`;
   - otherwise keep it. A change of parameters under the same key keeps the
     decoder.
2. Decode the packet; each decoded frame goes through the resampler; each
   result is put on the output.

Build from `p`: channel count and rate of `p`, default layout for that
count, decoder found by `p`'s codec id, created with `p`; the resampler's
input sample format is the decoder's.

`` `Flush ``: if a key exists, flush its decoder into the resampler, then
flush the resampler.

### 10.2 Raw (`track.ffmpeg.raw.decode.audio`)

State: the current resampler and its `(time_base, stream_idx)`.

For each input frame, take the `ffmpeg.audio.raw` content and its
parameters `{channel_layout, sample_rate, sample_format}`; each must be set.
For each chunk, sort by position, then for each frame:

1. Select the resampler: none yet, build from the parameters; key differs,
   flush the current one into the output and build; else keep.
2. Resample the frame and put the result on the output.

`` `Flush ``: flush the current resampler, if any.

## 11. Video decoding

Output: images at the global frame size and `settings.frame.video.framerate`,
produced by the internal video converter
([decoder-internal.md §2.2](decoder-internal.md#22-internal-video-converter)) with pixel format
`yuva420p` for frames whose format has alpha, `yuv420p` otherwise. The
converter keys its stage by `(stream_idx, time_base, frame format)` and
logs "Video format change." at level 3 when it rebuilds.

### 11.1 Copy (`track.ffmpeg.decode.video`)

A **stream** record holds the index, time base, codec (id, width, height,
pixel format from the parameters), decoder, current parameters, display
stage ([side-data.md §3](side-data.md#3-autorotate-on-decoding)) and a display-matrix source,
`decoder` or `stream m`.

For each chunk of the `ffmpeg.copy` content (content-level parameters
`` `Video p ``), sorted by position, for each packet:

1. Select the stream. Keep the current one when its index and time base
   equal the chunk's and either its parameters are physically the same
   object as `p` or its codec record equals `p`'s. Otherwise, if a current
   stream exists, log "Video frame format change detected.." at level 4 and
   flush it (below); then build a stream from `p`: decoder found by codec
   id and created with `p`, display stage built from `p` and the time
   base, matrix source `decoder`.
2. On a kept stream whose parameters object differs from `p`: if the
   parameters' side data differs, drain the display stage, build a new one
   from `p`, and set the matrix source to `stream m` with `m` the display
   matrix of `p`'s side data (possibly none). Then store `p`. The decoder is
   kept, because a decoder restarted inside a group of pictures has no
   reference picture.
3. Decode the packet. Each decoded frame: with matrix source `stream m`,
   duplicate the frame, remove its display matrix and add `m` when present;
   pass it through the display stage; each frame out goes to the converter
   with the stream's index and time base.

Flushing a stream: flush its decoder through step 3, then drain its display
stage into the converter. `` `Flush ``: flush the current stream, then flush
the converter.

### 11.2 Raw (`track.ffmpeg.raw.decode.video`)

A stream is `(stream_idx, time_base, display stage)`; the display stage is
built from the time base alone, so the frames' own side data drives it. For
each chunk, select the stream by `(stream_idx, time_base)` — on change drain
the old display stage — then pass each frame, sorted by position, through
the display stage into the converter. `` `Flush ``: drain the display stage,
flush the converter.

## 12. Track boundaries and metadata

- Track marks and metadata of the input are copied to the output at their
  input positions (§3.3). They are not aligned with the converted data.
- Nothing is reset at a track boundary: the encoder, decoder, resampler,
  scaler, frame-rate filter, timestamp counters and stream index carry on.
  Packets encoded from the end of one track can be emitted after the
  output's track mark.
- Decoders rebuild only on a change of `(stream_idx, time_base)` (audio,
  raw video) or of stream index, time base or codec (copy video), wherever
  it falls.
- `abort_track` on the producer clears its buffer and marks a track; the
  transform keeps its state.

## 13. Flush and end

The only flush is at producer sleep (§3.5): the transform gets `` `Flush ``
and is discarded.

| Transform        | On `` `Flush ``                                                     |
| ---------------- | ------------------------------------------------------------------- |
| encode audio     | flush the encoder into the chunker                                  |
| encode video     | flush the encoder into the chunker; fps filter not drained          |
| raw encode       | nothing                                                             |
| decode audio     | flush decoder → resampler, flush resampler                          |
| raw decode audio | flush resampler                                                     |
| decode video     | flush decoder → display → converter, drain display, flush converter |
| raw decode video | drain display, flush converter                                      |

What a flush emits lands in the buffer of the ending activation, which the
next wake-up replaces. No flush happens when the input ends or becomes
unavailable; data held in codecs, filters and the chunker stays there.

## 14. Clocks and latency

- The producer reads its input in a child clock (§3.1). Each producer frame
  ticks the child clock as many times as needed to fill a frame of output.
  Codec delay therefore shows up as bursts of child ticks, especially at
  start (encoder priming, lookahead) and after the frame-rate filter.
- Delay contributed, encoders: resampler or scaler, the audio frame writer's
  re-chunking, the fps filter (holds one frame), the encoder's own delay,
  and the chunker (always one item).
- The output's duration equals the sum of chunk lengths (encoders) or the
  decoded and converted media length (decoders). Truncation in the chunker
  (§8) makes an encoded stream run ahead of its input when packet durations
  are not whole numbers of main ticks.
- A sync source inside the input paces the child clock, not the producer's
  clock ([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)).

## 15. Failure behaviour

| Where       | What                                           | Outcome                                                             |
| ----------- | ---------------------------------------------- | ------------------------------------------------------------------- |
| evaluation  | §4 evaluation checks                           | script error with position                                          |
| first frame | §4 build checks, encoder/decoder creation      | source failure in the child clock                                   |
| any frame   | non-monotonic timestamp in the chunker         | `Content.Invalid`, logged at level 3, source failure in child clock |
| any frame   | buffer above `settings.clock.child.max_buffer` | runtime error kind `clock`                                          |
| any frame   | codec errors from the bindings                 | raised from the transform, source failure in the child clock        |
| raw decode  | raw parameter missing                          | exception from reading the absent value, source failure             |

A source failure in the child clock follows the clock's source error rule;
with no handler the child clock fails, then the reader's clock
([clock spec §11](../../../clock/spec/clock.md#11-failure)).

## 16. Settings read

| Key                                                                                                                                     | Default                              | Used by                              |
| --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------ | ------------------------------------ |
| `settings.clock.child.max_buffer`                                                                                                       | `10.`                                | producer buffer limit                |
| `settings.ffmpeg.scaling_algorithm`, `settings.ffmpeg.scaling_threads`, `settings.ffmpeg.autorotate`, `settings.ffmpeg.format_change.*` | [common.md §1](common.md#1-settings) | video encode scaler and video decode |
| `settings.frame.audio.samplerate`, `.channels`, `settings.frame.video.*`                                                                | —                                    | internal formats on both sides       |
