# Inline encoding and decoding operators

This file owns the script operators that turn a track into `ffmpeg.copy` or
`ffmpeg.raw` content inside a script and back: `track.ffmpeg.encode.*`,
`track.ffmpeg.raw.encode.*`, `track.ffmpeg.decode.*`,
`track.ffmpeg.raw.decode.*` and their source-level wrappers `ffmpeg.encode.*`,
`ffmpeg.raw.encode.*`, `ffmpeg.decode.*`, `ffmpeg.raw.decode.*`. It also owns
the **inline producer** (§3): the plumbing these operators share with the
bitstream filters, with the transform lifetime, the relay of metadata and
track marks, and the source ids inline encoders give their output (§5).

It relies on: the content model of `ffmpeg.copy` and `ffmpeg.raw`, source
identity and parameter compatibility ([content.md](content.md) §3, §5); the
`%ffmpeg` format and its parsed stream options
([encoder-format.md](encoder-format.md)); the stream-encoder pieces it reuses
— audio frame writing ([encoder-internal.md](encoder-internal.md)), hardware
contexts, pixel format choice and time-base conversion
([common.md](common.md)); the duration helper
([common.md §6](common.md#6-duration-helper)); draining
([draining.md](draining.md)); the encoder-side timestamp rules
([timestamps.md](timestamps.md)); the internal video converter and display
stage used by decoders
([decoder.md §16.1](decoder.md#161-internal-video-converter),
[side-data.md §3](side-data.md#3-autorotate-on-decoding)); child clocks
([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)); and the
FFmpeg bindings ([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)).
Part B is in [language-notes/inline-codecs.md](language-notes/inline-codecs.md).

## 1. Operators

The eight track operators exist only when Liquidsoap is built with FFmpeg.
Each takes `?id : string?` (default `null`) and returns a track. Category
`Track / Conversion`. Their types, as printed by the type checker:

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

Typing rules:

- Encoded mode types the format by its **input**: the format's `audio` (or
  `video`) field and the input track share `'a`. The format's channel count
  is therefore the input's channel count, and an encoded inline audio
  encoder never remixes channels.
- Raw mode types the format by its **output**: the input is any `pcm` /
  `yuv420p` track, and the output carries the raw format's parameters.
- The format's row `'b` is open: a format may describe other streams
  (`%video` next to `%audio`), which the operator ignores. One format value
  then serves both halves of an `audio_video` wrapper.
- The stream description is looked up in the format under the name `audio`
  (audio operators) or `video` (video operators), whatever the field name of
  the input track.
- Inline audio encoders take float PCM (`pcm`) input only. Accepting
  `pcm_s16` and `pcm_f32` input is a permitted extension; it changes no
  existing script.
- The output format of a decoder is the operator's resolved output type
  (§10, §11). An output type left unconstrained resolves to the frame
  defaults like any other track.

The `id` argument names the producer source: `<id>.producer`, with `<id>`
defaulting to `audio` or `video`. The generic source machinery makes source
ids unique (`audio.producer`, `audio.producer.1`, … in logs).

## 2. Source wrappers

Twelve script functions wrap the track operators. Each takes `~id` defaulting
to its own name, e.g. `ffmpeg.encode.audio(~id=null("ffmpeg.encode.audio"), f, s)`.
The encoders exist when `track.ffmpeg.raw.encode.audio` exists, the
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
**audio** track when there is one, the video track otherwise. Other fields of
`s` are not part of the result.

An `audio_video` wrapper is ready when both producers are. A source fault on
either producer ends the wrapper's track (§15), so a fault on the video half
is visible to the script even though metadata and track marks come from the
audio half.

In the `audio_video` forms both converted tracks come from the same input
`s`, so they carry the same metadata and track marks. Each converted track
places them on its own data (§3.3), so the two copies differ only by the
alignment tolerance of §3.3. The wrapper takes one copy, the audio one,
because a source has one metadata track and one track-mark track; the video
copy carries no information the audio copy lacks.

Examples of the printed types:

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

Every inline operator, and every bitstream filter
([bitstream-filters.md §4](bitstream-filters.md#4-structure)), is a
**producer**: a source that reads its input source in a child clock and
converts each input frame through a **transform**. The producer has the frame
type `{<field> = <output type>}`; the child is typed
`{<field> = <input type>}` with an open row.

A **transform** is the conversion state of one producer: codecs, resamplers,
scalers, frame-rate converters, display stages, the chunker (§8) and the
timestamp counters (§9). A transform accepts input frames. It can be
**drained** (told that no input follows, so that it delivers everything it
holds) or **discarded** (released with what it holds).

### 3.1 Construction

The producer is a child-clock reader as the clock spec defines it
([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)): at operator
evaluation it wraps its input in a child output named `<input id>.child`,
creates a passive child clock whose parent is the producer's clock, and
unifies that clock with the input's clock. It registers the child clock on
wake-up and deregisters it on sleep.

The producer accepts an input with a sync source (a hardware input,
`input.srt`): it does not apply the self-sync refusal of the clock spec. The
sync source paces the child clock's ticks; the producer's clock keeps its
own pacing. Inline conversion works at that pace, in bursts set by the codec
delay (§14).

**The input's clock is the child clock.** A source belongs to one clock. Any
other output reading the same input therefore runs in that child clock and is
animated only when a producer ticks it: its pace is the pace at which the
producer's consumer reads. This is intended behaviour, and the operators'
documentation MUST state it. A script that wants the other output to run at
its own pace puts a `buffer` between the input and one of the two readers.
The clock's start-up log names every source of the child clock, for example:

```
[clock.audio.producer:3] Starting nested clock, controlled by output.dummy.1,
  sync: passive, sources: sine (passive), output.dummy (output), sine.child (output)
```

Several producers reading the same input (the two halves of an
`audio_video` wrapper, several encoders of one source) share one child
clock: a tick by any of them fills the buffers of all
([clock spec §15, Shared child clocks](../../../clock/spec/clock.md#15-child-clocks)).

### 3.2 Transform lifetime

The producer holds at most one transform at a time.

**Build.** When the producer has no transform, the next input frame builds
one: from the operator's format (encoders) or from the first run's
parameters (decoders), and, for encoders, from the source-id record of §5.

**Life.** A transform lives across track boundaries of its input. A track
boundary is metadata about the stream, not a discontinuity of the media:
codec, resampler, scaler, frame-rate converter, chunker and timestamp
counters carry on. Rebuilding at each track boundary would cost every track
a codec priming gap and a source switch downstream, for no change in the
media. Metadata and track marks stay with their data (§3.3).

**End.** A transform ends at exactly these events:

| Event                                                                       | How it ends                                                                                                                                           |
| --------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| end of input: the producer is read while its input is not ready (§3.4)      | drained into the buffer (§13)                                                                                                                         |
| a decoder meets a new source id, time base or decoder parameters (§10, §11) | the affected stage is drained, then replaced (§13)                                                                                                    |
| the source-id record it uses is closed (encoders, §5)                       | as the event that closed the record                                                                                                                   |
| the producer seeks its input (§3.4)                                         | discarded ([draining.md §5](draining.md#5-discarded-on-purpose))                                                                                      |
| `abort_track` on the producer (§3.4)                                        | discarded: what it holds belongs to the aborted track                                                                                                 |
| the producer goes to sleep                                                  | discarded: the producer has no reader any more, and the clock spec clears its buffer ([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)) |
| a source fault of the producer (§15)                                        | drained when its stages can still drain, else discarded ([draining.md §5](draining.md#5-discarded-on-purpose))                                        |

The next input frame after an end builds a new transform. For an encoder,
the new transform belongs to a new source-id record unless a sibling
transform keeps the old record open (§5); downstream, a new record is a
source switch ([content.md §5](content.md#5-stream_idx-and-time_base)).

Conformance:

- The transform that converts the frames before a track boundary of its
  input also converts the frames after it: the output carries no source
  switch at a track boundary.
- Sleep, seek and `abort_track` release the transform; the next input frame
  builds a new one.

### 3.3 Metadata and track marks

Metadata and track marks of the input stay with the data they belong to.
Each input frame's metadata `(pos, m)` and track marks `pos` are relayed.
This includes those of a frame that carries no media (length 0), and a mark
at the frame's end position, which is the same boundary as the start of the
next frame and is relayed once.

**Input time.** Every relayed entry has an input time: its position in the
input stream, in main ticks, counted from an origin fixed when the transform
is built (for encoders, the timeline origin of §9). Every output item of the
transform (a packet, a raw frame, a block of decoded samples, a decoded
image) also has an input time: the input time of the media it was produced
from.

- Encoders: the item's presentation timestamp converted to main ticks with
  the time-base conversion of [common.md §5](common.md#5-time-base-conversion).
  Inline encoder timestamps count input media from the timeline origin (§9),
  so the conversion gives the input time.
- Decoders: the input time of the packet or raw frame the item was decoded
  from, matched by presentation timestamp. An item with no timestamp takes
  the input time of the last packet or frame sent to the decoder.

**Placement.** An entry is written into the buffer immediately before the
first output item, in output order, whose input time is at or after the
entry's input time: at the start of the chunk that carries that item.
Entries are written in input order. When a transform ends, every entry still
pending is written at the end of the buffer, after the drained output when
the transform is drained.

The binding constraint: an entry falls in the output between the items
derived from input before it and the items derived from input after it. With
a codec that reorders frames, items whose presentation order crosses the
entry may sit on either side of it; for every other codec the placement is
exact to one output item. In particular, a track mark at the end of a track
falls after the last packet encoded from that track and before the first
packet encoded from the next one, so an output that starts a file at a track
mark puts each packet in the file of its track.

Metadata written at the same buffer position as an existing entry is merged
into it.

Conformance:

- Inline-encode a sequence of two tracks to AAC and split the copy output on
  track marks: every packet whose presentation time lies in the first track
  is in the first file, and the first file ends with the packets of the last
  input samples of the first track.
- A metadata entry at input time `t` appears in the output at a position
  whose input time differs from `t` by less than one output item.
- A track mark carried by an input frame of length 0 reaches the output.

### 3.4 Reading

The producer is ready when its buffer is not empty, its input is ready, or
its transform has received input since it was built and has not been
drained.

To produce a frame:

1. When the input is not ready and a transform exists, drain the transform
   into the buffer (§13). This is the end of input of
   [draining.md](draining.md#2-the-rule).
2. While the input is ready and the buffer holds less than one frame (the
   minimum length over the buffer's media fields), tick the child clock.
3. Take up to one frame from the buffer.

When the input stops being ready the producer delivers what it has, then
ends its track.

On each child-clock tick where the input is ready, the child output pulls one
frame from the input, relays its metadata and track marks (§3.3), and passes
the frame to the transform, building the transform first if there is none.
When the buffer then holds more than `settings.clock.child.max_buffer`
(default `10.` seconds) of media, the producer's reader has fallen behind.
The limit protects memory against a consumer that stops reading one of
several producers sharing a child clock; it applies to a lone producer too.
Reaching it is a source fault of that producer
([faults.md §3](faults.md#3-source-faults-reach-the-script)): the producer
logs an error naming itself and its input, clears its buffer, ends its track
at that point, and goes on with the input's next frames. Nothing is raised:
the other readers of the child clock keep streaming. Example of the message
(wording not binding): "Operator <producer> has buffered more than <n>s of
data from <input>."

**Readiness during a source fault.** While a source fault persists (§15), a
fallible producer is not ready. It recovers as
[faults.md §3](faults.md#3-source-faults-reach-the-script) requires of a
producer whose input sits in a child clock: after `fault_retry_delay` it
reads its input again, discarding what the faulty source delivers, and
becomes ready on the first content it can convert (a new source id, new
parameters, a rebuilt transform). An infallible
producer cannot become not ready: its source fault is a hard error (§15).

Other source methods:

| Method        | Behaviour                                                                                                                                                                                                                   |
| ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fallible`    | The input's.                                                                                                                                                                                                                |
| `self_sync`   | The input's static/dynamic flag, with no sync source reported across the child clock.                                                                                                                                       |
| `remaining`   | Buffer remaining if known; else input remaining plus buffer length; `-1` if the input reports `-1`. Data held in the transform is not counted.                                                                              |
| `seek len`    | Drop `min(buffer length, len)` from the buffer and seek the input by the rest. When the input moves by a non-zero amount, the transform is discarded and, for an encoder, its source-id record closed (§5). Return the sum. |
| `abort_track` | Clear the buffer, discard the transform, close the source-id record (encoders, §5), add a track mark at the start of the buffer, abort the input's track.                                                                   |

### 3.5 Buffer lifetime

The buffer is created on each wake-up with the producer's content type and
installed into the child output. On sleep, the child clock is deregistered,
the input put to sleep, the transform discarded (§3.2) and the buffer
cleared. The next wake-up creates a new buffer.

## 4. Format validation (encoders)

The format argument is a `%ffmpeg` value. Every check below is a
configuration error ([faults.md §2](faults.md#2-classes)): it fails the
script before it streams, with the operator's script position.

| When       | Condition                                                                           | Error                                                                            |
| ---------- | ----------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| evaluation | top-level muxer options present, e.g. `%ffmpeg(foo=1, …)`                           | invalid value on the format: muxer options are not supported for inline encoders |
| evaluation | `format=` given                                                                     | invalid value: the format option is not supported for inline encoders            |
| typing     | stream is `%audio.copy` / `%video.copy`                                             | type error                                                                       |
| evaluation | encoded mode, stream is `%audio` / `%video` without `codec`                         | invalid value on the stream: inline encoders need a codec                        |
| evaluation | encoded mode, `codec` names no encoder FFmpeg has                                   | invalid value on the stream: encoder not found                                   |
| evaluation | raw mode, any stream option (§4.1)                                                  | invalid value on the stream: unrecognized options, naming each                   |
| start-up   | encoded mode, options left after the encoder is opened                              | invalid value: unrecognized options, naming each                                 |
| start-up   | the encoder cannot be opened with the format (sample format, size, hardware method) | invalid value on the stream, with FFmpeg's reason                                |
| start-up   | `settings.ffmpeg.scaling_algorithm` not one of its accepted names (video)           | the error of [common.md §10](common.md#10-scaling)                               |

Error wordings are examples, not requirements.

**Start-up check.** Some errors are known only once an encoder is opened.
When the operator's content type is final and before the producer's first
frame, the operator opens its encoder once with the format, the stream
options and the resolved input format, checks what is left of the options,
and closes it. A failure fails the script before it streams. For an operator
created while the script already streams, the same check runs before its
first frame; a failure there follows from the configuration and the host,
so it is a hard error whenever it shows
([faults.md §2](faults.md#2-classes)), naming the operator's position.

An implementation MAY run part of the check at evaluation, when the codec's
option table can be queried without opening the encoder.

Every build applies the stream's full option set: options consumed by one
build are not missing from the next.

Defaults of the stream description (channels `2`, samplerate
`settings.frame.audio.samplerate`, video size and rate from the frame
settings, `hwaccel` `auto`, …) are those of the format parser
([encoder-format.md §5](encoder-format.md#5-audio-tracks), [§6](encoder-format.md#6-video-tracks)).

### 4.1 Raw-mode options

Raw content carries only its format parameters (channel layout, sample
format and rate; size, pixel format and pixel aspect), and no codec consumes
it at this point, so a codec option in a raw stream has no effect anywhere.
In raw mode the stream's arguments are its media keys
([encoder-format.md §5](encoder-format.md#5-audio-tracks),
[§6](encoder-format.md#6-video-tracks)); every other labelled argument,
`codec=` included, is an error at evaluation, naming each one.
`%audio.raw(foo=12)` and `%audio.raw(codec="aac")` are rejected.

### 4.2 Output parameters

An encoded transform's content parameters are known only once its encoder is
open: codec parameters, extradata, and the packetisation form they imply.
Before the first chunk of a transform is written, the producer merges these
parameters into its output format
([content.md §3](content.md#3-parameters)):

- The first build sets the parameters every reader of the producer's output
  sees.
- A later build whose parameters fail the merge is a source fault of the
  producer ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged),
  [faults.md §3](faults.md#3-source-faults-reach-the-script)): it emits
  nothing for that source, logs both parameter sets at error level and ends
  its track; while the fault persists it is not ready (fallible input) or
  raises a hard error (infallible input) (§15). No packet of the rejected
  build reaches an output.
- With `settings.ffmpeg.content.copy.relaxed_compatibility_check` on,
  parameters of the same media type and codec id are compatible, and libav
  judges the rest.

Raw-mode parameters are set by the first frame, like those of every raw
producer ([content.md §3](content.md#3-parameters)): the producer merges them
into its output format before its first chunk, under the same rule.

Conformance:

- `%ffmpeg(%audio(b="128k"))` in `ffmpeg.encode.audio` fails at evaluation
  with the script position.
- `%ffmpeg(%audio.raw(foo=12))` in `ffmpeg.raw.encode.audio` fails at
  evaluation, naming `foo`.
- An encoded option unknown to the codec fails the script before its first
  frame, naming it.

## 5. Stream index

Inline encoders give every chunk they emit a source id
([content.md §5](content.md#5-stream_idx-and-time_base)). Encoders that read
the same input at the same time share one id: the audio and video encoders of
one source then form one source downstream, and a copy output switches their
tracks together ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)).
Several encoders of one source (three AAC bitrates of one `s`) share it too.

The producer side keeps, for each input source with a live inline encoder
transform, one **source-id record**:

- the id;
- the timeline origin: the input position at which the id was allocated;
- the input time elapsed since that origin, in main ticks;
- whether the record is open;
- the live transforms that use it.

Rules:

1. A transform build looks up the record of its input source by the
   source's identity (that source, not an equal one). When an open record
   exists, the transform takes its id and its timeline (§9). Otherwise a
   fresh id is allocated from the process-wide counter of
   [content.md §5](content.md#5-stream_idx-and-time_base), and a new open
   record with elapsed time 0 replaces any closed one.
2. The elapsed time advances by the input media each child-clock tick
   delivers. The input belongs to one child clock (§3.1), so every transform
   of the record sees the same elapsed time.
3. An input discontinuity closes the record: end of input (§3.4), a seek of
   the input (§3.4), `abort_track`. A closed record gives its id to no new
   transform. Each transform still using it ends at its producer's next
   step: drained at end of input, discarded at a seek or an abort.
4. A record is released when its last transform ends. A producer going to
   sleep ends only its own transform: while a sibling transform lives, the
   record stays open and the woken producer joins it.

Consequences:

- A record lives exactly as long as a transform uses it, and no state is
  kept for an input source with no live inline encoder: the lifetime rule of
  [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state)
  applied on the producer side.
- Whether an encoder rebuilt after a sleep, a seek or an end of input gets a
  new id follows from these rules alone. A rebuild with no sibling alive gets
  a new id, so downstream it is a source switch, and timestamps stay
  monotonic across it ([timestamps.md](timestamps.md)).
- Two transforms under one id never restart timestamps below each other:
  they share one timeline (§9).

Decoders do not allocate ids; they read the id of each run (§10, §11).
Bitstream filters keep the incoming run's id
([bitstream-filters.md §5](bitstream-filters.md#5-packets-through-the-filter)).

Conformance:

- `ffmpeg.encode.audio_video` produces audio and video chunks with one id.
- Put an inline encoder to sleep and wake it, with no other encoder on its
  input: the chunks after the wake-up carry a new id, every time.
- Same, with a second encoder on the same input that stays awake: the woken
  encoder's chunks carry the second encoder's id, and their timestamps are
  above every timestamp the woken encoder emitted before its sleep.
- After a seek through one producer, every producer on that input emits a
  new id.

## 6. Audio encoding

Fixed at transform build:

| Name                 | Value                                       |
| -------------------- | ------------------------------------------- |
| input layout         | the input content format's channel layout   |
| input rate           | `settings.frame.audio.samplerate`           |
| target layout        | default layout for the format's `channels`  |
| target rate `R`      | format `samplerate`                         |
| target time base `T` | `1/R`                                       |
| target sample format | format `sample_format` if given, else `dbl` |

Encoded mode:

1. Find the encoder by name; replace the sample format by the codec's best
   match for it.
2. Create the encoder with the stream options, target layout, sample format,
   rate and time base `T`. Its parameters become the content parameters
   `` `Audio params `` (§4.2); its time base `E` is the chunk time base.
3. Frame size: none if the codec has the variable-frame-size capability,
   else the encoder's frame size.

Raw mode sets the content parameters to `{channel_layout = target layout,
sample_format = target sample format, sample_rate = R}`, frame size none,
chunk time base `T`.

Per input frame:

1. Resample the frame's samples from planar float at the input layout and
   rate to the target layout, rate and sample format.
2. Pass the result to the audio frame writer of the stream encoder
   ([encoder-internal.md §5.1](encoder-internal.md#51-audio),
   [timestamps.md §3](timestamps.md#3-audio-internal-and-raw)), with time
   base `T`, rate `R`, the frame size and the starting sample count of §9:
   the writer re-chunks to the frame size when there is one and sets each
   frame's pts.
3. Encoded: send each frame to the encoder; each packet goes to the chunker
   (§8) in DTS mode with source time base `E`. Raw: send each frame to the
   chunker in PTS mode with source time base `T`.

Drain: §13.

## 7. Video encoding

Fixed at transform build:

| Name                  | Value                                                             |
| --------------------- | ----------------------------------------------------------------- |
| input size, rate `F`  | the input content format's size, `settings.frame.video.framerate` |
| input time base       | `1/F`                                                             |
| target size           | format `width` × `height`                                         |
| target rate `G`       | format `framerate`                                                |
| pixel aspect          | `1/1`                                                             |
| scaling flag, threads | [common.md §10](common.md#10-scaling)                             |

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
2. Create the frame-rate converter from `F` to `G`: the Fps helper
   ([filters.md §16.2](filters.md#162-fps)) with the target size and pixel
   format, time base `1/F`, pixel aspect `1/1`, and a start pts equal to the
   first image number of §9, so that its output keeps the transform's
   timeline. Its output time base is `V`.
3. Encoded: resolve the hardware context and stream pixel format from
   `hwaccel`, `hwaccel_device`, `hwaccel_pixel_format`
   ([common.md §11](common.md#11-hardware-contexts)); create the encoder with
   that context, the stream options, frame rate `G/1`, the stream pixel
   format, the target size and time base `V`. Content parameters are
   `` `Video {avg_frame_rate = G/1, codec_params = encoder params} `` (§4.2);
   the chunk time base is the encoder's time base `E`.
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

**Global headers.** An inline encoder has no muxer to ask whether it needs
global headers. The codec writes its headers out of band only when the
stream options request it (`flags="+global_header"`). The resulting codec
parameters (extradata present or not, and the packetisation form it implies)
are part of the content parameters (§4.2), so a copy output whose muxer needs
another form sees the difference at the compatibility check
([content.md §3.1](content.md#31-ffmpegcopy)). The documented ways to adapt
are the `flags` option and `ffmpeg.filter.bitstream.extract_extradata`.

Drain: §13.

## 8. Chunker

The chunker turns the transform's packets (encoded) or frames (raw) into
content runs. It is the duration helper of
[common.md §6](common.md#6-duration-helper), created with source time base
`S` (the chunk time base of §6 or §7), the mode given there (PTS or DTS),
timestamps written back unconverted and offset 0. Each run's length comes
from the timestamp step to the next item, in main ticks.

Lengths derive from absolute timestamps
([common.md §6.0](common.md#60-absolute-origin)): the end of each run is the
next item's timestamp minus the transform's first timestamp, converted to
main ticks with `rescale` ([common.md §5](common.md#5-time-base-conversion)),
and its length is that end minus the previous end. The sum of the lengths of
a transform's runs therefore equals the span of its timestamps converted
once, and never drifts from the media duration.

Each emitted batch becomes one content value `{params; chunks = [{length;
stream_idx; time_base = S; data}]}`, written into the buffer on the
operator's field ([content.md §4](content.md#4-two-chunk-layers)), with
`stream_idx` the transform's source id (§5).

The helper holds the last item until the next one arrives. When the
transform is drained (§13), the chunker is flushed after the encoder or
frame-rate converter, and its held items are emitted with the length of
[common.md §6.3](common.md#63-flush-t). When the transform is discarded, the
chunker is dropped unflushed
([common.md §6.5](common.md#65-draining-and-discarding)).

Worked example. Defaults `44100` Hz audio, `25` Hz video, so main rate
`44100` and frames of 882 samples. AAC at `R = 48000`: packets carry 1024
samples, `E = 1/48000`, so each packet spans `1024 × 44100 / 48000 = 940.8`
ticks. The end of the run of packet `k` is `rescale(1024 k, 1/48000, 1/44100)`,
the nearest integer to `940.8 k`: ends 941, 1882, 2822, 3763, 4704, so run
lengths 941, 941, 940, 941, 941, repeating every five packets for 4704
ticks, exactly five packets' duration. The producer's output length equals
its input length up to a constant codec delay, and over a free-running clock
the input and output frame counts keep a constant difference.

Conformance:

- Encode AAC at 48 kHz inline for one hour on a free-running clock: the
  difference between input and output frame counts stays constant after
  start-up.

## 9. Timestamps (inline part)

What the stream encoders and muxers do with these timestamps is in
[timestamps.md](timestamps.md). The inline encoders produce:

| Stream               | Where pts originate                                                                      | Unit      |
| -------------------- | ---------------------------------------------------------------------------------------- | --------- |
| audio, before writer | the writer's sample count: the samples written before the frame, from the starting count | `T = 1/R` |
| audio packets        | the encoder, from those pts; the chunker reads **dts**                                   | `E`       |
| video, before fps    | image counter `n`, from the starting count, 1 per image                                  | `1/F`     |
| video after fps      | the frame-rate converter, on the `G` grid of the same timeline                           | `V`       |
| video packets        | the encoder; the chunker reads **dts**                                                   | `E`       |

**Timeline.** Every transform of one source-id record (§5) shares one
timeline: time 0 is the record's origin. A transform built when the record
opens starts its counters at 0. A transform that joins an open record starts
its counters at the record's elapsed time, converted from main ticks to the
counter's unit with [common.md §5](common.md#5-time-base-conversion): the
audio sample count at `elapsed × R / main rate`, the image counter at
`elapsed × F / main rate`.

- Counters never reset inside a transform: not at track boundaries, not on
  source changes upstream.
- A transform whose input keeps delivering has no gap in its timestamps; an
  input that stops delivering ends the transform (§3.4).
- No offset is applied and timestamps are not converted to main ticks;
  chunks keep the encoder or converter time base.
- A new record starts a new timeline under a new id. Downstream, that is a
  source switch, and the encoder keeps DTS strictly increasing per output
  stream across it ([timestamps.md](timestamps.md)). Under one id, the
  timestamps of one producer's output never go back.
- A packet from the inline encoder whose DTS goes back is an item fault: the
  chunker drops it with a warning
  ([common.md §6.2](common.md#62-push-t-item)). The layer's own encoder
  produces one only on a defect, so it is not repaired. `max_consecutive_faults`
  such drops in a row escalate to a source fault (§15).

Decoders ignore packet and frame timestamps for audio length: output length
is the number of decoded samples after resampling. Input timestamps serve the
placement of metadata (§3.3). For video, decoded frame pts in the run's time
base drive the internal video converter's frame-rate conversion
([decoder.md §16.1](decoder.md#161-internal-video-converter)).
No decoder reads the run lengths of its input.

## 10. Audio decoding

Output: planar float in the operator's resolved output format, the channel
layout of its `pcm` type, at `settings.frame.audio.samplerate`, put on the
operator's field. The output format is read from the operator's content type
at the first build, never from the frame defaults directly, so the data
always matches its type.

Conformance: with `settings.frame.audio.channels := 1`,
`(ffmpeg.decode.audio(e) : source(audio=pcm(stereo)))` delivers stereo
content.

### 10.1 Copy (`track.ffmpeg.decode.audio`)

State: the current decoder with its resampler, and the key it was built for:
the run's `(stream_idx, time_base)` and the decoder parameters.

**Decoder parameters** are the codec parameters a decoder is created from,
compared as [content.md §3.1](content.md#31-ffmpegcopy) compares them with
the relaxed setting off, plus extradata. They are the parameters of the run
being decoded. One content value never mixes runs whose parameters differ
(content.md owns that guarantee), so the content-level parameters are the
run's.

For each input frame, take the field's `ffmpeg.copy` content. For each run,
take its items in position order (stable), then for each packet:

1. Select the decoder:
   - none yet: build one from the parameters;
   - the key differs: drain the current decoder through its resampler into
     the output (§13), then build a new one. When the id and time base are
     the same and only the parameters differ, log a warning naming both
     parameter sets: under one id this happens only with the relaxed
     compatibility setting or a defective producer;
   - otherwise keep it.

   When no decoder exists for the parameters, or its creation fails, the
   source is undecodable: a source fault (§15).

2. Decode the packet; each decoded frame goes through the resampler; each
   result is put on the output. A packet the decoder rejects is an item
   fault, dropped with a warning (§15).

Build: decoder found by the codec id and created with the parameters; the
resampler converts from the decoder's sample format, layout and rate to the
output format.

Conformance:

- Copy content whose codec changes under one source id is decoded with a
  decoder for the new codec from the first packet of the change, and the
  change is logged.

### 10.2 Raw (`track.ffmpeg.raw.decode.audio`)

State: the current resampler and its key: `(stream_idx, time_base)` and the
raw parameters `{channel_layout, sample_rate, sample_format}`.

Raw content carries all three parameters
([content.md §3.2](content.md#32-ffmpegaudioraw)). A run with one of them
missing is invalid input: an item fault, the run is dropped with a warning
naming the missing parameter (§15).

For each run, in position order, for each frame:

1. Select the resampler: none yet, build from the parameters; key differs,
   drain the current one into the output and build; else keep.
2. Resample the frame and put the result on the output.

## 11. Video decoding

Output: images in the operator's resolved output format (the size of its
video type) at `settings.frame.video.framerate`, produced by the internal
video converter
([decoder.md §16.1](decoder.md#161-internal-video-converter))
with pixel format `yuva420p` for frames whose format has alpha, `yuv420p`
otherwise. The converter keys its stage by `(stream_idx, time_base, frame
format)`, decides a format change with the one comparison of
[common.md §9](common.md#9-video-format-comparison), drains its stage before
rebuilding it, and handles a mid-stream format change without a track mark.

### 11.1 Copy (`track.ffmpeg.decode.video`)

A **stream** record holds the id, time base, decoder, current parameters,
display stage ([side-data.md §3](side-data.md#3-autorotate-on-decoding)) and
a display-matrix source, `decoder` or `stream m`.

For each run of the `ffmpeg.copy` content (parameters `` `Video p ``), in
position order, for each packet:

1. Select the stream. Keep the current one when its id and time base equal
   the run's and its decoder parameters (as §10.1) equal `p`'s. Otherwise
   drain the current stream, if any (below), logging a warning when only the
   decoder parameters changed (as §10.1); then build a stream from `p`:
   decoder found by codec id and created with `p`, display stage built from
   `p` and the time base, matrix source `decoder`.
2. On a kept stream whose parameters differ from `p` in side data only:
   drain the display stage, build a new one from `p`, and set the matrix
   source to `stream m` with `m` the display matrix of `p`'s side data
   (possibly none). The decoder is kept: a decoder restarted inside a group
   of pictures has no reference picture. Store `p`.
3. Decode the packet. Each decoded frame: with matrix source `stream m`,
   duplicate the frame, remove its display matrix and add `m` when present;
   pass it through the display stage; each frame out goes to the converter
   with the stream's id and time base.

When no decoder exists for `p`, or its creation fails, the source is
undecodable: a source fault (§15). A packet the decoder rejects is an item
fault, dropped with a warning; the decoder is kept.

A newly built decoder SHOULD receive packets from the first keyframe on:
packets before it cannot be decoded without the references they need, and a
decoder fed them produces damaged pictures. The skipped packets are counted
and logged at warning level. The wait is bounded by `max_keyframe_wait`,
measured in input time, with the recommended value of encoder-copy's
parameter of the same name
([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)):
when no keyframe arrives within it, the wait is an input-side wait that
failed ([faults.md §5](faults.md#5-waiting)), a source fault (§15).

Draining a stream: drain its decoder through step 3, then drain its display
stage into the converter.

### 11.2 Raw (`track.ffmpeg.raw.decode.video`)

A stream is `(stream_idx, time_base, display stage)`; the display stage is
built from the time base alone, so the frames' own side data drives it. For
each run, select the stream by `(stream_idx, time_base)`, draining the old
display stage on a change, then pass each frame, in position order, through
the display stage into the converter.

## 12. Track boundaries and metadata

- Metadata and track marks of the input reach the output at the position of
  the data they belong to (§3.3).
- A track boundary resets nothing: the encoder, decoder, resampler, scaler,
  frame-rate converter, timestamp counters and source id carry on (§3.2).
- Decoders rebuild a stage only on a change of id, time base or decoder
  parameters (§10, §11), wherever it falls, and drain the old stage first.
- `abort_track` on the producer clears its buffer, discards the transform
  and marks a track (§3.4).

## 13. Flush and end

Each transform drains as [draining.md](draining.md) requires. Draining a
transform drains its stages upstream first, each stage's output entering the
next before the next is drained, and ends with the chunker (encoders) or the
output (decoders). Drained output is ordinary output: it lands in the buffer
the producer's consumer reads, before the metadata and track marks still
pending (§3.3), and it obeys §8 and §9.

| Transform        | Drain order                                                                                                                |
| ---------------- | -------------------------------------------------------------------------------------------------------------------------- |
| encode audio     | resampler → frame writer (final partial frame per [draining.md §4](draining.md#4-partial-final-units)) → encoder → chunker |
| encode video     | frame-rate converter → encoder → chunker                                                                                   |
| raw encode audio | resampler → frame writer → chunker                                                                                         |
| raw encode video | frame-rate converter → chunker                                                                                             |
| decode audio     | decoder → resampler → output                                                                                               |
| raw decode audio | resampler → output                                                                                                         |
| decode video     | decoder → display stage → converter → output                                                                               |
| raw decode video | display stage → converter → output                                                                                         |

A transform is drained (§3.2):

- **At end of input.** The producer is read while its input is not ready:
  the transform is drained into the buffer, the producer delivers the
  buffer, then ends its track. This is the case of a finite input, and the
  one in which the encoder tail reaches a file.
- **When a decoder stage is replaced.** On a new id, time base or decoder
  parameters, the old stage is drained into the output before the first
  output of its replacement.

A transform is discarded at a seek, an `abort_track`, a sleep or a failure
(§3.2). Its pending metadata and track marks are still written (§3.3).

A drain that fails part-way is an item fault: what it could not deliver is
dropped with a warning, the transform is released and streaming goes on.

Conformance:

- One second of sine encoded inline to AAC and muxed by a copy output gives
  as many packets as the same sine encoded by a `%ffmpeg(%audio(codec="aac"))`
  output.
- A finite video encoded inline delivers its last input image.
- Inline-decode a finite copy stream: the decoded duration equals the
  stream's duration to within the codec priming.

## 14. Clocks and latency

- The producer reads its input in a child clock (§3.1). Each producer frame
  ticks the child clock as many times as needed to fill a frame of output.
  Codec delay therefore shows up as bursts of child ticks, especially at
  start (encoder priming, lookahead) and after the frame-rate converter.
- Delay contributed by encoders: resampler or scaler, the audio frame
  writer's re-chunking, the frame-rate converter, the encoder's own delay,
  and the chunker (one item). By decoders: the decoder's delay, the
  resampler, the display stage and the converter.
- The output's duration equals the sum of run lengths (encoders) or the
  decoded and converted media length (decoders). Over any stretch it equals
  the input's duration up to the constant delay above (§8).
- A sync source inside the input paces the child clock, not the producer's
  clock ([clock spec §15](../../../clock/spec/clock.md#15-child-clocks)).
- Other readers of the input run at the producer's pace (§3.1).

## 15. Failure behaviour

Every fault is placed in a class of [faults.md §2](faults.md#2-classes).
Nothing is raised out of the producer while it streams, except the hard error
of an infallible producer below.

| Where      | What                                                                                                   | Class               | Outcome                                                                         |
| ---------- | ------------------------------------------------------------------------------------------------------ | ------------------- | ------------------------------------------------------------------------------- |
| evaluation | §4 evaluation checks                                                                                   | configuration error | script error with position                                                      |
| start-up   | §4 start-up check                                                                                      | configuration error | script fails before streaming                                                   |
| build      | encoder parameters fail the output format's merge (§4.2)                                               | source fault        | nothing emitted for that source; both sets logged at error level; below         |
| any frame  | DTS going back from an inline encoder (§9)                                                             | item fault          | packet dropped with a warning; bound `max_consecutive_faults`                   |
| decode     | the decoder rejects a packet (§10.1, §11.1)                                                            | item fault          | packet dropped with a warning; bound `max_consecutive_faults`                   |
| raw decode | raw parameter missing on a run (§10.2)                                                                 | item fault          | run dropped with a warning naming the parameter; bound `max_consecutive_faults` |
| any frame  | an encode, resample, scale or filter call rejects a frame                                              | item fault          | frame dropped with a warning; bound `max_consecutive_faults`                    |
| drain      | a drain fails part-way (§13)                                                                           | item fault          | the rest of the drain dropped with a warning                                    |
| decode     | no decoder for a source's parameters, or its creation fails                                            | source fault        | below                                                                           |
| decode     | no keyframe within `max_keyframe_wait` (§11.1)                                                         | source fault        | below                                                                           |
| any frame  | an item-fault bound reached                                                                            | source fault        | below                                                                           |
| rebuild    | a rebuild fails for a reason that follows from configuration and host (codec, hardware method, format) | configuration error | hard error whenever it shows                                                    |
| rebuild    | a rebuild fails for a reason that depends on the input's data                                          | source fault        | below                                                                           |
| any frame  | buffer above `settings.clock.child.max_buffer` (§3.4)                                                  | source fault        | buffer cleared, track ended, error log; the input goes on                       |

**Item faults** ([faults.md §4](faults.md#4-item-faults-escalate)) are
counted per producer against the one bound `max_consecutive_faults` of
faults.md §4. The count is reset by an item converted successfully. Logging
follows faults.md §4 (`fault_log_interval`).

**Source faults** ([faults.md §3](faults.md#3-source-faults-reach-the-script)):

1. The producer ends its track at the fault. Data already converted is
   delivered, and the transform is drained when its stages can still drain,
   else discarded (§3.2). An encoder's source-id record is closed (§5), so
   the next transform is a new source downstream.
2. While the fault persists (the input still delivers the source that cannot
   be converted), a producer whose input is fallible is not ready (§3.4). A
   producer is fallible exactly when its input is (§3.4), for every operator
   of this file.
3. A producer whose input is infallible is infallible and cannot become not
   ready: a source fault that persists past the end of track is a hard error.
   The producer raises, and the script fails with an error naming the
   producer and the cause
   ([clock spec §11](../../../clock/spec/clock.md#11-failure)). The producer
   never stands silence, black or a repeated frame in for media it cannot
   convert; a script that wants filler puts `mksafe`, `fallback` or `blank`
   around a fallible input.
4. A not-ready producer retries after `fault_retry_delay`
   ([faults.md §3](faults.md#3-source-faults-reach-the-script)): it reads its
   input again (encoders rebuild their transform) and becomes ready, with a
   new track, on the first content it can convert.

Each source fault is logged at error level, naming the producer, its input
and the cause. The inline operators have no callbacks of their own; the
script sees the fault through readiness and track ends. The documentation of
every operator of this file states that with an infallible input a
persistent data fault is a hard error, and names `mksafe` and `fallback` as
the script's ways to keep streaming through it.

## 16. Settings read

| Key                                                                                                                                     | Default                              | Used by                              |
| --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------ | ------------------------------------ |
| `settings.clock.child.max_buffer`                                                                                                       | `10.`                                | producer buffer limit (§3.4)         |
| `settings.ffmpeg.content.copy.relaxed_compatibility_check`                                                                              | `false`                              | output parameters (§4.2)             |
| `settings.ffmpeg.scaling_algorithm`, `settings.ffmpeg.scaling_threads`, `settings.ffmpeg.autorotate`, `settings.ffmpeg.format_change.*` | [common.md §1](common.md#1-settings) | video encode scaler and video decode |
| `settings.frame.audio.samplerate`, `settings.frame.video.framerate`                                                                     | —                                    | internal rates on both sides         |
