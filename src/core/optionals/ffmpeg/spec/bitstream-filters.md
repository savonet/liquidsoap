# FFmpeg bitstream filters

This file owns the `ffmpeg.filter.bitstream.*` operators: one per FFmpeg
bitstream filter and media kind, applied to encoded (`ffmpeg.copy`) content.
It relies on the bitstream filter bindings (`src/modules/synced/ffmpeg/spec/avcodec.md`,
`BitstreamFilter`), copy content ([content.md](content.md)), the argument
mapping of [filters.md §5](filters.md#5-arguments-from-avoptions), child clocks ([clock.md §15](../../../clock/spec/clock.md#15-child-clocks)) and the
inline producer with its metadata relay ([inline-codecs.md §3](inline-codecs.md#3-the-inline-producer)).

## 1. Which operators

At start-up (timed as "FFmpeg bitstream filters registration"), each
bitstream filter of the bindings' list gives operators according to the codecs
it declares:

| Declared codecs                                 | Modes        | Operators                                                                      |
| ----------------------------------------------- | ------------ | ------------------------------------------------------------------------------ |
| none, or at least one audio and one video codec | audio, video | `ffmpeg.filter.bitstream.<name>.audio`, `ffmpeg.filter.bitstream.<name>.video` |
| audio codecs only                               | audio        | `ffmpeg.filter.bitstream.<name>`                                               |
| video codecs only                               | video        | `ffmpeg.filter.bitstream.<name>`                                               |
| neither audio nor video codecs                  | none         | no operator; logs at important `No valid mode found for filter <name>!`        |

A codec counts as audio or video when it is in the bindings' audio or video
codec identifier lists. From the implementation, the build used here logs
the last case for `eia608_to_smpte436m`, `lcevc_metadata`, `pgs_frame_merge`
and `smpte436m_to_eia608`, and makes e.g. `ffmpeg.filter.bitstream.h264_mp4toannexb`,
`ffmpeg.filter.bitstream.aac_adtstoasc`, `ffmpeg.filter.bitstream.null.audio`
and `ffmpeg.filter.bitstream.null.video`.

Category `Source / FFmpeg filter`, flag `extra`, description `FFmpeg <path>
bitstream filter. See ffmpeg documentation for more details.`, where
`<path>` is `<name>` for a single mode and `<name>.audio` / `<name>.video`
otherwise.

## 2. Signature

Arguments: the generic `id` of operators, one optional argument per option
of the filter (types, documentation and validation as
[filters.md, Arguments from AVOptions](filters.md#5-arguments-from-avoptions)), and the source,
unlabelled. The source's type has the filtered field (`audio` or `video`) of
copy content with default parameters and any other fields; the result has
the same type.

Examples (from the implementation, `liquidsoap -h`):

```
ffmpeg.filter.bitstream.h264_mp4toannexb :
  (?id : string?, source(video=ffmpeg.copy, 'a)) -> source(video=ffmpeg.copy, 'a)

ffmpeg.filter.bitstream.noise.audio :
  (?id : string?, ?amount : string?, ?drop : string?, ?dropamount : int?,
   source(audio=ffmpeg.copy, 'a)) -> source(audio=ffmpeg.copy, 'a)
```

Composition: the operator inherits composition from its effective source.

## 3. Options

The rendered argument pairs ([filters.md, Validation and rendering](filters.md#54-validation-and-rendering))
keep only ground values: an array option logs at important `Invalid bitstream
filter option!` and is dropped. The pairs become an option dictionary, a
later pair of the same name replacing an earlier one, passed to the bindings'
filter initialisation.

## 4. Structure

The operator is a producer over a child clock
([clock.md §15](../../../clock/spec/clock.md#15-child-clocks)): its source is animated in a
child clock of the operator's clock, without the self-sync check, and every
tick of that clock processes the source's frame into the operator's buffer.
The operator reads its buffer, ticking the child clock while the source is
ready and the buffer holds less than a frame. Self-sync, readiness, seek,
remaining and abort follow the producer ([clock.md §15](../../../clock/spec/clock.md#15-child-clocks)).

Processing one frame of the source:

1. Relay the frame's metadata, and its track marks strictly before the frame
   size, to the buffer ([inline-codecs.md §3.3](inline-codecs.md#33-metadata-and-track-marks)).
2. For each field of the frame other than metadata and track marks:
   - the filtered field: process its copy content (§5);
   - any other field: put verbatim into the buffer.

Processing `Flush` (when the operator goes to sleep): flush the current
filter (§6) and forget it.

## 5. Packets through the filter

The operator holds at most one **handler**: `{stream index; filter; output
parameters; time base; duration converter}`.

For each chunk of the content, and each packet of the chunk, in order:

1. Get the handler for the chunk's stream index and time base, given the
   content's codec parameters:
   - no handler: initialise the bitstream filter with the options and the
     parameters (the bindings return the filter and its output parameters),
     with a new duration converter, keyed by this stream index and time base;
   - a handler of another stream index: flush it (§6), then make a new one as
     above;
   - a handler of the same stream index: keep it, whatever the time base.
2. Send the packet to the filter.
3. Receive packets until the filter answers "again" or "end of stream". Each
   received packet goes through the duration converter; each batch it
   releases is put into the filtered field as one chunk `{length; stream
index of the handler; time base of the handler; packets}` with the
   handler's **output** parameters (video parameters carry no average frame
   rate).

The content's parameters are read with "must be present": content with
chunks and no parameters fails.

**Duration converter**: the duration helper of [common.md §6](common.md#6-duration-helper), in
DTS mode, with no time-base conversion and offset 0. The filtered field therefore lags the other fields by the packets
the converter holds. What the encoder makes of these timestamps is in
[timestamps.md](timestamps.md).

## 6. Flush

Flushing a handler:

1. Send end of stream to the filter.
2. Receive and put packets as §5, step 3.
3. Put the converter's remaining packets as one chunk whose length is the
   largest duration among them, in main ticks, at least 1.

Step 3 puts a chunk even when the converter holds nothing: a chunk of length
1 with no packet.

## 7. Failure

| Situation                                                | Result                                                |
| -------------------------------------------------------- | ----------------------------------------------------- |
| option value invalid                                     | error 7 at script time ([filters.md](filters.md))     |
| array option                                             | logged, dropped                                       |
| filter initialisation fails                              | the bindings' error, raised in the child clock's tick |
| send or receive fails other than "again"/"end of stream" | the bindings' error, raised in the tick               |
| content without parameters                               | failure in the tick                                   |
| non-monotonic DTS                                        | log important, content-invalid exception              |
