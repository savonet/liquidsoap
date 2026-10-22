# The `%ffmpeg` encoder: container and stream-encoder contract

This file owns the lifecycle of one `%ffmpeg` encoder instance: how the
container is opened, how its streams are created and fed, how metadata,
flushing, HLS segmenting and closing work, how options are checked, and the
contract every stream encoder meets (§12). It relies on the format record of
[encoder-format.md](encoder-format.md), on the output container operations of
the bindings ([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
and on the generic encoder interface of Liquidsoap (stated in §2 as far as this
encoder relies on it). Stream implementations are in
[encoder-internal.md](encoder-internal.md) and
[encoder-copy.md](encoder-copy.md); every timestamp rule is in
[timestamps.md](timestamps.md).

## 1. Entry points and call order

The generic encoder layer asks every registered encoder plug-in whether it
accepts a format; the `ffmpeg` plug-in accepts exactly the `%ffmpeg` formats.
Accepting returns a factory:

```
factory : ?hls:bool -> pos:position option -> name:string -> metadata -> encoder
```

`hls` defaults to `false`; `name` is unused. Calling the factory builds the
instance: §3 runs once, then `encode_metadata initial_metadata` (§6).

The returned `encoder` has:

| Operation           | Called by                                  |
| ------------------- | ------------------------------------------ |
| `encode frame`      | every output, once per (sub-)frame of data |
| `encode_metadata m` | outputs, when a frame carries metadata     |
| `header ()`         | Harbor, at listener connect                |
| `stop ()`           | outputs, when they stop or reopen          |
| `hls.*`             | the HLS output only (§8)                   |

The generic layer wraps every operation of the instance, `hls.*` included, in
one mutex per instance: two operations of the same instance never run at the
same time.

## 2. What the encoder relies on from callers

- Frames passed to `encode` are the output's frames or sub-frames; the pipe,
  file and URL outputs pass only frames of positive length. Every stream
  encoder also ignores a frame of length 0 (§12.3), except for stream creation
  (§5).
- Metadata is passed before the data of the same sub-frame (the outputs' default
  "metadata first" order).
- The content of every field the format names is present in each frame with
  the content type of [encoder-format.md §10.2](encoder-format.md#102-at-instantiation-time-from-the-record).

## 3. Building an instance

In order:

1. **Stream defaults.** Every encoded track's codec options are copied, and
   `flags` is set to `"+global_header"` unless the track already has a `flags`
   option. A `q`/`qscale` track has one ([encoder-format.md §8](encoder-format.md#8-q-and-qscale)),
   so it does not get `+global_header` from here. Reason (code comment): global
   headers let the stream's encoded content be passed to the muxer and its
   properties be inferred.
2. **HLS defaults**, only when `hls` is true:
   - `format="mp4"`: container option `movflags` defaults to
     `"+frag_custom+dash+delay_moov"` (fragments are cut only when the
     encoder asks, §8.3).
   - `interleaved=Default` becomes `False`; an explicit value is kept.
   - every encoded and copied stream gets a keyframe hook (§8.2).
3. **Stream registry.** A per-source tracking store is created for the copy
   tracks, sized by the number of copy tracks in the format
   ([encoder-copy.md §12](encoder-copy.md#12-source-switching-and-shared-per-source-state)). It lives as long as the instance,
   across container restarts (§6).
4. **Container** (`make`, also run again by §6):
   1. Container options = a copy of the format's `opts`. Two keys are
      converted: `sample_fmt` from a name to FFmpeg's sample-format id (an
      unknown name raises FFmpeg's error), `channel_layout` from a name to
      FFmpeg's canonical description. A non-string value for either is an
      assertion failure.
   2. The muxer is guessed from `format` (short name), the `Url` target as
      file name, and `mime_type`. None found is allowed for `Url` (FFmpeg
      guesses from the URL).
   3. `interleaved`: `Default` → true when the format lists more than one
      track (drop tracks counted); `True`/`False` as written. Reason (history,
      #4401): a single-stream output has nothing to interleave, and an
      interleaving muxer buffers packets, which left the first HLS segment
      empty.
   4. Open:
      - `Stream` output: no muxer found raises `Format is required!` when
        `format` is none, else
        `No ffmpeg format could be found for format="<f>"` (kind `encoder`,
        at the format's position). The output
        writes into an in-memory buffer through a write callback that appends
        and returns the length written. A seek callback over that buffer is
        given only for HLS with `format="mp4"`.
      - `Url u` output: the bindings open `u` themselves, with the guessed
        muxer if any. Nothing is written to the buffer.
   5. Streams: every non-drop track builds its stream encoder, in track order
      (§12). An encoded track with no `codec` raises
      `Codec unspecified for %ffmpeg stream %<field>!`. Stream encoders are
      kept by field name, so a field named twice keeps only the last track's
      stream encoder. The first track's audio, subtitle or copy stream has
      already been added or reserved in the container at its build; it stays
      there and is never fed, and `interleaved` (step 3) and the copy
      `total_count` ([encoder-copy.md §12.1](encoder-copy.md#121-model)) count both
      tracks.
   6. Container options left unconsumed by the open raise
      `Unrecognized options: <k=v,…>` (kind `encoder`). This check runs after
      §4.5, so a stream's unknown option is reported first.
   7. The handler holds: the muxer, the output container, the stream encoders
      by field, `started` = false, an empty queue of actions pending the
      container header, and an `insert_id3` that returns none.
5. **Initial metadata**: `encode_metadata` with the metadata given to the
   factory (§6).

Every failure raises out of the factory; the output decides what follows
(retry, stop, error callback).

## 4. Option handling

There are two option sets, checked separately:

- **Container options** (format `opts`, plus the HLS and output defaults of
  §3 and §10): consumed by the bindings' open (generic container options, muxer
  private options, then the I/O protocol). Leftovers raise §3.4.6.
- **Stream options** (track `opts`, plus `flags`): consumed by the stream
  encoder's creation. Each stream encoder compares the options left after
  creation with its input; a leftover key raises
  `Unrecognized options: <k=v,…>` (kind `encoder`, at the format's position).
  Copy streams take no options.

The check is per instance and per container restart. A format whose options
are all valid but unusable together fails at FFmpeg's open with FFmpeg's error.

Example (from the implementation): `%ffmpeg(format="adts", foo=3,
%audio(codec="aac", bar="x"))` fails with `Unrecognized options: bar="x"`;
without `bar` it fails with `Unrecognized options: foo=3`. In both cases the
target file is left created and empty.

## 5. Encoding a frame

`encode frame`:

1. On the first call of the container's lifetime (`started` switches to true),
   every stream encoder's `mk_stream frame` runs. This is where stream
   encoders that need to see content create their muxer stream (video,
   copy); others did it at §3.4.5. This step runs even for a frame of length 0.
2. Every stream encoder's `encode frame` runs, in field iteration order. Field
   iteration order is the **reverse** of the track order in the format: with
   `%ffmpeg(%audio.copy, %video.copy)`, video is fed first (from the
   implementation; it matters for [encoder-copy.md §12](encoder-copy.md#12-source-switching-and-shared-per-source-state)).
3. When the container header has been written and actions are pending, they
   run in queue order (§8.5).
4. The bytes the buffer holds are taken and returned (empty for a `Url`
   output).

The container header is written by the bindings on the first packet or frame
written to any stream. Exceptions from a stream encoder or from the muxer
propagate out of `encode`; the instance is then in whatever state the failure
left it.

## 6. Metadata

`encode_metadata m`:

1. The entries of `m` are added onto the format's own `metadata`: on a
   shared key the value from `m` wins, and the format's other entries are
   kept.
2. If the container header is not written yet: the list replaces the
   container's metadata.
3. Else if the output is `Stream` and `format="ogg"`: the container is
   restarted — every stream encoder's `flush` (§12.8), the container is
   closed (its trailer goes to the buffer), a new container is built by
   §3.4 (new muxer state, new stream encoders, `started` false), and the
   metadata is set on it. Reason (history, f4dee266a): Ogg carries in-stream
   metadata by chaining a new logical bitstream, which a reader decodes as a
   new track.
4. Else: nothing happens; the metadata is dropped.

The bytes produced by the restart stay in the buffer and are returned by the
next `encode` or `stop`. What a restart does to timestamps is in
[timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).

## 7. Stop

`stop ()`: every stream encoder's `flush`, then the container is closed
(exceptions from the close are ignored), then the buffer is returned. Closing
flushes every audio and video encoder and writes the trailer
([avformat.md §2.1 Release](../../../../modules/synced/ffmpeg/spec/avformat.md#21-container--input-container-output-container)).
A container that never received a packet is closed without header or trailer.

`header ()` returns empty bytes: a listener joining a running stream receives
no container header from this encoder (see §10).

## 8. HLS

Only with `hls=true`. The HLS output calls `init`, then `init_encode` until it
returns an init segment or the encoder has data, then `split_encode` for every
later frame.

### 8.1 `init ?id3_enabled ?id3_version ()`

By the muxer's name:

| Muxer                        | `id3_enabled` | Result                                                                                                                        |
| ---------------------------- | ------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `mpegts`                     | not `false`   | a timed-ID3 data stream is added (§8.5); returns `true`                                                                       |
| `mpegts`                     | `false`       | returns `false`                                                                                                               |
| `adts`, `mp3`, `ac3`, `eac3` | `false`       | `Format requires ID3 metadata!`                                                                                               |
| `adts`, `mp3`, `ac3`, `eac3` | otherwise     | `insert_id3` returns a raw ID3 tag to prepend to the segment (the generic HLS ID3 builder, version default 3); returns `true` |
| other                        | `true`        | `Format does not support HLS ID3 metadata!`                                                                                   |
| other                        | not `true`    | returns `false`                                                                                                               |
| none (no muxer)              | any           | `Format is required!`                                                                                                         |

### 8.2 Keyframe hooks

Each track has a flag "has seen a keyframe since the last split", false at
start. Each encoded and copied stream gets a hook, called when that stream is
about to give a key packet to the muxer; the stream first flushes the muxer
(unless its codec is intra-only), then calls the hook, so the buffer holds
exactly what precedes the key packet. The hook:

1. calls the current split callback (§8.4);
2. then sets this track's flag.

Subtitle tracks get no hook.

### 8.3 `init_encode frame`

| `format` | Steps                                                                                                                                                                                                                                         |
| -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `mp4`    | encode the frame (§5), flush the muxer, read the I/O position `p`. If `p > 0`: init segment = buffer bytes `[0, p)`, data = the rest prefixed with the `styp` box. Else raise "not enough data" (the HLS output retries with the next frame). |
| `webm`   | flush the muxer, init segment = the buffer so far, then encode the frame; data = the buffer                                                                                                                                                   |
| other    | encode the frame; no init segment; data = the buffer                                                                                                                                                                                          |

The `styp` box (24 bytes, from the implementation):
`00 00 00 18 's' 't' 'y' 'p' 'm' 's' 'd' 'h' 00 00 00 00 'm' 's' 'd' 'h' 'm' 's' 'i' 'x'`
— size 24, major brand `msdh`, minor version 0, compatible brands `msdh`, `msix`.
It starts every mp4 media segment.

### 8.4 `split_encode frame`

1. If every track that has a stream encoder has its flag set: `flushed` =
   the buffer as it is before this frame.
2. Else a split callback is installed: when a hook runs (§8.2) and every such
   flag is set and nothing was captured yet, it captures the buffer into
   `flushed`.
3. The frame is encoded (§5). The callback is removed afterwards, also on
   exception.
4. `encoded` = what the buffer holds now.
5. With `flushed`: every flag is cleared; result
   `Ok (flushed, encoded)`, `encoded` prefixed with `styp` for HLS mp4. The
   HLS output appends `flushed` to the current segment and may start the next
   segment with `encoded`.
6. Without: result `Nope encoded`; everything stays in the current segment.

Because the hook sets its own track's flag after calling the callback, the
callback never sees the flag of the track whose keyframe is arriving. A split
is therefore taken at the start of the frame **after** the frame in which the
last keyframe arrived, not before the key packet. Observed (from the
implementation, `%ffmpeg(format="mpegts", %audio(codec="aac"),
%video(codec="libx264", g=25, …))`, 2 s segments): every segment after the
first starts two packets after a key packet, and the key packet is the last
packet of the previous segment ([findings](findings/encoder.md)).

A subtitle track has a stream encoder and no hook (§8.2), so its flag is never
set: a format with a subtitle track never satisfies step 1 or step 2, and
`split_encode` always returns `Nope`.

### 8.5 Timed ID3 in mpegts

`init` adds a data stream of codec `timed_id3`, time base `1/audio_rate`.
`insert_id3 ~frame_position ~sample_position m` builds an ID3v2 tag (version
default 3), makes a packet with pts = dts as given by
[timestamps.md §8](timestamps.md#8-timed-id3-hls-mpegts), and writes it to that stream; before the
container header it is queued and written by §5.3. It returns none (the tag is
in the stream, not prepended).

### 8.6 Properties

| Query            | Answer                                                                         |
| ---------------- | ------------------------------------------------------------------------------ |
| `codec_attrs ()` | the stream encoders' codec strings joined by `,`, in field order; none if none |
| `bitrate ()`     | the sum of the bit rates the streams report; none if none                      |
| `video_size ()`  | the single stream that reports a size; none with zero or several               |

When `codec_attrs` gives none, the HLS output falls back to
[encoder-format.md §12](encoder-format.md#12-file-extension-and-mime-type).

## 9. Outputs with a URL or file target

| Output        | Effect on the format                                                                                                                                                                    |
| ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `output.file` | `output = Url "file:<path>"`, container option `truncate` = 0 when `append=true`, else 1. The output itself creates the file with its permissions and closes it; the encoder writes it. |

With a `Url` target the encoder returns empty bytes; the output writes nothing
itself. `output.file` with `reopen_*` builds a new instance per file.
`output.url` also sets a `Url` target; its format check, errors and restarts
are in [io.md §1.4](io.md#14-outputurl) and [io.md §11](io.md#11-outputurl).

## 10. Icecast and Harbor

- MIME type, the mp3 container options and the Icecast stream info of a
  `%ffmpeg` format are in [io.md §12](io.md#12-other-outputs-special-casing-ffmpeg).
- Harbor sends `header ()` to each new listener; it is empty here.
  `dedicated_encoder=true` builds one instance per listener, so each gets the
  stream from its own container header.

## 11. Failure behaviour summary

| Situation                                   | Behaviour                        |
| ------------------------------------------- | -------------------------------- |
| factory failure (§3, §4)                    | raises; output-specific handling |
| muxer or stream error in `encode`           | raises out of `encode`           |
| error in `stop`'s close                     | ignored                          |
| metadata after the header, non-ogg or `Url` | dropped silently                 |

## 12. Stream-encoder contract

Each non-drop track has one stream encoder. Its implementations follow this
numbering: [encoder-internal.md](encoder-internal.md),
[encoder-copy.md](encoder-copy.md). The inline encoders
([inline-codecs.md](inline-codecs.md)) do not implement this contract; they
reuse the format record, the audio frame writer of
[encoder-internal.md §5.1](encoder-internal.md#51-audio), and the internal resampler and
scaler.

### 12.1 Interface

A stream encoder is built from `(field, track mode, options, keyframe hook,
output container)` and provides:

| Operation         | Meaning                                                                           |
| ----------------- | --------------------------------------------------------------------------------- |
| `mk_stream frame` | called once, with the container's first frame (§5.1); may create the muxer stream |
| `encode frame`    | consume the field's content of the frame                                          |
| `flush ()`        | push out what the encoder holds before a close                                    |
| `codec_attr ()`   | RFC 6381 codec string, or none                                                    |
| `bitrate ()`      | bit rate, or none                                                                 |
| `video_size ()`   | (width, height), or none                                                          |

### 12.2 Stream creation

When the muxer stream is added (at build, or at `mk_stream`), and from what.
All streams must exist before the first packet: the header is written then.

### 12.3 Input

What the stream reads from the frame field; frames of length 0 are ignored.

### 12.4 Conversion

What is done to the content before the muxer.

### 12.5 Timestamps

Owned by [timestamps.md](timestamps.md); each implementation links its section.

### 12.6 Writing

Which bindings write call, with which time base.

### 12.7 Keyframes

When the keyframe hook (§8.2) is called.

### 12.8 Flush and end

What `flush` pushes; what is lost at close.

### 12.9 Properties

What `codec_attr`, `bitrate`, `video_size` report and from when.

### 12.10 Options and errors

Which options it consumes; every error it raises.

### 12.11 Logs

Every message, by level.
