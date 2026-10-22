# Side data

This file owns what Liquidsoap does with FFmpeg side data: where it is carried
with content, the autorotate rule applied when video is decoded, the
`ffmpeg.autorotate.on_undecided` builtin, the two track operators
`track.ffmpeg.side_data` and `track.ffmpeg.raw.side_data`, and which side data
an encoder is given. It relies on the bindings' side-data interface
(`src/modules/synced/ffmpeg/spec/side-data.md`, its display converter in
`avfilter.md` §4.13, §11.4, §11.5, the display matrix in `avutil.md` §4.19),
on the content model of [content.md](content.md), and on the settings of
[common.md, Settings](common.md#1-settings).

## 1. Where side data is carried

| Content       | Stream-wide side data                                      | Per-item side data   |
| ------------- | ---------------------------------------------------------- | -------------------- |
| `ffmpeg.copy` | the coded side data of the content's codec parameters      | each packet's        |
| `ffmpeg.raw`  | the entries of each frame whose kind has the `Global` prop | each frame's entries |
| internal      | none                                                       | none                 |

- Copy content holds one codec-parameters value (or none) per content value
  and a list of packets ([content.md](content.md)). Its stream-wide side data
  is the coded side data of those parameters ([avcodec.md §4.11](../../../../modules/synced/ffmpeg/spec/avcodec.md#411-side-data)).
- Raw content holds `AVFrame`s. A frame's stream-wide side data is the subset
  of its entries whose kind's descriptor carries the `Global` property
  ([avutil.md §4.20](../../../../modules/synced/ffmpeg/spec/avutil.md#420-frame-side-data)), in the
  frame's order. This subset is called **global side data** below (§7).
- Internal content (PCM, YUV420 images) carries no side data. What must be
  acted on is acted on when frames become internal content (§3).

## 2. Kinds, names and payloads

- The two families have their own kind enumerations. A kind is named by
  FFmpeg's name for it: `av_packet_side_data_name` for copy content,
  `av_frame_side_data_name` for raw content. The display matrix is
  `Display Matrix` on a copy track and `3x3 displaymatrix` on a raw track
  (§9, from the implementation). These are the names `ffprobe` prints and the
  names the `remove` argument of §5 and §6 matches.
- A payload is a byte string in FFmpeg's in-memory layout. Liquidsoap reads
  no payload itself except the display matrix, which it reads and writes
  through the binding's typed view (36 bytes: nine `int32` in native byte
  order, `avutil.md` §4.19).
- Rotations visible to scripts through §5 and §6 are in degrees
  **counter-clockwise**, as `Display_matrix.rotation` returns them, in
  [-180, 180]. Rotations reported by §4 are **clockwise**, in [0, 360), as
  the binding's `` `Odd_rotation `` carries them.
- A script cannot build an entry from arbitrary bytes. The only entry a
  script can create is a display matrix, from an angle.

## 3. Autorotate on decoding

Setting: `settings.ffmpeg.autorotate`, `bool`, default `true`
([common.md, Settings](common.md#1-settings)).

Every place that turns decoded video frames into content (internal or raw)
goes through one display stage. Which decoders create one and when is
described in [decoder.md](decoder.md) and the decoder files; this section
owns what the stage does.

### 3.1 Creating a display stage

A display stage is created with the decoded stream's codec parameters
(optional) and the time base of the frames' timestamps.

1. When `settings.ffmpeg.autorotate` is `false` at creation, the stage is
   empty: every frame is delivered as it is, display matrix included.
2. Otherwise the stage is a binding display converter
   ([avfilter.md §11.5](../../../../modules/synced/ffmpeg/spec/avfilter.md#115-display-converter)) created
   with:
   - `cropping`: the cropping entry of the codec parameters' coded side data
     ([avcodec.md §4.11](../../../../modules/synced/ffmpeg/spec/avcodec.md#411-side-data)), when the parameters are given and hold
     one;
   - `on_undecided`: the handler installed by §4 **at the time of creation**,
     or the binding's default when none is installed;
   - `ignore`: the frame properties left out by `settings.ffmpeg.format_change`
     at the time of creation ([common.md, Video format comparison](common.md#9-video-format-comparison));
   - `time_base`: as given.

The display matrix is not read from the codec parameters: the converter reads
it from each frame. FFmpeg's decoders copy the stream's display matrix, and
EXIF orientation of images, onto each frame.

### 3.2 Converting

- `convert stage frame deliver`: an empty stage calls `deliver frame`. A
  converter stage hands the frame to `convert_display`; frames are delivered
  in order, upright, cropped, without their display matrix, as
  `avfilter.md` §11.5 defines. Timestamps are unchanged.
- `eof stage deliver`: an empty stage does nothing. A converter stage flushes
  its graph and delivers the frames it held; the stage stays usable.
- The stage has no lock. One decoder uses it from one thread.

### 3.3 Geometry before the first frame

Decoders size content before any frame is decoded. Two readers give the
geometry the display stage will produce, from the codec parameters only:

- **Stored layout**: width and height of the parameters, pixel aspect of the
  parameters unless a `stored` pixel aspect is given, no filter.
- **Displayed layout**: when `settings.ffmpeg.autorotate` is `false`, the
  stored layout. Otherwise the binding's display layout
  ([avfilter.md §11.4](../../../../modules/synced/ffmpeg/spec/avfilter.md#114-display-filter-chain-and-layout)) on the stored width, height and pixel aspect, with the parameters'
  cropping and the display matrix of the parameters' coded side data. When
  that returns `` `Undecided ``, the stored layout.

| Reader                        | Returns                                  |
| ----------------------------- | ---------------------------------------- |
| `expected_size params`        | (width, height) of the displayed layout  |
| `pixel_aspect ?stored params` | the pixel aspect of the displayed layout |

A quarter turn swaps width and height and inverts the pixel aspect. Example:
a 1920×1080 stream with a 90° matrix has an expected size of 1080×1920. The
size is a hint: the frames delivered by §3.2 are authoritative, and what a
consumer does when they differ is [decoder.md](decoder.md)'s.

### 3.4 Undecided cases

The binding reports three cases it does not decide: a rotation that is not a
quarter turn (`` `Odd_rotation `` with the clockwise angle), a cropping that
leaves no picture or has a negative field (`` `Invalid_cropping ``), and a
hardware frame with a matrix or cropping to apply (`` `Hardware_frame ``).
For each run of frames with the same question, the converter calls the
stage's `on_undecided` once:

- a list of filters: the frames go through them and lose their display
  matrix;
- no decision: the frames are delivered as stored, display matrix included.

The binding's default handler logs one line at FFmpeg warning level through
FFmpeg's log, and decides nothing. With log capture on, it reaches the
Liquidsoap log under the label `ffmpeg` at `settings.ffmpeg.log.level`
([common.md, Log bridge](common.md#2-log-bridge)), for instance
`[ffmpeg:3] Video left as stored, with its display matrix: the rotation of 327 degrees is not a quarter turn.`
(from the implementation, a file stored with a counter-clockwise rotation of
33°).

A chain answered by a handler that fails to build is logged by the binding
and the frames are delivered as stored (`avfilter.md` §11.5 step 5).

## 4. `ffmpeg.autorotate.on_undecided`

```
ffmpeg.autorotate.on_undecided :
  (({cropping : {bottom : int, left : int, right : int, top : int}?,
     reason : string, rotation : float?}) -> [string * [string * string]]?)
  -> unit
```

(type from `liquidsoap -h`; category `Liquidsoap`; the module
`ffmpeg.autorotate` exists to hold it.)

- Calling it installs the function process-wide, replacing any function
  installed before. It returns `()`. It is never uninstalled.
- Display stages read the installed function when they are created (§3.1):
  it applies to stages created afterwards, never to existing ones.
- The function receives one record per question:

  | `reason`             | `rotation`                                  | `cropping`                  |
  | -------------------- | ------------------------------------------- | --------------------------- |
  | `"odd_rotation"`     | the clockwise angle in degrees, in [0, 360) | `null`                      |
  | `"invalid_cropping"` | `null`                                      | the four borders, in pixels |
  | `"hardware_frame"`   | `null`                                      | `null`                      |

- It returns `null` for no decision, or a list of filters. A filter is
  `(name, [(option, value)])`; each option is passed to the filter as a
  string argument `option=value`. An empty list means "nothing to apply":
  the frames are delivered without their display matrix.
- The function runs on the thread that decodes, inside the conversion of the
  frame that raised the question.

Example (from the implementation): a file stored with
`-display_rotation 33` (counter-clockwise) asks once with
`reason="odd_rotation"`, `rotation=327.0`.

## 5. `track.ffmpeg.side_data`

```
track.ffmpeg.side_data :
  (?id : string?, ?rotation : {float?}, ?remove : [string],
   ffmpeg.copy('a)) -> ffmpeg.copy('a)
```

Methods: `rotation : () -> float?`, `side_data : () -> [{data : string, kind : string}]`.

| Argument   | Default | Meaning                                                                        |
| ---------- | ------- | ------------------------------------------------------------------------------ |
| `id`       | `null`  | track operator id                                                              |
| `rotation` | `null`  | getter; a non-null value sets the stream's display rotation, counter-clockwise |
| `remove`   | `[]`    | packet-family kind names to drop from the stream-wide side data                |

The operator is a track operator in category `Track`: it returns the same
field of a new source. The source is a pass-through of its input: readiness,
fallibility, remaining time, track abort, effective source and self-sync are
the input's.

### 5.1 Per frame

For each frame produced by the input:

1. Read the copy content of the field. When it has no codec parameters, the
   frame is passed unchanged and the stored entries of §5.3 keep their
   previous value.
2. Read the `rotation` getter once: `r`.
3. If the previous frame's input parameters are the same value (physical
   identity) and its `r` was equal, reuse the previous result. Otherwise
   compute it (§5.2) and remember (input parameters, `r`, result).
4. Store the result's entry list for §5.3.
5. When the result's parameters are the input's (identity), pass the frame
   unchanged. Otherwise replace the field's content by the same content with
   the result's parameters.

The cache of step 3 gives an unchanged stream one parameters value.
`track.ffmpeg.decode.video` keeps its decoder when the parameters are that
same value or carry an equal codec record
([inline-codecs.md §11.1](inline-codecs.md#111-copy-trackffmpegdecodevideo)); copy encoders compare stream
indices only ([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)).

### 5.2 The edit

Let `E` be the coded side data of the parameters, in order.

1. Drop every entry of `E` whose kind name is in `remove`.
2. Video only: when the `rotation` getter, read again, is non-null `a`, drop
   every display-matrix entry and append one display matrix encoding a pure
   counter-clockwise rotation by `a` degrees ([avutil.md §4.19](../../../../modules/synced/ffmpeg/spec/avutil.md#419-display-matrix-and-cropping)).
   Audio and subtitle parameters get step 1 only.
3. When the resulting list equals `E` (structural equality of kinds and
   payloads), the result's parameters are the input parameters themselves.
   Otherwise they are a functional update of the input parameters with the
   new list ([avcodec.md §4.11](../../../../modules/synced/ffmpeg/spec/avcodec.md#411-side-data)).

Per-packet side data is not edited. Removal happens before the rotation is
set, so `remove=["Display Matrix"]` with a rotation yields the new matrix.

### 5.3 Methods

- `side_data()`: the stored entry list (§5.1 step 4), each as
  `{kind = <FFmpeg name>, data = <payload>}`. `[]` before the first frame
  with parameters.
- `rotation()`: `Display_matrix.rotation` of the first display-matrix entry
  of the stored list that decodes; `null` when there is none or the matrix is
  singular.

## 6. `track.ffmpeg.raw.side_data`

```
track.ffmpeg.raw.side_data :
  (?id : string?, ?rotation : {float?}, ?remove : [string],
   ffmpeg.video.raw('a)) -> ffmpeg.video.raw('a)
```

Same arguments, methods and pass-through behaviour as §5, on a raw video
track. `remove` matches frame-family kind names.

### 6.1 Per frame

1. Read the `rotation` getter once: `r`. Every frame of this Liquidsoap
   frame uses `r`.
2. When `r` is `null` and `remove` is `[]`, the field is passed unchanged.
3. Otherwise, every AVFrame of every chunk of the field is replaced by an
   edited copy, positions kept:
   1. duplicate the frame, because one frame is shared by every
      consumer of the track;
   2. remove every entry whose kind name is in `remove` (all entries of that
      kind);
   3. when `r` is non-null, attach a display matrix for a counter-clockwise
      rotation by `r` (replacing an existing one).
4. Remember the last AVFrame of the field (edited or not), in chunk and
   position order.

The edit applies to every entry of the frame, global or not.

### 6.2 Methods

- `side_data()`: the global side data (§7) of the remembered frame, named and
  shaped as in §5.3. `[]` before the first frame.
- `rotation()`: the rotation of the first display-matrix entry of that list,
  `null` when none.

With `settings.ffmpeg.autorotate` on, decoded raw frames have lost their
display matrix (§3), so `rotation()` reads `null` unless a rotation is set
by this operator.

## 7. Side data given to encoders

- **Global side data** of a frame: its entries whose kind has the `Global`
  prop, in order. One reader computes it for §6.2 and for encoders.
- When a video encoder for raw content creates its stream from the first
  Liquidsoap frame, it is given the global side data of the first AVFrame of
  the first chunk that has one. The stream carries it into the output
  container. How and when the stream is created is
  [encoder-internal.md](encoder-internal.md)'s.
- Global side data that changes after the stream exists is not applied.
- Copy encoding writes the codec parameters it is given, coded side data
  included, and each packet's side data ([encoder-copy.md](encoder-copy.md)).
- Streams encoded from internal content and raw audio streams are given no
  side data. Inline encoders (`ffmpeg.encode.*`, [inline-codecs.md](inline-codecs.md))
  are given none either.

## 8. Elsewhere

- Frames enter and leave `ffmpeg.filter.*` graphs with whatever side data
  the filters keep. Liquidsoap adds and removes nothing there
  ([filters.md](filters.md)).
- Decoders that rebuild a display stage when the stream format changes, and
  the frame fitting used when frame sizes change, are described in the
  decoder files and [filters.md](filters.md).
- The script library (`src/libs/*.liq`) defines nothing about side data or
  autorotate.

## 9. Worked examples

All from the implementation, on a 320×240 MPEG-4 file whose stream was
given `-display_rotation 90` by the `ffmpeg` tool (ffprobe: `Display Matrix`,
`rotation=90`).

| Script                                                                                | Result                                                                          |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- |
| copy track, `v = track.ffmpeg.side_data(video)`                                       | `v.side_data()` kinds `["Display Matrix"]`, `v.rotation()` `90.0`               |
| `track.ffmpeg.side_data(rotation=33., remove=["Display Matrix"], v)`, copied to mp4   | `rotation()` `32.9997143765`; ffprobe of the output: `rotation=32`              |
| raw track, `settings.ffmpeg.autorotate := false`, `track.ffmpeg.raw.side_data(video)` | kinds `["3x3 displaymatrix"]`, `rotation()` `90.0`                              |
| the same, then `track.ffmpeg.raw.side_data(rotation=180., …)`, encoded from raw       | `rotation()` `-180.0`; ffprobe of the output: `Display Matrix`, `rotation=-180` |
| raw track, autorotate on (default)                                                    | kinds `[]`, `rotation()` `null`                                                 |
