# Side data

This file owns what Liquidsoap does with FFmpeg side data: where it is carried
with content (§1), the autorotate rule applied when video is decoded (§3),
the `ffmpeg.autorotate.on_undecided` builtin (§4), the two track operators
`track.ffmpeg.side_data` and `track.ffmpeg.raw.side_data` (§5, §6), and which
side data an encoder is given (§7). It relies on the bindings' side-data
interface (the bindings' `side-data.md`, the display converter of their
`avfilter.md` §11.4–§11.5, the display matrix of their `avutil.md` §4.19), on
the content model of [content.md](content.md), and on the settings of
[common.md, Settings](common.md#1-settings).

## 1. Where side data is carried

| Content       | Stream-wide side data                                          | Per-item side data   |
| ------------- | -------------------------------------------------------------- | -------------------- |
| `ffmpeg.copy` | the coded side data of each run's source parameters            | each packet's        |
| raw           | the entries of each frame whose kind has the `Global` property | each frame's entries |
| internal      | none                                                           | none                 |

- Copy content carries the source parameters of each run
  ([content.md §4.2](content.md#42-run-layer)). The stream-wide side data of
  a run is the coded side data of those parameters
  ([avcodec.md §4.11](../../../../modules/synced/ffmpeg/spec/avcodec.md#411-side-data)).
  Header side data kinds take part in copy compatibility
  ([content.md §3.1](content.md#31-ffmpegcopy)).
- Raw content holds frames. A frame's stream-wide side data is the subset of
  its entries whose kind's descriptor carries the `Global` property
  ([avutil.md §4.20](../../../../modules/synced/ffmpeg/spec/avutil.md#420-frame-side-data)),
  in the frame's order. This subset is called **global side data** below.
- Internal content (PCM, YUV420 images) carries no side data. What must be
  acted on is acted on when frames become internal content (§3).

## 2. Kinds, names and payloads

- The two families have their own kind enumerations. A kind is named by
  FFmpeg's name for it: the packet side-data name for copy content, the
  frame side-data name for raw content. The display matrix is
  `Display Matrix` on a copy track and `3x3 displaymatrix` on a raw track.
  These are the names `ffprobe` prints, and the names the `remove` argument
  of §5 and §6 matches.
- A payload is a byte string in FFmpeg's in-memory layout. Liquidsoap reads
  no payload itself except the display matrix, which it reads and writes
  through the bindings' typed view (nine 32-bit integers in native byte
  order).
- Rotations that §5 and §6 take and return are in degrees
  **counter-clockwise**, in [-180, 180], as the bindings' display-matrix
  rotation returns them. Rotations that §4 reports are **clockwise**, in
  [0, 360), as the bindings' undecided-rotation case carries them. Each
  builtin documents its convention.
- A script cannot build an entry from arbitrary bytes. The only entry a
  script can create is a display matrix, from an angle.

## 3. Autorotate on decoding

Setting: `settings.ffmpeg.autorotate`, `bool`, default `true`
([common.md, Settings](common.md#1-settings)).

Every place that turns decoded video frames into content (internal or raw)
goes through one display stage. Which decoders create one and when is
specified in [decoder.md](decoder.md) and the decoder files; this section
owns what the stage does.

### 3.1 Creating a display stage

A display stage is created with the decoded stream's codec parameters
(optional) and the time base of the frames' timestamps.

1. When `settings.ffmpeg.autorotate` is `false` at creation, the stage is
   empty: every frame is delivered as it is, display matrix included.
2. Otherwise the stage is a bindings display converter
   ([avfilter.md §11.5](../../../../modules/synced/ffmpeg/spec/avfilter.md#115-display-converter))
   created with:
   - `cropping`: the cropping entry of the codec parameters' coded side data,
     when the parameters are given and hold one;
   - `on_undecided`: the handler installed by §4 at the time of creation,
     wrapped as §3.4 states, or the bindings' default when none is
     installed;
   - `ignore`: the frame properties that do not count as a format change,
     read from `settings.ffmpeg.format_change.*` at each frame like every
     format comparison
     ([common.md, Video format comparison](common.md#9-video-format-comparison));
     there is no creation-time snapshot;
   - `time_base`: as given.

The display matrix is not read from the codec parameters: the converter
reads it from each frame. FFmpeg's decoders copy the stream's display
matrix, and the EXIF orientation of images, onto each frame.

### 3.2 Converting and draining

- `convert stage frame deliver`: an empty stage calls `deliver frame`. A
  converter stage hands the frame to the bindings' display conversion, which
  delivers frames in order, upright, cropped and without their display
  matrix. Timestamps are unchanged.
- `eof stage deliver`: an empty stage holds nothing. A converter stage
  drains: it flushes its graph and delivers every frame it held, before the
  stage is released or replaced ([draining.md](draining.md)). The stage
  stays usable after a drain.
- A seek discards the stage's held frames instead
  ([draining.md §5](draining.md#5-discarded-on-purpose)).
- The stage is used by one decoder from one thread and needs no lock.

Conformance: a decoded rotated stream ending inside the converter's delay
delivers as many frames as were decoded.

### 3.3 Geometry before the first frame

Decoders size content before any frame is decoded. Two readers give the
geometry the display stage will produce, from the codec parameters only:

- **Stored layout**: width and height of the parameters, and the pixel
  aspect of the parameters unless a `stored` pixel aspect is given. No
  filter.
- **Displayed layout**: when `settings.ffmpeg.autorotate` is `false`, the
  stored layout. Otherwise the bindings' display layout
  ([avfilter.md §11.4](../../../../modules/synced/ffmpeg/spec/avfilter.md#114-display-filter-chain-and-layout))
  on the stored width, height and pixel aspect, with the parameters'
  cropping and the display matrix of the parameters' coded side data. When
  that layout is undecided, the stored layout.

| Reader                        | Returns                                  |
| ----------------------------- | ---------------------------------------- |
| `expected_size params`        | (width, height) of the displayed layout  |
| `pixel_aspect ?stored params` | the pixel aspect of the displayed layout |

A quarter turn swaps width and height and inverts the pixel aspect. Example:
a 1920×1080 stream with a 90° matrix has an expected size of 1080×1920. The
size is a hint. The frames delivered by §3.2 are authoritative, and the
content conformers fit later frames to the format the first frame set
([content.md §9](content.md#9-raw-video-conformance)).

### 3.4 Undecided cases

The bindings report three cases they do not decide: a rotation that is not a
quarter turn (with the clockwise angle), a cropping that leaves no picture
or has a negative border, and a hardware frame with a matrix or cropping to
apply. For each run of frames with the same question, the converter calls
the stage's `on_undecided` once:

- a list of filters: the frames go through them and lose their display
  matrix;
- no decision: the frames are delivered as stored, display matrix included.

The bindings' default handler logs one warning through FFmpeg's log and
decides nothing. With log capture on, it reaches the Liquidsoap log under
the label `ffmpeg` ([common.md, Log bridge](common.md#2-log-bridge)), for
instance (wording not binding):

```
[ffmpeg:3] Video left as stored, with its display matrix: the rotation of 327 degrees is not a quarter turn.
```

A script handler installed by §4 runs inside the conversion. When it raises,
the stage logs the error at level 2 (severe) with the reason of the question,
applies the default (no decision: video left as stored), and decoding
continues. A chain answered by a handler that fails to build (an unknown
filter name, a bad option) is logged the same way and the frames are
delivered as stored. Both are item faults
([faults.md §2](faults.md#2-classes)) whose item is the decision, not the
frame: they carry no media, so they do not count toward
`max_consecutive_faults` ([faults.md §4](faults.md#4-item-faults-escalate)).
Repeats are counted and logged at most once per `fault_log_interval`.

Conformance: a handler that raises on every call leaves the stream decoding,
as stored, with one severe log line per question.

## 4. `ffmpeg.autorotate.on_undecided`

```
ffmpeg.autorotate.on_undecided :
  (({cropping : {bottom : int, left : int, right : int, top : int}?,
     reason : string, rotation : float?}) -> [string * [string * string]]?)
  -> unit
```

Category `Liquidsoap`; the module `ffmpeg.autorotate` exists to hold it.

- Calling it installs the function process-wide, replacing any function
  installed before. It returns `()`.
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
  frame that raised the question. An exception it raises is handled as §3.4
  states.

Example: a file stored with `-display_rotation 33` (counter-clockwise)
asks once with `reason="odd_rotation"`, `rotation=327.0`.

## 5. `track.ffmpeg.side_data`

```
track.ffmpeg.side_data :
  (?id : string?, ?rotation : {float?}, ?remove : [string],
   ffmpeg.copy('a)) -> ffmpeg.copy('b)
```

Methods: `rotation : () -> float?`, `side_data : () -> [{data : string, kind : string}]`.

| Argument   | Default | Meaning                                                                               |
| ---------- | ------- | ------------------------------------------------------------------------------------- |
| `id`       | `null`  | track operator id                                                                     |
| `rotation` | `null`  | getter; a non-null value sets the stream's display rotation, counter-clockwise        |
| `remove`   | `[]`    | packet-family kind names to drop from the stream-wide side data and from every packet |

The operator is a track operator in category `Track`: it returns the same
field of a new source. Fallibility, remaining time, track abort, effective
source and self-sync are the input's. The operator is fallible exactly when
its input is. Its readiness is its own: it is not ready while a
rejected-source fault persists (§5.1 step 2.3), and otherwise ready exactly
when its input is.

With an infallible input, a persistent data fault (a rejected source) is a
hard error and the script fails
([faults.md §3](faults.md#3-source-faults-reach-the-script)). The operator's
documentation says so and names `mksafe` and `fallback` as the way to keep
streaming through such a fault.

A name in `remove` that is not one of FFmpeg's packet side-data kind names is
a configuration error at operator creation
([faults.md §2](faults.md#2-classes)). Source ids pass through unchanged
([content.md §5](content.md#5-stream_idx-and-time_base)).

The output format is separate from the input format (`'b`, not `'a`). The
edit changes header side data, which takes part in copy compatibility
([content.md §3.1](content.md#31-ffmpegcopy)): an input and an edited output
that shared one format would fail to merge.

### 5.1 Per frame

For each frame produced by the input:

1. Read the `rotation` getter once: `r`. Every run of this frame uses `r`.
2. For each run of the field's copy content, in order:
   1. When the run's source id differs from the last run seen, the stored
      entries of §5.3 are cleared.
   2. When the previous run's source parameters are the same value (physical
      identity) as this run's and the previous `r` is equal, reuse the
      previous result. Otherwise compute the edit (§5.2) and remember
      (input parameters, `r`, result).
   3. A newly computed result is merged into the output format
      ([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)).
      When the merge fails and the input parameters are those of the last
      accepted result (only `r` changed), the operator keeps the last
      accepted result for this run and logs a warning once per rejected
      value: a written stream header cannot change. When the input
      parameters are new (a new source), the operator logs both parameter
      sets at error level and ends that source's current track; a playlist
      or fallback moves on and the output keeps streaming. With the relaxed
      setting on, the merge succeeds and the muxer judges.

      Classes ([faults.md §2](faults.md#2-classes)): the rejected rotation
      is an item fault whose item is the edit; the data still flows, so it
      does not escalate. The rejected source is a source fault
      ([faults.md §3](faults.md#3-source-faults-reach-the-script)): end of
      track; while it persists, the operator is not ready when its input is
      fallible, and raises a hard error naming the source and both
      parameter sets when its input is infallible. It never invents
      content. A non-finite rotation is an item fault: the
      rotation is ignored for that frame with a warning.

   4. The run's source parameters become the result's. When `remove` names
      kinds present on the run's packets, each such packet is replaced by a
      duplicate without those entries; other packets are passed as they
      are.
   5. Store the result's entry list for §5.3.

The cache of step 2.2 gives an unchanged stream one parameters value.
Consumers that compare parameters by identity before comparing by value
then do no extra work: `track.ffmpeg.decode.video` keeps its decoder
([inline-codecs.md §11.1](inline-codecs.md#111-copy-trackffmpegdecodevideo)),
and the copy encoder's runtime check
([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)) has
nothing to compare.

### 5.2 The edit

Let `E` be the coded side data of the run's source parameters, in order.

1. Drop every entry of `E` whose kind name is in `remove`.
2. Video only: when `r` is non-null, drop every display-matrix entry and
   append one display matrix encoding a pure counter-clockwise rotation by
   `r` degrees ([avutil.md §4.19](../../../../modules/synced/ffmpeg/spec/avutil.md#419-display-matrix-and-cropping)).
   Audio and subtitle parameters get step 1 only.
3. When the resulting list equals `E` (structural equality of kinds and
   payloads), the result is the input parameters themselves. Otherwise it
   is a functional update of the input parameters with the new list
   ([avcodec.md §4.11](../../../../modules/synced/ffmpeg/spec/avcodec.md#411-side-data)).

Removal happens before the rotation is set, so `remove=["Display Matrix"]`
with a rotation yields the new matrix.

### 5.3 Methods

The methods describe the current source: the source of the last run seen.

- `side_data()`: the stored entry list (§5.1 step 2.5), each as
  `{kind = <FFmpeg name>, data = <payload>}`. `[]` before the first run of
  the current source.
- `rotation()`: the rotation of the first display-matrix entry of the stored
  list that decodes; `null` when there is none or the matrix is singular.

Conformance:

- A getter whose value changes between two frames is applied with the value
  read for each frame; no frame mixes two values.
- On a copy track already streaming to an output, a getter that changes its
  rotation keeps the first accepted rotation, logs one warning, and the
  output keeps streaming.
- `remove=["Display Matrix"]` on a stream whose packets carry a display
  matrix: neither the output header nor any output packet carries one.
- After a switch to a source without side data, `side_data()` returns `[]`.

## 6. `track.ffmpeg.raw.side_data`

```
track.ffmpeg.raw.side_data :
  (?id : string?, ?rotation : {float?}, ?remove : [string],
   ffmpeg.video.raw('a)) -> ffmpeg.video.raw('a)
```

Same arguments, methods and pass-through behaviour as §5, on a raw video
track, fallible exactly when its input is, and ready exactly when its input
is. Its documentation carries the same note on infallible inputs, naming
`mksafe` and `fallback`. `remove` matches frame-family kind names; an unknown name is a configuration error at operator creation. A
non-finite rotation is an item fault: the rotation is ignored for that frame
with a warning. The operator merges nothing and has no source fault of its
own. Raw formats do not include
side data, so input and output share one format.

### 6.1 Per frame

1. Read the `rotation` getter once: `r`. Every frame of this Liquidsoap
   frame uses `r`.
2. When `r` is `null` and `remove` is `[]`, the field is passed unchanged.
3. Otherwise, every frame of every run of the field is replaced by an
   edited copy, positions kept:
   1. duplicate the frame, because one frame is shared by every consumer of
      the track ([content.md §6.7](content.md#67-item-copy));
   2. remove every entry whose kind name is in `remove`;
   3. when `r` is non-null, attach a display matrix for a counter-clockwise
      rotation by `r`, replacing an existing one.
4. Remember the last frame of the field (edited or not), in run and
   position order. A run of a new source id clears the remembered frame
   before its frames are seen.

The edit applies to every entry of the frame, global or not.

### 6.2 Methods

- `side_data()`: the global side data (§1) of the remembered frame, named
  and shaped as in §5.3. `[]` before the first frame of the current source.
- `rotation()`: the rotation of the first display-matrix entry of that list,
  `null` when there is none.

With `settings.ffmpeg.autorotate` on, decoded raw frames have lost their
display matrix (§3), so `rotation()` reads `null` unless this operator sets
a rotation.

## 7. Side data given to encoders

- **Global side data** of a frame: its entries whose kind has the `Global`
  property, in order. One reader computes it for §6.2 and for encoders.
- A container encoder that creates a video stream from raw frames gives the
  stream the global side data of the first frame that has any. The stream
  carries it into the output container. How and when the stream is created
  is [encoder-internal.md](encoder-internal.md)'s.
- A stream header cannot change once written. Global side data that changes
  after the stream exists is not applied; the encoder logs a warning the
  first time it differs, naming the kinds that changed. This is not a fault
  of the data: the frames are encoded, and only the header keeps its first
  values.
- Copy encoding writes the source parameters it is given, coded side data
  included, and each packet's side data
  ([encoder-copy.md](encoder-copy.md)).
- Streams encoded from internal content and raw audio streams are given no
  side data: internal content carries none, and raw audio's global entries
  have no stream-header counterpart the layer writes. Inline encoders
  ([inline-codecs.md](inline-codecs.md)) take internal content, so they are
  given none either.

## 8. Elsewhere

- Frames enter and leave `ffmpeg.filter.*` graphs with whatever side data
  the filters keep. Liquidsoap adds and removes nothing there
  ([filters.md](filters.md)).
- Decoders that rebuild a display stage when the stream format changes, and
  the frame fitting used when frame sizes change, are specified in the
  decoder files and [content.md §9](content.md#9-raw-video-conformance).
- The script library defines nothing about side data or autorotate.

## 9. Worked examples

On a 320×240 MPEG-4 file whose stream was given `-display_rotation 90` by
the `ffmpeg` tool (ffprobe: `Display Matrix`, `rotation=90`):

| Script                                                                                | Result                                                                          |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- |
| copy track, `v = track.ffmpeg.side_data(video)`                                       | `v.side_data()` kinds `["Display Matrix"]`, `v.rotation()` `90.0`               |
| `track.ffmpeg.side_data(rotation=33., remove=["Display Matrix"], v)`, copied to mp4   | `rotation()` ≈ `33.0`; ffprobe of the output: `rotation=32` or `33`             |
| raw track, `settings.ffmpeg.autorotate := false`, `track.ffmpeg.raw.side_data(video)` | kinds `["3x3 displaymatrix"]`, `rotation()` `90.0`                              |
| the same, then `track.ffmpeg.raw.side_data(rotation=180., …)`, encoded from raw       | `rotation()` `-180.0`; ffprobe of the output: `Display Matrix`, `rotation=-180` |
| raw track, autorotate on (default)                                                    | kinds `[]`, `rotation()` `null`                                                 |
