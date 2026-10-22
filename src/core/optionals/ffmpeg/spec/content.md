# FFmpeg content

This file owns how FFmpeg data lives inside Liquidsoap frames: the three
FFmpeg content kinds, their parameters and how types name, print, parse,
merge and compare them; the two chunk layers; the run structure with
`stream_idx` and `time_base`; every data operation (make, length, blit,
sub, collapse, copy, consolidation); how the kinds exist in a process with no
libav; and the checksum. It relies
on the generic content contract of Liquidsoap (stated in §4 and §6 where the
FFmpeg kinds use it), on the FFmpeg bindings for packets, frames and codec
parameters ([bindings spec](../../../../modules/synced/ffmpeg/spec/README.md)),
and on the libavfilter helpers of [filters.md §16](filters.md#16-helper-graphs). Encoder-side timestamp rules live in
[timestamps.md](timestamps.md); how decoders fill content lives in
[decoder.md](decoder.md).

## 1. Kinds

| Kind name (scripts, types) | Content name (registry) | Payload                                  | Parameters                                        |
| -------------------------- | ----------------------- | ---------------------------------------- | ------------------------------------------------- |
| `ffmpeg.copy`              | `ffmpeg.copy`           | encoded packet: audio, video or subtitle | optional codec parameters (§3.1)                  |
| `ffmpeg.audio.raw`         | `ffmpeg.raw.audio`      | decoded audio frame                      | channel layout, sample format, sample rate (§3.2) |
| `ffmpeg.video.raw`         | `ffmpeg.raw.video`      | decoded video frame                      | width, height, pixel format, pixel aspect (§3.3)  |

No other FFmpeg content kind exists. The kind name is what a script writes and
what a type prints. The content name is the key under which the content
registers; it appears in no script-visible place except as the name of a
method on a format value (dots replaced by `_`, see §2.5).

A copy packet is one of `` `Audio of audio packet ``, `` `Video of video packet ``,
`` `Subtitle of subtitle packet ``. A single `ffmpeg.copy` track can in
principle hold packets of any of the three media; its parameters (§3.1) say
which one it holds once known.

Raw content is decoded frames that flow from decoder to encoder (or through
FFmpeg filters) without conversion to Liquidsoap's internal PCM or video
formats. Copy content is demuxed packets that are remuxed without decoding.

## 2. Names in types

### 2.1 Annotation syntax

A track annotation is `field=kind` or `field=kind(label=value, …)`:

```liquidsoap
def f(s) =
  (s:source(audio=ffmpeg.audio.raw(sample_rate=48000,channel_layout=stereo,sample_format=fltp)))
end
```

prints as (from the implementation, `liquidsoap -i --check`):

```
fff   :
  (source(audio=ffmpeg.audio.raw(sample_rate=48000,sample_format=fltp,channel_layout=stereo))) ->
  source(audio=ffmpeg.audio.raw(sample_rate=48000,sample_format=fltp,channel_layout=stereo))
```

Resolution of an annotation `kind(params)`:

1. The kind name is looked up among all registered kinds; the first whose
   parser accepts the exact string wins. Unknown name → parse error
   `Unknown type constructor: <kind>(<params>).` (error 3).
2. With no parameters the track type is the kind with a fresh format
   variable: `ffmpeg.copy('a)`.
3. Each `label=value` is parsed by the kind's parameter parser into a format
   holding only that parameter. A parser that returns nothing (unknown label,
   or any label for `ffmpeg.copy`) or raises (bad value) turns into the same
   error 3.
4. The formats of all parameters are merged left to right (§3). A merge
   failure (e.g. `sample_rate=48000,sample_rate=44100`) turns into error 3.

Examples, all from the implementation:

| Annotation                                                                             | Result                                                                         |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| `audio=ffmpeg.copy`                                                                    | `ffmpeg.copy('a)`                                                              |
| `audio=ffmpeg.copy(codec="aac")`                                                       | `Error 3: Unknown type constructor: ffmpeg.copy(codec=aac).`                   |
| `audio=ffmpeg.raw.audio`                                                               | `Error 3: Unknown type constructor: ffmpeg.raw.audio().`                       |
| `audio=ffmpeg.audio.raw(foo=1)`                                                        | error 3                                                                        |
| `audio=ffmpeg.audio.raw(channel_layout=bogus)`                                         | error 3                                                                        |
| `audio=ffmpeg.audio.raw(sample_rate=abc)`                                              | error 3                                                                        |
| `video=ffmpeg.video.raw(width=640,height=360,pixel_format=yuv420p,pixel_aspect="1/1")` | `ffmpeg.video.raw(pixel_aspect=1/1,pixel_format=yuv420p,height=360,width=640)` |
| `video=ffmpeg.video.raw(pixel_aspect=bogus)`                                           | `ffmpeg.video.raw` (bad aspect parses to "unset")                              |

### 2.2 Printing

A format prints as `kind` when its parameter string is empty, and as
`kind(params)` otherwise. A kind whose format is still a variable prints as
`kind('a)`. Parameter strings are defined per kind in §3.

### 2.3 Bare type names

Each kind name is also registered as a plain type name, so `ffmpeg.copy`,
`ffmpeg.audio.raw` and `ffmpeg.video.raw` are valid type annotations outside
`source(…)`. Each stands for the kind with a fresh variable:

```
def fff(x) = (x:ffmpeg.audio.raw) end
fff   : (ffmpeg.audio.raw('a)) -> ffmpeg.audio.raw('a)
```

(from the implementation).

### 2.4 Subtyping and errors

Unifying two formats of the same kind merges them (§3). A failed merge is a
type error; two different kinds never unify. Examples (from the
implementation):

```
Error 5: this value has type
  source(audio=ffmpeg.audio.raw(sample_rate=44100), _)
but it should be a subtype of
  source(audio=ffmpeg.audio.raw(sample_rate=48000), _)
```

```
Error 5: this value has type
  source(audio=ffmpeg.copy('A), _)
but it should be a subtype of
  source(audio=ffmpeg.audio.raw(sample_rate=44100), _)
```

### 2.5 Format values in the language

Each kind registers a content-language entry: the format's value in scripts
is a string, the printed parameter string of §3 (`params_to_value`). Its
method name is the content name with `.` replaced by `_` (`ffmpeg_copy`,
`ffmpeg_raw_audio`, `ffmpeg_raw_video`).

### 2.6 Dumps

A typing dump (the environment written for a process that typechecks without
the streaming core, e.g. the playground and the language server) carries a
format as `<kind name>:<serialized params>`. For all three FFmpeg kinds the
serialized params are the empty string, so a dump carries `ffmpeg.copy:`,
`ffmpeg.audio.raw:`, `ffmpeg.video.raw:`. Reading a dump accepts only the
empty string and rebuilds the default parameters (§3); any other payload
fails to parse. The reason, from the code: parameters read off a codec do not
travel, and a reader without libav could not rebuild them.

The script cache is a different mechanism: it stores the typed term itself,
and a cached script keeps annotated parameters (a run loaded from cache logs
`Content type is {audio=ffmpeg.audio.raw(sample_rate=48000)}`, from the
implementation).

## 3. Parameters

Every kind supplies, for its parameter type: a default, a printer, a
single-label parser, `merge` and `compatible`. A format holds the parameters;
a merge (below) makes two formats share one value from then on.

Generic merge of two formats `p` and `p'` of the same kind:

1. Compute `m = merge(params p, params p')`. If it raises, the generic layer
   raises `Incompatible_format (p, p')`, printed as
   `Content.Incompatible_format: formats <p> and <p'> are incompatible!`.
2. Store `m` in `p'` and link `p` to `p'`: both formats now read `m`, and any
   later change through either is seen by both.

Generic compatibility of `p` with `p'`: false if the kinds differ, else
`compatible(params p, params p')`. Compatibility changes nothing.

The per-parameter merge helper used by the raw kinds:

| Left  | Right           | Result                                 |
| ----- | --------------- | -------------------------------------- |
| unset | unset           | unset                                  |
| unset | `v`             | `v`                                    |
| `v`   | unset           | `v`                                    |
| `v`   | `v'`, equal     | `v`                                    |
| `v`   | `v'`, different | raises `Failure "Incompatible <name>"` |

Equality is structural unless stated otherwise.

### 3.1 `ffmpeg.copy`

Parameters: `None` (unknown) or `Some` of one of:

- `` `Audio codec_params ``
- `` `Video { avg_frame_rate : rational option; codec_params } ``
- `` `Subtitle { time_base : rational; codec_params } ``

Default: `None`. Parser: rejects every label (copy parameters wrap codec
parameters read from a container; there is nothing to build from a string).

Printer: comma-separated `key=value`, no spaces, in this order:

| Media    | Keys                                                                                                                                                                                                                                          |
| -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| none     | (empty string)                                                                                                                                                                                                                                |
| audio    | `codec` (quoted, OCaml string escaping), `channel_layout` (quoted description), `sample_format` (name, or `none`), `sample_rate`                                                                                                              |
| video    | `codec` (quoted), `width`, `height`, `aspect_ratio` (`num/den` of the sample aspect ratio), then `framerate` (`num/den`) only if known, then `pixel_format` only if the codec parameters have one (name, or `none` if the format has no name) |
| subtitle | `codec` (quoted), `time_base` (`num/den`)                                                                                                                                                                                                     |

Example (from the implementation, a 2 s AAC + H.264 MP4 decoded as copy):

```
{audio=ffmpeg.copy(codec="aac",channel_layout="stereo",sample_format=fltp,sample_rate=44100),video=ffmpeg.copy(codec="h264",width=320,height=240,aspect_ratio=1/1,framerate=25/1,pixel_format=yuv420p)}
```

`compatible(p, p')`:

1. Either side `None` → true.
2. Audio/audio: codec ids equal, and either the relaxed setting (§13) is on
   or: channel layouts equal (layout comparison), sample formats equal,
   sample rates equal.
3. Video/video: codec ids equal, and either relaxed or: widths equal, heights
   equal, sample aspect ratios compatible, pixel formats equal. Aspect ratios
   are compatible when equal, or when one is `0/1` and the other `1/1` (`0/1`
   means undefined and is treated as square). `avg_frame_rate` is never
   compared.
4. Subtitle/subtitle: relaxed, or codec ids equal. With relaxed on, the codec
   id is not checked for subtitles.
5. Different media → false.

`merge(p, p')`:

1. Either side `None` → the other side.
2. `compatible(p, p')` → `p` (the left side, unchanged; the right side's
   values, e.g. its frame rate, are dropped).
3. Otherwise raises `Failure "Incompatible format!"`.

### 3.2 `ffmpeg.audio.raw`

Parameters: `{ channel_layout; sample_format; sample_rate }`, each optional.
Default: all unset.

From a decoded frame (`frame_params`): all three set from the frame. From
codec parameters (`mk_params`): all three set from the codec parameters.

Parser:

| Label            | Value                                         | Failure                |
| ---------------- | --------------------------------------------- | ---------------------- |
| `channel_layout` | layout name, looked up by the bindings        | lookup raises          |
| `sample_format`  | sample format name, looked up by the bindings | lookup raises          |
| `sample_rate`    | decimal integer                               | `int_of_string` raises |
| other            | —                                             | returns nothing        |

Printer: `key=value` for each set parameter, joined by `,`, in the order
`sample_rate`, `sample_format`, `channel_layout` (the reverse of the
declaration order; unset parameters omitted). `channel_layout` prints the
layout description, `sample_format` the format name or `none`.

`merge`: per-parameter merge (table above); `channel_layout` compares with the
bindings' layout comparison, the others structurally.

`compatible(src, dst)`:

1. If `src` has all three parameters set: build a libavfilter `aformat`
   conversion from `src` (channel layout, sample format, sample rate, time
   base `1/<main tick rate>`) to whatever `dst` sets. Compatible if building
   it succeeds, incompatible if it raises. So compatibility here means "a
   conversion exists", not equality.
2. Otherwise: for each parameter, compatible when either side is unset or
   both are equal (layout comparison for channel layout).

### 3.3 `ffmpeg.video.raw`

Parameters: `{ width; height; pixel_format; pixel_aspect }`, each optional.
Default: all unset. From a frame: width, height, pixel format set from the
frame; pixel aspect from the frame when the frame has one.

Parser:

| Label             | Value                        | Failure                                   |
| ----------------- | ---------------------------- | ----------------------------------------- |
| `width`, `height` | decimal integer              | raises                                    |
| `pixel_format`    | pixel format name (bindings) | raises                                    |
| `pixel_aspect`    | first match of `(\d+)/(\d+)` | no match → parameter left unset, no error |
| other             | —                            | returns nothing                           |

Printer: `key=value` for set parameters in the order `pixel_aspect`
(`num/den`), `pixel_format` (name or `none`), `height`, `width` (reverse of
declaration order). With nothing set the format prints as the bare kind
`ffmpeg.video.raw`.

`merge`: per-parameter merge on all four, structural equality.

`compatible`: each parameter compatible when either side is unset or both are
equal. No conversion check.

## 4. Two chunk layers

FFmpeg content is chunked twice.

**View layer** (generic, shared by every content type). A content value is
`{ params; chunks : view list; total_length }` where each view is
`{ data; offset; length }`: a window of `length` ticks starting at `offset`
into an underlying data value. Slicing and appending frames create and
combine views without touching the data. `length = max_int` is an "infinite"
sentinel used by some contents; FFmpeg data never produces it but the
generic code handles it.

**Run layer** (FFmpeg only). The data value a view points to is

```
content = { params : P; chunks : run list }
run     = { length : int;              (* ticks *)
            stream_idx : int64;
            time_base : rational;
            data : (int * item) list } (* (position in ticks, payload), sorted *)
```

A run is a stretch of time from one producer. Its `data` is sparse: zero, one
or many items, each at a position relative to the start of the run. A run with
`data = []` and positive length means "this stream is present here and has
nothing to say", which is how sparse streams (subtitles) and dense streams
slower than a frame (25 fps video in a 20 ms frame) are represented.

Runs are laid end to end: run `i` starts at the sum of the lengths of runs
`0..i-1`. Runs carry no absolute offset.

The length of a run-layer content is the sum of its run lengths. Operations
that work in place (§6.6) replace `params` and `chunks` of an existing value.

## 5. `stream_idx` and `time_base`

`stream_idx` is a 64-bit identifier of the producer. A process-wide counter
starts at 0; `new_stream_idx ()` increments it and returns the new value, so
the first id handed out is 1. `0` is the default of a run built without one.
Allocation:

| Producer             | Granularity                                                                                                                                                           |
| -------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| decoder              | one per stream mapping, shared by all its fields: at open and at every seek ([decoder.md §12](decoder.md#12-seeking)); each reconnect of an input opens a new decoder |
| inline encoder       | one per source                                                                                                                                                        |
| FFmpeg filter output | one per output, renewed when the filter graph is rebuilt                                                                                                              |

(details in [decoder.md](decoder.md), [inline-codecs.md](inline-codecs.md),
[filters.md](filters.md)). Producers that pass content through (bitstream
filters, side-data operators) keep the incoming run's `stream_idx`.

The content layer uses `stream_idx` for one thing: adjacent runs with the
same id are merged by collapse (§6.5). What encoders do on a new id is in
[timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what).

`time_base` is the time base of the timestamps carried inside the run's
items (§7). The content layer never converts with it; it only carries it.

## 6. Operations

### 6.1 make

Run layer: `make params` returns `{ params; chunks = [] }`. The requested
length is ignored: FFmpeg data has no notion of a pre-sized empty buffer.

View layer: `make ~length params` wraps that empty data in a single view of
the requested length: `{ chunks = [{ data; offset = 0; length }];
total_length = length }`. So a frame created with a given length
(e.g. `Frame.create`) holds a view of `length` ticks over run data of length 0. Without `~length`, the view length is the data length, 0.

`make_data` (run constructor) defaults: `length = 0`, `stream_idx = 0`,
`time_base = 1/1`, `data = []`; it sorts `data` by position.

### 6.2 lift_data

Wraps run-layer content in one view: `offset` defaults to 0, `length`
defaults to the run-layer length minus `offset`; the view-layer params are
the run-layer params. This is how every producer hands a chunk of content to
a generator: it builds a content with one run and lifts it.

### 6.3 length

Run layer: sum of run lengths. View layer: `total_length`.

### 6.4 sub of a run

`sub_data run offset len`: a run of length `len`, same `stream_idx` and
`time_base`, holding the items with `offset <= pos < offset + len`, each at
`pos - offset`, each passed through the kind's item copy (§6.7), sorted.

### 6.5 collapse

Input: a list of `(absolute offset, run)` sorted by offset. Walk it, keeping
an accumulator. For each entry, if the previous accumulated entry has the
same `stream_idx`, replace it by a merged run:

1. `new_start = min(prev_offset, offset)`,
   `new_end = max(prev_offset + prev.length, offset + run.length)`.
2. Items of the previous run shift by `prev_offset - new_start`; items of the
   current run by `offset - new_start`.
3. The merged run has length `new_end - new_start`, the current run's
   `stream_idx` and `time_base`, and the union of items sorted by position.

Otherwise push the entry. The result drops the offsets and keeps the runs in
order.

Adjacency is not checked: two consecutive entries with the same id merge even
if a gap separates them, and the gap becomes part of the merged run. The time
base of the later run wins for the merged run.

### 6.6 blit

`blit src src_pos dst dst_pos len` (run layer), in place on `dst`:

1. `dst.params <- src.params`, always, even when no item is copied.
2. Keep each run of `dst` (at its implicit offset) that lies entirely before
   `dst_pos` or entirely at or after `dst_pos + len`. A `dst` run that
   overlaps the region at all is dropped whole, including the parts outside
   the region.
3. For each run of `src` (at its implicit offset `pos`) that overlaps
   `[src_pos, src_pos + len)`: `start = max 0 (src_pos - pos)`,
   `stop = min run.length (src_pos + len - pos)`; take
   `sub_data run start (stop - start)` and place it at
   `dst_pos + pos + start - src_pos`.
4. Concatenate kept and new entries, sort by offset, collapse (§6.5).
5. `dst.chunks <-` the collapsed runs.

Because runs carry no offsets (§4), step 5 loses every gap: a kept run after
the region, or a new run placed beyond the current end of `dst`, ends up
directly after the previous run. Examples from a verbatim copy of the
implementation's pure code (items shown by position):

| Case                                                                                                                     | Result                                                      |
| ------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------- |
| `src` one run `id=2, length=100, [15;35;55;75]`, `dst` one run `id=1, length=100, [5;25;50;85]`, `blit src 20 dst 20 40` | `dst` = one run `id=2, length=40, [15;35]`; total length 40 |
| empty `dst`; blit 30 ticks of an empty content at 0, then a run `id=3, length=50, [0;20]` at 30                          | one run `id=3, length=50, [0;20]`; total length 50, not 80  |
| run `id=3, len 40, [0;30], tb 1/44100` then run `id=3, len 40, [0], tb 1/1000` blitted at 0 and 40                       | one run `id=3, length=80, tb=1/1000, [0;30;40]`             |
| runs `id=3, len 40` and `id=4, len 40` blitted whole                                                                     | two runs kept apart, total 80                               |

### 6.7 Item copy

| Kind                 | Item copy                                                                        |
| -------------------- | -------------------------------------------------------------------------------- |
| `ffmpeg.copy`        | duplicate the packet (bindings `Packet.dup`, a new reference to the same buffer) |
| raw audio, raw video | identity: the frame value is shared, not copied                                  |

### 6.8 copy

Run layer: a new content with the same params and every run rebuilt with its
items passed through the item copy and re-sorted.

View layer: consolidation with copy (§6.9), which always builds a new run
buffer.

### 6.9 Consolidation (view layer)

`consolidate ~copy content` turns any list of views into at most one view
starting at offset 0:

1. `total_length = 0` → no views.
2. One view at offset 0 whose length is the data length (or `max_int`), and
   `copy` false → unchanged.
3. Otherwise: `buf = make params` (run layer, empty); for each view in order,
   with `pos` starting at 0: if its length (capped at `data length - offset`
   for `max_int`) is positive, `blit view.data view.offset buf pos length`;
   `pos += length`. The result is one view `{ offset = 0; length =
total_length; data = buf }`. With `copy` false the content is updated in
   place; with `copy` true a new content is returned.

`get_data` consolidates without copy and returns the single data value (an
empty run content if there is no view; raises `Invalid` if more than one view
remains, which consolidation never leaves).

Consequences for FFmpeg data (from §6.6):

- A view over data that is shorter than the view (the empty data of §6.1)
  contributes nothing and leaves no gap: later runs move earlier. After
  consolidation the run-layer length can be smaller than the view length.
- Runs of the same `stream_idx` coming from consecutive views merge into one
  run with positions relative to its start.
- Runs of different `stream_idx` stay apart; a frame that straddles a track
  boundary yields two runs.

Consumers iterate the runs of `get_data` and read each item's position
relative to its run ([encoder-internal.md §3](encoder-internal.md#3-input),
[encoder-copy.md §3](encoder-copy.md#3-input), [filters.md §13](filters.md#13-frame-flow-and-timestamps)).

### 6.10 sub, truncate, append, is_empty (view layer)

These never touch FFmpeg data.

- `sub content ofs len`: raises `Invalid_argument "Content.sub"` if
  `ofs > total_length` or `ofs + len > total_length`; else keeps the views
  that intersect `[ofs, ofs + len)`, each narrowed (offset moved forward,
  length cut). `total_length = len`.
- `truncate content len`: drops the first `len` ticks of views.
- `append c c'`: concatenates the view lists; total length is the sum
  (`max_int` if either is).
- `is_empty`: `total_length = 0`.

There is no fill operation.

## 7. Timestamps carried in content

Items keep the timestamps their producer gave them, expressed in the run's
`time_base`:

- copy packets keep their demuxed `pts`, `dts` and `duration` untouched, in
  the stream time base of the container they came from, which is the run's
  `time_base`;
- raw frames keep their `pts` in the producer's time base: the stream time
  base for decoders, the filter output time base for filter outputs.

Positions inside a run are in main ticks and are what the frame timeline uses;
timestamps inside items are what encoders use. The content layer never
rewrites either: blit and collapse shift positions, never timestamps, and
merge runs while keeping only the later `time_base`.

How producers derive run lengths and positions from timestamps (each packet or
frame emitted in its own run, at position 0, with a length equal to the
timestamp difference to the next one) is in
[common.md §6](common.md#6-duration-helper) and [decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side). How
encoders turn carried timestamps into output timestamps is in
[timestamps.md](timestamps.md).

## 8. Keyframes

Copy content tracks no keyframe state. A packet's key flag is the packet's own
(bindings). Waiting for a keyframe at the start of a stream or after a change
of `stream_idx` is encoder behaviour
([encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet), [§12.3](encoder-copy.md#123-contributions-and-readiness)).

## 9. Raw video conformance

Every producer of raw video (raw decoder, FFmpeg filter outputs) delivers
frames through a conformer bound to the track's format. The first frame sets
the track's format, which stays open until then; a later frame in another
format is fitted to it.

For each frame, with `declared` the format's current parameters and `params`
the frame's (§3.3):

1. `declared = params` → pass the frame on.
2. `compatible(declared, params)` → merge the frame's parameters into the
   format (filling unset parameters), pass the frame on.
3. Otherwise build a target: width, height, pixel format each from `declared`
   if set, else from the frame; pixel aspect from `declared` (possibly unset).
   Merge `{declared with width; height; pixel_format}` of the target into the
   format, then convert the frame through the fit filter (scale into the
   target while keeping proportions; [filters.md §16.4](filters.md#164-fit)) and pass
   on its output.

Merges here change the shared format (§3), so the source's content type
reflects the first frame from then on.

## 10. Without libav

The kinds exist in every process, libav or not. They are split into:

- a kind-and-name layer that has no libav dependency: the names (§1), the
  type registration (§2.3), printing, the single-label parser hook, merge and
  compatibility hooks, and the dump form (§2.6); it takes a parameter
  implementation as input;
- a parameter implementation, of which exactly one is present in a process:
  - **libav**: the parameters of §3, read off codecs and frames; plus the
    run-layer data operations of §6;
  - **stand-in** ("the stand-in for libav"): parameters are the
    `(label, value)` pairs a script wrote, with no data operations (making
    data raises `Invalid`). Only a process that cannot stream links it: the
    playground and the language server's analysis.

Stand-in parameters:

| Operation           | Behaviour                                                                                                                     |
| ------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| default             | `[]`                                                                                                                          |
| parse `label value` | always succeeds: `[(label, value)]`, any label, any kind, including `ffmpeg.copy`                                             |
| print               | `label=value` joined by `,`, in stored order                                                                                  |
| compatible `p p'`   | every label of `p` present in `p'` has the same value                                                                         |
| merge `p p'`        | raises `Failure "Incompatible format!"` if not compatible; else `p` followed by the pairs of `p'` whose labels are not in `p` |

Exactly one implementation registers per process. The first kind to register
records its implementation name; a later registration under another name
raises `Failure "FFmpeg content types are implemented by <first> here: <second>
cannot be linked with it!"`. Reason, from the code: both register the same
kinds, and a second registration would otherwise silently win.

The `%ffmpeg` encoder's frame type refers to these kinds by name
(`ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw`), so it typechecks with
either implementation ([encoder-format.md §10](encoder-format.md#10-content-type-implied-by-a-format)). Outside `%ffmpeg`, encoder
types reject `pcm_s16` and `pcm_f32` audio parameters with
`<s> audio is only supported by the %ffmpeg encoder.`.

## 11. Checksum

Two levels, both MD5 rendered as 32 lowercase hex digits.

Run layer, per content:

1. For each run, for each item `(pos, item)`:
   `"<pos>:<stream_idx>:<num>/<den>:<item checksum>"`; items joined by `|`.
   A run with no items gives the empty string. Run lengths are not digested.
2. Runs joined by `||`; MD5 of the result.

Item checksums:

| Kind      | Item checksum                                                                                                 |
| --------- | ------------------------------------------------------------------------------------------------------------- |
| copy      | MD5 of the packet's bytes                                                                                     |
| raw audio | convert the frame with swresample to interleaved `s16`, same channel layout and sample rate; MD5 of the bytes |
| raw video | convert the frame with swscale to `yuv420p`, same size; concatenate the plane data; MD5                       |

A converter is created for every item.

View layer: for each view `"<offset>:<length>:<run-layer checksum of its
whole data>"`, joined by `|`, MD5. The frame checksum (used by the
frame-checksum debugging callbacks of sources) consolidates each field with
copy first, so an FFmpeg field is digested as one view at offset 0.

Position, stream and time base are digested on purpose: the same frames at
different offsets are different content (code comment).

Example (from a verbatim copy of the implementation's pure code, string
items digested as themselves): one run `id=3, tb=1/1000` with items
`(0,P);(30,P2);(40,Q)` → `bdb4cf98124d3dac6ef6ea8adbdf82f3`.

## 12. Inline transform plumbing

The shared producer of inline codecs and bitstream filters (metadata and
track-mark relay, transform lifetime) is described in
[inline-codecs.md, The inline producer](inline-codecs.md#3-the-inline-producer).

## 13. Settings

| Key                                                        | Type | Default | Effect                                                                                             |
| ---------------------------------------------------------- | ---- | ------- | -------------------------------------------------------------------------------------------------- |
| `settings.ffmpeg.content.copy.relaxed_compatibility_check` | bool | `false` | §3.1: skip all parameter checks except the codec id for audio/video, and every check for subtitles |

`settings.ffmpeg.content` is an empty section holding the above.
