# FFmpeg Content Structure

This document explains how Liquidsoap handles FFmpeg content—audio frames, video frames, and encoded packets.

## Why This Structure?

The content structure addresses three main challenges:

1. **Sparse streams** – Subtitles often have long gaps with no data. We need to track that a stream exists even when nothing is happening.

2. **Multiple sources** – Content can come from different decoders, each with its own `stream_idx`. These shouldn't get mixed together.

3. **Efficient operations** – Blit, copy, and sub operations need to respect stream boundaries and handle gaps correctly.

## Two Layers of Chunks

There are two chunk layers stacked on top of each other, and they are not the same thing:

- `Content_base.MkContentBase` (in `src/core/base/stream/`) wraps every content type in its own
  `chunks` list. That layer is a **view**: each entry is an `{ data; offset; length }` window into a
  larger value, so `Frame.slice` and `Frame.append` are cheap and copy nothing. It is generic and
  applies to PCM and video content just the same.

- `Ffmpeg_content_base` adds a second `chunks` list _inside_ the value that layer views. That layer
  is a **run**: each entry carries `stream_idx` and `time_base` and holds sparse `(position, payload)`
  pairs. It exists for the three reasons above — a run is what lets a subtitle stream have gaps, and
  what keeps packets from two decoders from being merged.

So a single ffmpeg track can be chunked twice: once by the generic view layer, once by the
stream-run layer. `Content_base.consolidate_chunks` collapses the outer one by calling `blit`, which
is where `Ffmpeg_content_base.blit` and `collapse_chunks` below come in.

## Core Types

The `Ffmpeg_content_base` module defines the fundamental types:

```ocaml
(* A chunk of data from a single stream *)
type 'b data = {
  length : int;                 (* Duration in main ticks *)
  stream_idx : Int64.t;         (* Unique identifier for the source stream *)
  time_base : Avutil.rational;  (* Time base for timestamps *)
  data : (int * 'b) list;       (* List of (position, payload) pairs *)
}

(* Top-level content container *)
type ('a, 'b) content = {
  mutable params : 'a;          (* Format parameters *)
  mutable chunks : 'b data list;  (* List of data chunks *)
}
```

## Visual Examples

### Basic Structure

```
┌─────────────────────────────────────────────────────────────────┐
│ content                                                         │
├─────────────────────────────────────────────────────────────────┤
│ params: { channel_layout: stereo, sample_rate: 44100, ... }     │
├─────────────────────────────────────────────────────────────────┤
│ chunks:                                                         │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ chunk 0                                                 │   │
│   │   stream_idx: 1                                         │   │
│   │   time_base: 1/44100                                    │   │
│   │   length: 1024                                          │   │
│   │   data: [(0, frame), (512, frame), (768, frame)]        │   │
│   └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### Dense Audio (Single Source)

```
Time:        0       256      512      768     1024
             |--------|--------|--------|--------|

content.chunks = [
  ┌────────────────────────────────────────────────┐
  │ stream_idx: 1, length: 1024                    │
  │ data: [                                        │
  │   (0,   ♪ audio frame 1),                      │
  │   (256, ♪ audio frame 2),                      │
  │   (512, ♪ audio frame 3),                      │
  │   (768, ♪ audio frame 4)                       │
  │ ]                                              │
  └────────────────────────────────────────────────┘
]
```

### Dense Video with Variable Frame Rate (Encoded)

In copy mode, encoded video packets retain their frame types. With variable frame rate (VFR), frames aren't evenly spaced:

```
Time:        0       100      200      300      400      500      600
             |--------|--------|--------|--------|--------|--------|
             I      P     P            P         I          P
             0      80    140          290       400        520

content.chunks = [
  ┌──────────────────────────────────────────────────────────────┐
  │ stream_idx: 7, length: 600                                   │
  │ data: [                                                      │
  │   (0,   🎬 I-frame),   ← keyframe, can be decoded alone      │
  │   (80,  🎬 P-frame),   ← predicted, depends on previous      │
  │   (140, 🎬 P-frame),                                         │
  │   (290, 🎬 P-frame),   ← note the gap: VFR in action         │
  │   (400, 🎬 I-frame),   ← new keyframe                        │
  │   (520, 🎬 P-frame)                                          │
  │ ]                                                            │
  └──────────────────────────────────────────────────────────────┘
]

Frame intervals vary: 80, 60, 150, 110, 120 ticks.
I-frames appear periodically for seeking and error recovery.
```

### Sparse Subtitles

```
Time:        0       500     1000     1500     2000
             |--------|--------|--------|--------|
                           ▼              ▼
                        "Hello"        "World"

content.chunks = [
  ┌────────────────────────────────────────────────┐
  │ stream_idx: 42, length: 2000                   │
  │ data: [                                        │
  │   (800,  📝 "Hello"),                          │
  │   (1200, 📝 "World")                           │
  │ ]                                              │
  │                                                │
  │ (No subtitles from 0-799, 801-1199, 1201-2000) │
  └────────────────────────────────────────────────┘
]
```

### Multiple Sources

When blitting from different sources (e.g., track changes), chunks keep their distinct `stream_idx`:

```
Time:        0              500            1000
             |---------------|---------------|
             ← Source A →    ← Source B →

content.chunks = [
  ┌────────────────────────────────────────────────┐
  │ AUDIO CHUNK (from source A)                    │
  │ stream_idx: 1, length: 500                     │
  │ time_base: 1/44100                             │
  │ data: [(0, ♪), (128, ♪), (256, ♪), (384, ♪)]   │
  └────────────────────────────────────────────────┘
  ┌────────────────────────────────────────────────┐
  │ AUDIO CHUNK (from source B)                    │
  │ stream_idx: 2, length: 500                     │
  │ time_base: 1/44100                             │
  │ data: [(0, ♪), (128, ♪), (256, ♪), (384, ♪)]   │
  └────────────────────────────────────────────────┘
]

These chunks won't be collapsed—different stream_idx values are kept separate.
Total length: 500 + 500 = 1000
```

### Empty Chunk

A chunk can exist without any data. This is useful for tracking that a stream is present during a silent period:

```
Time:        0              1000
             |----------------|

content.chunks = [
  ┌────────────────────────────────────────────────┐
  │ SUBTITLE CHUNK (empty - no subtitles yet)      │
  │ stream_idx: 5, length: 1000                    │
  │ time_base: 1/1000                              │
  │ data: []   ← empty list, but chunk exists!     │
  └────────────────────────────────────────────────┘
]

This says: "We have a subtitle stream spanning 1000 ticks,
            but no subtitles appear during this period."
```

## Key Concepts

### Stream Index (`stream_idx`)

Each chunk carries a `stream_idx` identifying its source:

- Data from different sources should never be mixed
- Adjacent chunks with the same `stream_idx` get merged automatically during blit
- Different values mean the data came from different decoders

Generate a new unique `stream_idx` with `Ffmpeg_content_base.new_stream_idx()`.

### Data Positions

The `data` field holds a list of `(position, payload)` pairs:

- Positions are **relative to the chunk start** (ranging from 0 to `length`)
- Positions are always sorted
- Sparse content (like subtitles) will have gaps between positions

### Dense vs Sparse Streams

A chunk's `length` is its time duration, but `data` may have fewer entries than you'd expect:

- **Dense streams** (audio/video): Each packet or frame is immediately followed by another. But a dense stream can still have gaps if its data rate is lower than the chunk duration. For example, 25fps video produces one frame every 40ms—a chunk shorter than 40ms might have no frames at all.

  In practice, **audio streams have no gaps** because sample rates (e.g., 44100 Hz) are higher than Liquidsoap's internal tick rate.

- **Sparse streams** (subtitles): Data appears only at specific moments, with long gaps in between. A subtitle might show up once every few seconds.

- **Empty chunks**: A chunk with `data = []` but positive `length` is valid. It represents stream presence without content—common for sparse streams during quiet periods.

## Content Types

### Raw Content (`Ffmpeg_raw_content`)

Decoded frames that can optionally be processed through FFmpeg filters. The key advantage is that raw FFmpeg content can flow through the entire pipeline—from decoding to encoding—without being converted to Liquidsoap's native internal format. This can save significant memory and CPU.

The trade-off: most of Liquidsoap's internal operators aren't available for this content type. Things like crossfade, LUFS measurement, amplitude adjustment, and other audio/video processing tools require native content.

**Audio** (`Ffmpeg_raw_content.Audio`):

- Payload: `Avutil.audio Avutil.frame`
- Parameters: `channel_layout`, `sample_format`, `sample_rate`

**Video** (`Ffmpeg_raw_content.Video`):

- Payload: `Avutil.video Avutil.frame`
- Parameters: `width`, `height`, `pixel_format`, `pixel_aspect`

### Copy Content (`Ffmpeg_copy_content`)

Encoded packets for copy/passthrough mode—packets aren't decoded, just remuxed. This is the most efficient way to handle media when you don't need to modify the content, preserving both CPU and memory.

However, proper remuxing can get tricky. It may require a solid understanding of the underlying bitstream structure, and you might need to use bitstream filters to ensure compatibility between container formats.

- Payload: `packet` (audio, video, or subtitle)
- Parameters: codec-specific (`codec_params`)

## Operations

### `blit`

```ocaml
val blit : src -> src_pos -> dst -> dst_pos -> len -> unit
```

Copies a range of data from source to destination. Extracts `len` ticks starting at `src_pos` and writes them to `dst` starting at `dst_pos`.

What happens:

- Overlapping data in `dst` gets replaced
- Positions are adjusted to be relative to their new chunk
- Adjacent chunks with the same `stream_idx` are merged
- Source's `params` are copied to `dst`

### Blit Example: Basic Operation

```
SOURCE (stream_idx=2):
  Position:  0    10   20   30   40   50   60   70   80   90  100
             |----|----|----|----|----|----|----|----|----|----|
  chunks: [
    ┌──────────────────────────────────────────────────────────┐
    │ stream_idx: 2, length: 100                               │
    │ data: [(15, ♪A), (35, ♪B), (55, ♪C), (75, ♪D)]           │
    └──────────────────────────────────────────────────────────┘
  ]

DESTINATION (stream_idx=1, has existing data):
  Position:  0    10   20   30   40   50   60   70   80   90  100
             |----|----|----|----|----|----|----|----|----|----|
  chunks: [
    ┌──────────────────────────────────────────────────────────┐
    │ stream_idx: 1, length: 100                               │
    │ data: [(5, ♪X), (25, ♪Y), (50, ♪Z), (85, ♪W)]            │
    └──────────────────────────────────────────────────────────┘
  ]


OPERATION: blit src 20 dst 20 40
           ─────────────────────────
           Copy 40 ticks from src[20..60] into dst[20..60]

  dst before:
             0         20──────────────────60               100
             |          [====blit region====]                |
             ♪X           ♪Y          ♪Z             ♪W
             5            25          50             85
                          ↑           ↑
                          └─ overlaps ┘ (these will be removed)

  src:       0         20──────────────────60               100
             |          [====blit region====]                |
                           ♪B          ♪C
                           35          55
                           ↓           ↓
                   positions adjusted: 35-20=15, 55-20=35
                           ↓           ↓
                  in dst:  20+15=35    20+35=55


RESULT - dst.chunks after blit:
  Position:  0    10   20   30   40   50   60   70   80   90  100
             |----|----|----|----|----|----|----|----|----|----|
  chunks: [
    ┌─────────────────────┐
    │ stream_idx: 1       │  ← preserved (before blit region)
    │ length: 20          │
    │ data: [(5, ♪X)]     │
    └─────────────────────┘
    ┌─────────────────────────────┐
    │ stream_idx: 2               │  ← inserted from src (starts at 20)
    │ length: 40                  │
    │ data: [(15, ♪B), (35, ♪C)]  │     (relative: 35-20=15, 55-20=35)
    └─────────────────────────────┘
    ┌─────────────────────┐
    │ stream_idx: 1       │  ← preserved (starts at 60)
    │ length: 40          │
    │ data: [(25, ♪W)]    │     (relative: 85-60=25)
    └─────────────────────┘
  ]

  ♪Y and ♪Z were in the blit region and got replaced.
  Chunks aren't merged because stream_idx differs (1 vs 2).
  All positions are relative to their chunk's start.
```

### `collapse_chunks`

Merges adjacent chunks with the same `stream_idx`. Called automatically during `blit`.

```
BEFORE:
  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
  │ stream_idx: 1   │ │ stream_idx: 1   │ │ stream_idx: 2   │
  │ length: 25      │ │ length: 25      │ │ length: 50      │
  └─────────────────┘ └─────────────────┘ └─────────────────┘
          │                   │                   │
          └─────────┬─────────┘                   │
                    ↓                             ↓
AFTER:
  ┌─────────────────────────────────────┐ ┌─────────────────┐
  │ stream_idx: 1                       │ │ stream_idx: 2   │
  │ length: 50                          │ │ length: 50      │
  └─────────────────────────────────────┘ └─────────────────┘
```

## Invariants

1. **Data is sorted** – The `data` list is always sorted by position
2. **Positions are relative** – Positions range from 0 to `length`, relative to chunk start
3. **Adjacent chunks collapse** – Same `stream_idx` + adjacent = merged during blit
4. **Non-adjacent chunks stay separate** – Chunks with the same `stream_idx` but separated by other sources remain distinct
5. **Empty data is valid** – `data = []` with positive `length` represents stream presence without content

## Configuration

- `settings.ffmpeg.content.copy.relaxed_compatibility_check` – When `true`, allows mixing streams with different parameters (e.g., different sample rates or resolutions)
