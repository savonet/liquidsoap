# Findings: content

## Defects

### Consolidation loses gaps and shifts positions

`Ffmpeg_content_base.blit` places runs at computed offsets, then
`collapse_chunks` returns `List.map snd`, dropping the offsets
(base/ffmpeg_content_base.ml:93-127, 163). A view over shorter data (e.g. a
field of `Frame.create ~length`, whose FFmpeg `make` ignores `?length`,
base/ffmpeg_copy_content.ml:205) blits nothing, and the next view's runs land
at offset 0 instead of after the gap. After `get_data`, run-layer length
< view length, and item positions no longer match frame positions. Consumers
that filter `0 <= pos < Frame.position` (encoder/ffmpeg_internal_encoder.ml:226, 583) then see items at the wrong time. Why it matters: silent timeline shift
for any FFmpeg field that is padded or partially empty.
**checked** (verbatim copy of the pure code: empty 30 ticks + 50-tick run →
total 50, not 80).

### blit drops a partially overlapped destination run whole

base/ffmpeg_content_base.ml:136-141: any dst run that touches
`[dst_pos, dst_pos+len)` is removed entirely, including parts outside the
region, and the dst length shrinks. Only matters when blitting into a
non-empty dst; consolidation always blits into an empty one. Pinned as
intended by tests/core/ffmpeg_content_base_test.ml:118, contradicted by
base/FFMPEG_CONTENT.md ("Blit Example": claims runs before/after the region
are preserved, lengths 20/40/40). **checked**.

**Verification** (verification pass): Worse: the packets of the dropped run that lie outside the blit region are lost with it.

### Collapse merges runs with different time bases

base/ffmpeg_content_base.ml:111: the merged run takes the later run's
`time_base`; items from the earlier run keep timestamps in their own time
base, now labelled with another. Same `stream_idx` normally means the same
stream, so time bases match in practice, but nothing enforces it.
**checked** (copy of pure code: tb 1/44100 + 1/1000 → 1/1000).

## Asymmetries

### Collapse ignores adjacency

`collapse_chunks` merges consecutive same-id runs whatever the distance
between them; FFMPEG_CONTENT.md says "Adjacent chunks with the same
`stream_idx` get merged". **read only**.

### Raw audio `compatible` means "convertible", raw video means "equal"

ffmpeg_raw_content.ml:99-121 builds an aformat graph when src is fully
specified; VideoSpecs.compatible (:239) is per-field equality, and the
conformer fits mismatched frames anyway. Copy audio/video always check codec
id; copy subtitles skip even the codec id when relaxed (ffmpeg_copy_content.ml:168-172).
**read only**.

### Copy merge keeps the left side

`Specs.merge` returns `p` when compatible (ffmpeg_copy_content.ml:176); the
right side's `avg_frame_rate` and, under relaxed checking, its
rate/size/layout are dropped. **read only**.

### Printed parameter order reversed for raw kinds

`Content.print_optional` folds by consing (formats/content_base.ml:64-69), so
raw formats print in reverse declaration order (`sample_rate,…,channel_layout`)
while copy prints in declaration order. **checked** (`-i --check`).

### Stand-in accepts what libav rejects

Dummy `parse` accepts any label for any kind (formats/dummy/…:30), so
`ffmpeg.copy(codec="aac")` and `ffmpeg.audio.raw(foo=1)` typecheck in the
playground/LSP and fail with error 3 in liquidsoap. Values are also compared
as strings (`48000` vs `48000.`). **read only** for the dummy, **checked**
for the libav side.

### `pixel_aspect` parse failure is silent

ffmpeg_raw_content.ml:204-216: a bad value yields an unset parameter rather
than a parse error, unlike every other label. **checked**
(`ffmpeg.video.raw(pixel_aspect=bogus)` → `ffmpeg.video.raw`).

### Checksum ignores run lengths

`Ffmpeg_content_base.checksum` digests items only; two contents differing
only in run lengths (or in empty runs) have equal checksums, although the
comment says position matters. **read only**.

### get_type raw audio branch replaces the requested format

stream_description.ml:256-262: the result field is a new format, not the
requested `f`, and a failed merge is swallowed when `compatible` holds; copy
and raw video keep `f`. **read only**.

### Inline producer doc comment vs behaviour

base/ffmpeg_inline.ml:44-45 says the transform is "dropped on flush, so codec
state never survives a track boundary"; the transform is only dropped on
`` `Flush ``, which inline-codecs.md reports is not sent at track boundaries.
**read only**.

## Gaps

### Dumps drop FFmpeg parameters

`serialize_params _ = ""` (formats/ffmpeg_content_type.ml:85): a typing dump
of `ffmpeg.audio.raw(sample_rate=48000)` reads back as `ffmpeg.audio.raw`.
Deliberate per comment for codec-read params; it also drops script-written
ones. **read only** (dump path not run; cache path checked to keep them).

### FFMPEG_CONTENT.md is stale

Names `src/core/base/stream/` for `Content_base` (now
`src/core/formats/content_base.ml`); blit example wrong (see Defects);
describes adjacency merging. **checked** (paths).

### `make ?length` ignored

All three `Data.make ?length:_` ignore the length; the view layer then holds
a view longer than its data. Root of the first defect. **read only**.

### No keyframe information in copy content

Copy content tracks no keyframe state; the brief's "keyframe tracking in copy
content" is encoder-side. **read only**.

## To verify

### Field numbering shifts when a stream fails to describe

stream_description.ml:177-251: a stream that raises is skipped and the next
one takes its field name. If the decoder maps fields to streams by its own
enumeration, field and stream can disagree. **read only**.

### Data fields offset by subtitle count

stream_description.ml:225: `data_n (rank + subtitle_count)`, so the first
data stream after one subtitle stream is `data_2`, and `data` may never
exist. Reason not found in code or log. **read only**.

### Matroska `profile` metadata

stream_description.ml:43-47 looks up input metadata `profile` for
`matroska*`; the fixtures only ever yield `matroska,webm`. Whether any
demuxer sets `profile` is unverified. **read only** (fixture seen).

### Field names beyond the defaults in annotations

`source(audio_2=ffmpeg.audio.raw(...))` raised `Not_found` from
`Fields.field_of_string` (typecheck/liquidsoap_typecheck.ml:95) instead of a
parse error, apparently because `audio_2` is registered lazily. Generic, not
FFmpeg-specific; noted for the owner. **checked**.

### Stream id counter is not atomic

`Ffmpeg_content_base.stream_idx` is a plain `ref`; decoders on several
domains could hand out the same id. **read only**.
