# Persisted state

Part A. As built. With `persist_at`, the output saves its playlist state at
`stop` and restores it at `start`, so a restarted process continues the same
playlists with the segments already on disk. This file owns the state file's
format and the save and restore protocols.

## 1. What is saved

Per variant: its name, its next position number, its pending inserted tags,
its discontinuity count. Per variant, its list of closed segments, oldest
first; per segment: position number, discontinuous flag, discontinuity
count, init segment name, file path, inserted tags, length.

Not saved: media time, the grid boundary, split points, metadata state, the
encoder, the open segment (it is closed before saving).

## 2. Format

UTF-8 JSON, written indented (not compact), without JSON5 extensions.

```
{
  "streams": [ <stream>, ... ],          one per variant, in variant order
  "segments": { "<variant name>": [ <segment>, ... ], ... }
}
<stream>  = { "name": string, "position": int,
              "pending_extra_tags": [string, ...], "discontinuity_count": int }
<segment> = { "id": int, "discontinuous": bool, "current_discontinuity": int,
              "init_filename": string,      optional
              "filename": string,           optional
              "extra_tags": [string, ...], "len": int }
```

- `position`: the next position number to give.
- `filename`: the full path of the published file, output directory
  included, as the process saw it.
- `init_filename`: the init segment's name as returned by `segment_name`,
  relative.
- `len`: main ticks ([segmenting.md §1](segmenting.md#1-segment-duration-and-the-grid)).
  The unit depends on the process's audio and video settings, which are not
  saved (finding F-PER-3).
- An absent optional key means none. Writers omit it. Readers also accept
  `null`, which older writers produced (seen in the reader).

Example, written by a run (`segments=3`, `segments_overhead=1`, one MPEG-TS
variant `a` and one fMP4 variant `m`; one entry per list shown, whitespace
reflowed, key order as written):

```json
{
  "streams": [
    {
      "name": "a",
      "position": 7,
      "pending_extra_tags": [],
      "discontinuity_count": 0
    },
    {
      "name": "m",
      "position": 7,
      "pending_extra_tags": [],
      "discontinuity_count": 0
    }
  ],
  "segments": {
    "a": [
      {
        "id": 6,
        "discontinuous": false,
        "current_discontinuity": 0,
        "filename": "/var/tmp/out/a_6.ts",
        "extra_tags": [],
        "len": 882
      }
    ],
    "m": [
      {
        "id": 6,
        "discontinuous": false,
        "current_discontinuity": 0,
        "init_filename": "m_0.mp4",
        "filename": "/var/tmp/out/m_6.m4s",
        "extra_tags": [],
        "len": 882
      }
    ]
  }
}
```

The last segment of each list is often a stub (here 882 ticks, 0.02 s): the
encoder's final bytes at `stop` close a segment of their own.

### 2.1 Reading rules

- The top-level object must have exactly the keys `streams` then `segments`,
  in that order.
- Each `<stream>` must have exactly its four keys, in the order shown.
- `<segment>` keys are looked up by name; unknown keys are ignored; a
  missing required key or a wrong type is invalid.

## 3. Saving

At `stop` with `persist_at`, after every open segment is closed
([operator.md §3.2](operator.md#32-stop)): the file is opened for writing
(created with the default mode under the umask, or truncated), the JSON is
written, and the file is closed. The write is not atomic and fires no event
(finding F-PER-1). An exception while closing a segment during `stop` skips
the save.

## 4. Restoring

At `start`, if the file exists:

1. Level-4 log `Resuming from saved state`.
2. The file is parsed (§2.1).
3. The saved streams are matched to the variants **by position**: a
   different count, or a different name at the same position, fails.
4. For each variant, in order: pending tags and discontinuity count are set;
   the init position becomes the saved `position`; the next position number
   becomes the saved `position` plus one. One position number is skipped
   (finding F-PL-1, checked: 6 then 8).
5. The lists of closed segments are replaced by the saved ones. Their
   segments are closed; their files are expected on disk.
6. The file is deleted; a failure is ignored.
7. Every variant is marked to start its next segment with a discontinuity.

The init segment of the new run is written under the new init position
([files.md §6](files.md#6-init-segments)); the first new segment is
discontinuous, so its playlist entry carries `EXT-X-MAP` with it.

**Failure**, at any step up to 6: with `strict_persist` false, level-4 log
`Failed to resume from saved state: <exn>`, and the output continues. The
effects of the steps already done stay (step 4 can be half applied), and the
file stays, so it is tried again at the next `start`. With `strict_persist`
true, the exception propagates out of `start`.

A `persist_at` that never existed, or was deleted by a successful restore,
is a fresh start.
