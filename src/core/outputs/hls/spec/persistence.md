# Persisted state

Part A. With `persist_at`, the output keeps its playlist state in a file, so
a restarted process continues the same playlists with the segments already
on disk. This file owns the state file's format and the save and restore
protocols.

## 1. What is saved

Per variant: its name, its counters (next position number, next init
position, discontinuity count, pending inserted tags) and its retained
segments, oldest first. Per segment: position number, discontinuity flag,
discontinuity value, forced flag, codec string, init segment name, file
name, inserted tags, length. Also the position number of the first segment
of each variant's window.

Not saved: media time and the grid (each run has its own,
[segmenting.md §1](segmenting.md#1-time-the-grid-and-the-target-duration)),
split points, metadata state, encoders, open segments.

## 2. Format

UTF-8 JSON:

```
{
  "variants": [ <variant>, ... ]
}
<variant> = { "name": string, "next_position": int,
              "next_init_position": int, "discontinuity_count": int,
              "window_start": int, "pending_tags": [string, ...],
              "segments": [ <segment>, ... ] }
<segment> = { "position": int, "discontinuous": bool,
              "discontinuity": int, "forced": bool,
              "codecs": string,         optional
              "init": string,           optional
              "name": string, "tags": [string, ...], "duration": float }
```

- Names are relative to the output directory, as in
  [files.md §2](files.md#2-file-names).
- `duration` is in seconds, with at least microsecond precision. Reason: a
  length in internal ticks depends on settings the file does not carry.
- An absent optional key means none. Unknown keys are ignored; key order is
  not significant. Every count is a non-negative integer.

## 3. Saving

With `persist_at`, the state is saved after every segment close
([files.md §4](files.md#4-closing-a-segment)) and at every stop
([operator.md §3.2](operator.md#32-stop)):

1. The JSON is written to a temporary file in the state file's directory.
2. It is renamed onto `persist_at`.

A crash at any point leaves the previous state file or the new one,
complete. A save that fails is logged as a warning and does not end the
run. No file-change event fires. Reason for saving at every close: a crash
then resumes from the last published segment, and numbering never goes
back over files that players have seen.

## 4. Restoring

At the first start after a wake-up ([operator.md §3](operator.md#3-lifecycle)),
when the file exists:

1. **Parse and check** the file as a whole: the format of §2; the same set
   of variant names as the output; for each variant, positions strictly
   increasing and below `next_position`, discontinuity values non-decreasing
   and at most `discontinuity_count`, init names below
   `next_init_position` in their position, every name valid by
   [files.md §2](files.md#2-file-names).
2. A failed check applies nothing. With `strict_persist` true, the output
   fails, without `reopen_on_error`: a bad state file does not heal by
   itself. Otherwise a warning gives the reason and the output starts
   fresh.
3. **Apply**: each variant's counters and window start are taken as saved.
   A listed segment whose file, or whose init segment, is missing is
   dropped with a warning naming it, together with every older segment of
   that variant: playlists number segments by their place, so a hole would
   renumber every segment after it. The others are retained and tracked;
   the window start moves forward past the dropped ones.
4. The run starts with a discontinuity in every variant
   ([operator.md §3.1](operator.md#31-start)). Position numbers continue at
   `next_position`, so the media sequence does not go back.

Reason for dropping missing segments rather than failing: a segment deleted
by hand, or by retention after the last save, must not cost the counters
that keep numbering monotonic.

A `persist_at` that does not exist is a fresh start. The file stays and is
replaced at the next save.

There is one format, with no reader for any other. What is not promised: a
state file written in another format fails the check, so after such a
change a persistent output with `strict_persist` false starts its numbering
again at 1 once, and with `strict_persist` true fails at every start until
the file is removed.
