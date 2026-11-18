# The transition function, as built (Part A)

What `cross` hands the script function it was given, and what it does with
the source that comes back. When it is called is
[cross.md §7](cross.md#7-building-the-compound).

## 1. Arguments

`transition(ending, starting)` returns a source. Each argument has three
fields:

| Field      | `ending`                                             | `starting`                                                |
| ---------- | ---------------------------------------------------- | --------------------------------------------------------- |
| `source`   | The ending source: the last `X` of the ending track. | The starting source: the first `S` of the starting track. |
| `db_level` | The ending level, in dB.                             | The starting level, in dB.                                |
| `metadata` | The ending metadata, adjusted.                       | The starting metadata, adjusted.                          |

- Both sources are finite buffers, not yet woken, in the clock of `cross`.
  Each reports its exact length as its remaining time and declares
  exhaustion when it is empty.
- The data keeps its marks and metadata. The starting source normally
  begins with the mark and the metadata of its track. The ending source
  carries those of its track only when the whole track fits in `X`.
- A side with no metadata gets an empty one. A level with nothing analysed
  is not a number.
- The function runs inside a probe of `cross`
  ([cross.md §5](cross.md#5-probe)), once per crossing. It is also called
  with an empty starting source where [cross.md §4](cross.md#4-taking-from-the-child)
  finds an end piece and nothing follows.

## 2. Metadata adjustments

Applied to the copies kept by `cross`
([cross.md §9](cross.md#9-metadata-kept)) before the call. Values are
written as decimal floats in the form `3.`, `1.98`, `0.`.

**Ending side**, for a window `w` = `X` in seconds. Only when
`liq_fade_out` is present:

- not a float: log (1), nothing else;
- `liq_fade_out < w`: `liq_fade_out_delay` ← `w − liq_fade_out`. Log (2);
- `w < liq_fade_out`: `liq_fade_out` ← `w`, `liq_fade_out_delay` ← `0.`.
  Log (3);
- then, when `liq_fade_out_start_next` is a float: it becomes
  `max(0, start_next + w − fade_out − delay)`, with `fade_out` and `delay`
  as they were before the adjustment, `delay` counting as zero when missing.

Reason, from the history: `liq_fade_out_start_next` is relative to the start
of a window of `delay + fade_out` ending where the buffer ends; when the
window is stretched to `w`, the value moves so that it stays at the same
place in the track.

**Starting side**, for a window `w` = `S` in seconds. Only when
`liq_fade_in` is present:

- not a float: log (1), for `liq_fade_in`;
- `w < liq_fade_in`: `liq_fade_in` ← `w`. Log (3), for `liq_fade_in` (F8).

Logs: (1) at level 3, (2) and (3) at level 4, amounts with two decimals.

```
(1) Invalid non-float value for `liq_fade_out`: <value>
(2) Setting `liq_fade_out_delay` to <delay> to make sure `fade.out` ends at the end of the buffered data.
(3) Dropping <amount> from `liq_fade_out` to match the buffered data.
```

The starting side also sets how much is buffered: the fade-in window of
[cross.md §5](cross.md#the-ending-track-is-over). Reason, from the history:
the starting track's fade-in is buffered in full, whatever the ending
duration.

**Worked examples**, from runs, with `F` = 0.02 s.

`duration` = 5; ending track with `liq_fade_out = "3."`,
`liq_fade_out_start_next = "1."`; starting track with `liq_fade_in = "7."`,
`liq_fade_in_delay = "1."`. `Lb` = 4.98 s, `La` = 8.00 s, `X` = 4.98 s,
`S` = 8.00 s.

```
ending:   liq_fade_out = "3."  liq_fade_out_delay = "1.98"  liq_fade_out_start_next = "2.98"
starting: liq_fade_in = "7."   liq_fade_in_delay = "1."
```

`duration` = 2; ending track with `liq_fade_out = "3."`,
`liq_fade_out_delay = "1."`, `liq_fade_out_start_next = "2.5"`. `X` =
1.98 s.

```
ending:   liq_fade_out = "1.98"  liq_fade_out_delay = "0."  liq_fade_out_start_next = "0.48"
```

## 3. The result

- The source returned is unified with the type of `cross`: it must produce
  what `cross` produces.
- It is placed in the clock of `cross` and woken by `cross`, alone or inside
  a sequence ([cross.md §7](cross.md#7-building-the-compound), step 8).
- It need not read either argument. Data of an argument it does not read is
  never played.
- `cross` reports no sync source on its behalf.

## 4. End of the transition

`cross` has no notion of the transition's length. It leaves the phase
`after` when the compound, probed, is not ready
([cross.md §5](cross.md#5-probe)).

- **No tail.** The compound is the transition source, or a head followed by
  it. It is over the first cycle the transition source is not ready, for
  whatever reason.
- **With a tail.** The compound is a sequence of the transition source and
  the tail, in which an item is finished only when it declares exhaustion.
  By the rule of the rebuilt sequence, an item that has nothing to deliver
  and is not exhausted is passed over when the last item can deliver. So the
  tail starts when the transition source is exhausted, or the first cycle it
  is not ready. Track ends inside the transition source do not finish it.
  The tail is entered as is.
- A transition source that never stops being ready is played for ever. The
  child is not read meanwhile (F13).

While the compound plays, the child is not read at all: buffering resumes
when the compound is over.

## 5. Callbacks

Callbacks the function registers, while it runs, on the ending source, on
the starting source or on the child of `cross` are collected. They are
released when the compound is: at the next crossing, at the next start of
buffering, on an abort in `after`. They are not released at sleep.

## 6. Shapes in use

What the tests and the standard library return, and where tracks begin as a
result. `a` and `b` are the two sources.

| Returned                          | Tracks in a crossing                                                                                                                           |
| --------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `add([a, b])`, faded or not       | One, on the first sample of the compound. The sum carries no mark.                                                                             |
| `sequence([a, b])`                | Two: the compound's first sample, then the join, which the sequence marks.                                                                     |
| `sequence([a, x, y, b])`          | One per item.                                                                                                                                  |
| `a` alone                         | One. The starting track is heard from the tail on; its metadata is replayed there ([script-layer.md §1](script-layer.md#1-the-cross-wrapper)). |
| a source unrelated to `a` and `b` | One, with no metadata.                                                                                                                         |
