# Helpers built on the seam (Part A)

Script-library operators built on [`sequence`](sequence.md) and
[`source.dynamic`](dynamic.md). They are specified here by where their
tracks start; construction is free.

**Default rule.** A track mark and the metadata that goes with it sit on the
logical start of a unit of content, and nowhere else
([source-contract.md §3](source-contract.md#3-tracks)). Over file-based
content a helper keeps the marks and metadata as the content gives them. A
helper presents several units as one track only when the script asks for it
with `merge`.

## 1. Once

`once(s)`: one track of `s`, then nothing.

- `s` is waited for: a source that is not available yet is played when it
  is.
- After the track's end was taken, the operator is exhausted.
- `once` over a source that never ends a track never ends.

## 2. Delayed fade-in

A fade-in with an initial delay `d` over `s`: `d` of silence, then `s`
faded in. The silence is there so that the result can be mixed with another
source from the start; it is lead-in, not a track.

- The silence carries no mark and no metadata.
- The first sample of `s` carries one mark and the metadata of `s`'s track.
- So one track is reported, where `s` becomes audible.
- With `d = 0` there is no silence and the first sample of `s` is delivered
  first.
- `s` is not read during the delay.

## 3. Prepend

`prepend(~merge, s, f)`: before each track of `s`, play the track that `f`
builds from that track's metadata.

- `merge` false (default): two tracks. A mark, with the prepended track's
  metadata when it has some, where the prepended track starts; a mark, with
  the main track's metadata, where the main track starts.
- `merge` true: one track. A mark where the prepended track starts; the main
  track's metadata is announced at the join as metadata, with no boundary.
- When `f` returns nothing the main track plays as it is.
- The prepended source is waited for like any item
  ([sequence.md §3](sequence.md#3-items)).

## 4. Append

`append(~merge, s, f)`: after each track of `s`, play the track that `f`
builds from that track's metadata, then go on with `s`.

- `merge` false (default): the appended track is a track of its own,
  carrying only its own metadata.
- `merge` true: main track and appended track are one track. A mark where
  each main track starts; none where the appended track starts; the appended
  track's metadata is announced at the join as metadata.
- When `f` returns nothing the next main track follows.

## 5. Stream start

Whether a stream starts with a mark depends on its content, by the default
rule, and never on whether a helper wraps its result in another operator.
