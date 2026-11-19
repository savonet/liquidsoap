# Composition (Part A)

How a source says how it wants to be entered and left by a
[switch](../../operators/spec/switch/switch.md), and what happens by default.

## 1. Tags

Every source has a composition tag: `file`, `live` or `inherit`.

| Kind of source                                         | Tag       |
| ------------------------------------------------------ | --------- |
| Active input (network, hardware)                       | `live`    |
| Synthesis that never ends a track (endless silence, …) | `live`    |
| Passive input; synthesis of a fixed duration           | `file`    |
| Output                                                 | none      |
| Any other operator                                     | `inherit` |

Rationale for the second row: a rule that waits for a track boundary never
lets go of a source that has none. A silence of last resort MUST be left as
soon as another child can play, with no tag set by the script.

**Resolution** of `inherit`, in order: the tag of the source's effective
source, when that is another source; for a switch with no selection, the tag
of its first child, in list order, that has something to deliver; for a
`source.dynamic` with no current source, the tag of its pending one, or
`file` before its function was ever asked; else
`file` when the source has children and all resolve to `file`; else `live`.

Rationale for the switch rule: a switch that has not selected yet will play
its first available child. Resolving it from all its children would let a
silence of last resort, which is `live`, make a playlist above it `live`
before it has played anything, and a selector over such switches would cut
into tracks.

`composition_type` is a script reference on every non-output source. Reading
it gives the resolved tag, `"file"` or `"live"`. Writing one of these two
strings sets the explicit tag; any other string raises an `invalid` error.

## 2. Profiles

One profile per resolved tag:

| Field             | `file`                                     | `live`     |
| ----------------- | ------------------------------------------ | ---------- |
| `track_sensitive` | true                                       | false      |
| `replay_metadata` | true                                       | true       |
| `on_select`       | default handoff ([§4](#4-default-handoff)) | same       |
| `on_leave`        | when `preempted`: abort the left source    | do nothing |

`source.composition.file(profile)` and `source.composition.live(profile)`
replace a profile for the whole script.

The file `on_leave`, given `preempted = true`, logs that the source was left
in the middle of a track and aborts it
([source-contract.md §6](source-contract.md#6-abort)): the source starts a
fresh track when it is next played, and a selector among the left sources
decides anew. Given `preempted = false` it does nothing.

A source on the live profile is not aborted when it is cut: it resumes where
it was.

What the profiles give for each pair, with the default handoff:

| Leaving → entering        | The switch | Blend                | Leaving track                             |
| ------------------------- | ---------- | -------------------- | ----------------------------------------- |
| `file` → `file`           | waits      | none                 | ended                                     |
| `file` → `live`           | cuts       | fade, PCM audio only | aborted, unless it ran out under the fade |
| `live` → `file` or `live` | cuts       | fade, PCM audio only | untouched                                 |
| nothing to deliver → any  | at once    | none                 | untouched                                 |

A handoff out of a source that is gone never needs data from it.

## 3. Per-source methods

Every non-output source carries `track_sensitive`, `replay_metadata`,
`single`, `on_select` and `on_leave`. Their built-in values read the profile
of the source's resolved tag at each call, so a later change of tag or
profile applies. `single` is false.

A script overrides one for a given use with `s.{method = value}`. A switch
reads the method from the value it was given; when the value does not carry
it, the switch reads the profile of the source, also at each use.

## 4. Default handoff

`source.composition.default_on_select({starting, ending, replay_metadata})`
returns the source to play.

**Metadata replay.** If `replay_metadata` and `starting` has a last
metadata: ask `starting` to insert it on its next sample
([source-contract.md §4](source-contract.md#4-metadata)), and log it.
Metadata the source delivers itself on that sample wins. With
`replay_metadata` false nothing is inserted.

Since a source's last metadata is cleared when its track ends or is aborted,
a replay never announces a track that is over. Since metadata already on the
sample wins, a nested switch that replayed is not replayed over.

**No ending**: return `starting`.

**Ending, content is not exactly one PCM audio field**: return `starting`.
The ending source is cut at once.

**Ending, PCM audio**:

1. `rem` := remaining time of the ending source. `fade` := `rem` if
   `0 < rem <= max_fade`, else `max_fade`, also when `rem` is unknown.
2. The **tail** is `ending`, limited to `max_fade`, faded out over `fade`.
   `ending` already stops at the end of its track
   ([switch.md §6](../../operators/spec/switch/switch.md#6-presented-sources)).
3. The **mix** is `starting` with the tail's audio added, unnormalized.
   Track marks and metadata come from `starting` only. The mix lasts as long
   as the tail delivers.
4. Return a source that plays the mix, then `starting`. The move from mix to
   `starting` is inside a track: no boundary, no sample of `starting` lost,
   repeated or delayed.

`starting` is heard from the first sample of the mix.

Consequences:

- The ending source's leave completes when the tail is over
  ([switch.md §7](../../operators/spec/switch/switch.md#7-leaving)).
- A file source cut by a live one fades under it for at most `max_fade`,
  then is aborted by its `on_leave`. If its track ran out under the fade,
  `preempted` is false and its next track is not skipped.

## 5. Parameters

| Parameter  | Setting                                | Default | Protects                                                      |
| ---------- | -------------------------------------- | ------- | ------------------------------------------------------------- |
| `max_fade` | `settings.source.composition.max_fade` | 1 s     | How long a cut source stays audible and in use after the cut. |
