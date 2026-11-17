# Composition (Part A, as-built)

How a source says how it wants to be entered and left, and what happens by
default.

## 1. Tags

Every source has a composition tag: `file`, `live` or `inherit`.

The tag set at creation depends on the kind of source:

| Kind                             | Tag       |
| -------------------------------- | --------- |
| Active input (network, hardware) | `live`    |
| Passive input, synthesis         | `file`    |
| Output                           | none      |
| Any other operator               | `inherit` |

**Resolution** of `inherit`, in order: the tag of the source's effective
source, when that is another source; else `file` when the source has children
and all resolve to `file`; else `live`.

`composition_type` is a script reference on every non-output source. Reading
it gives the resolved tag, `"file"` or `"live"`. Writing one of these two
strings sets the explicit tag; any other string raises an `invalid` error.

## 2. Profiles

One profile per resolved tag. A profile is a record:

| Field             | `file`                                    | `live`     |
| ----------------- | ----------------------------------------- | ---------- |
| `track_sensitive` | true                                      | false      |
| `replay_metadata` | true                                      | true       |
| `on_select`       | default handoff (§4)                      | same       |
| `on_leave`        | when preempted: clear last metadata, skip | do nothing |

`source.composition.file(profile)` and `source.composition.live(profile)`
replace a profile for the whole script. The values above are installed by the
standard library. Before it loads, both profiles do nothing on leave and return
`starting` on select.

File `on_leave`, when its argument has `track_sensitive = false`: logs "Source
left in the middle of a track: skipping current track", clears the last
metadata of the left source, and skips it (abort track). With
`track_sensitive = true` it does nothing.

## 3. Per-source methods

Every non-output source carries `track_sensitive`, `replay_metadata`,
`single`, `on_select` and `on_leave`. Their built-in values read the profile
of the source's resolved tag **at each call**, so a later change of tag or
profile applies. `single` is false.

A script overrides one for a given use with `s.{method = value}`. A switch
reads the method from the value it was given; when the value does not carry
it, the switch reads the profile of the source, also at each use.

## 4. Default handoff

`source.composition.default_on_select({starting, ending, replay_metadata})`
returns the source to play.

**Metadata replay.** If `replay_metadata` and `starting` has a last metadata:
ask `starting` to insert it in its next frame, with override off, and log
"inserting latest metadata". Otherwise log "no latest metadata to insert".
Metadata the source provides itself at that point wins.

**No ending** (boundary): return `starting`.

**Ending, content is not exactly one PCM audio field named `audio`**: return
`starting`. The ending source is dropped at once.

**Ending, PCM audio**:

1. `rem` := remaining time of the ending source. `fade` := `rem` if
   `0 < rem <= max_fade`, else `max_fade`. Log the fade duration.
2. The **tail** is the ending source, limited to its current track, limited to
   `max_fade`, faded out over `fade`.
3. The **mix** is `starting` with the tail's audio added to its audio,
   unnormalized. Track marks and metadata come from `starting` only. The mix
   is available while the tail is ready.
4. Return a source that plays the mix while it is available, then `starting`.
   The move from mix to `starting` happens at a frame boundary and is merged:
   it is not a new track.

`starting` is read from the first frame of the mix.

`settings.source.composition.max_fade`: seconds, default 1.

## 5. What the handoff means for the switch

- The ending proxy stays up while the mix reads it. That is the transition in
  progress ([switch.md §6](switch.md#6-get_source)). The ending source's
  `on_leave` runs when the mix lets go of it
  ([switch.md §10](switch.md#10-leaving)).
- A file source cut by a live one is faded under it, then skipped by its
  `on_leave`: next time it starts a fresh track.
- A live source that goes away is not ready any more: the switch is at a
  boundary and there is no ending.
