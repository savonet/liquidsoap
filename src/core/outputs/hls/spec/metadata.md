# Metadata and tags

Part A. Two things travel with segments besides media: track metadata,
carried as ID3, and tags the script inserts into media playlists. This file
owns both, and the cuts metadata may cause. How an encoder builds or carries
an ID3 tag is [encoder-contract.md §7](encoder-contract.md#7-insert_id3) and
its implementations.

## 1. Track metadata

Each variant holds a metadata state: none, **pending** `m`, or **sent** `m`.
When metadata `m` reaches the output ([operator.md §4](operator.md#4-per-frame-loop)),
for each variant: if its state is sent `m'` with `m'` equal to `m` as
exported, nothing; otherwise it becomes pending `m`, replacing an older
pending value.

Equality compares the exported entries (§2), not the raw metadata. Cover
art and other binary entries are not exported, and the output MUST NOT keep
them: kept metadata lives for the whole run.

Metadata reaches the encoder only through `insert_id3`. Metadata that
reaches the output before media time 0 of a run belongs to the first
segment.

## 2. ID3

Per variant, the encoder's `init` decides the ID3 mode from the variant's
`id3` and `id3_version` fields
([encoder-contract.md §3](encoder-contract.md#3-init)):

- **none**: no ID3;
- **in-stream**: the encoder carries tags inside its container, at a media
  time (MPEG-TS timed ID3);
- **prepended**: a tag is written at the start of every segment (packed
  audio, RFC 8216 §3.4).

Exported entries are the text entries that map to ID3 frames. `replay_id3`
defaults to `true`.

**In-stream.** When a variant's state becomes pending `m`, `insert_id3` is
called with the media time `t` of the chunk that carried `m`, after any cut
made at `t`, and the state becomes sent `m`: the tag is on time whether or
not a cut follows. When a segment opens with sent `m` and `replay_id3` true,
`insert_id3` is called with the segment's start and `m` if that start is
after the variant's last in-stream tag: a player joining at that segment
sees the current track. Reason: in-stream tag times MUST strictly increase.
A cut can land at a split point before the last tag's time, and a replay
there would label earlier media with the new track, and a muxer drops a tag
whose time goes back.

**Prepended.** When a segment opens, `insert_id3` is called with the
segment's start and:

| State     | `replay_id3` | Metadata given | State after |
| --------- | ------------ | -------------- | ----------- |
| pending m | any          | m              | sent m      |
| sent m    | true         | m              | sent m      |
| sent m    | false        | empty          | sent m      |
| none      | any          | empty          | none        |

The returned tag starts the segment, before the encoder's header. It is
returned also for empty metadata: it carries the segment's timestamp.

## 3. Inserted tags

`insert_tag(tag)` (on the output: every variant; on a `streams()` entry:
that variant) trims `tag` and adds it to the variant's pending tags. It may
be called from any thread; concurrent calls MUST NOT be lost.

A **playlist tag** is one line starting with `#`. `insert_tag` rejects
anything else with an `error.invalid` runtime error, which the script can
catch, and queues nothing; the output's
and the variants' `extra_tags` are checked the same way at construction.
Reason: a tag is written into the playlist as is, and a second line in it
would be read as a URI.

When a segment opens, it takes every pending tag, in insertion order, and
the pending list becomes empty; a dropped segment gives them back
([segmenting.md §6](segmenting.md#6-opening-a-segment)). The tags are
written in that segment's playlist entry
([playlists.md §2](playlists.md#2-media-playlist)). Inserted tags may
cause a cut (§4); otherwise they wait for the next segment, at most one
segment duration.
Pending tags, and the tags of the open segment at a stop, are kept
([persistence.md](persistence.md), [operator.md §3](operator.md#3-lifecycle)).

## 4. Cuts on metadata and tags

A cut on metadata makes a new segment start at the metadata's media time,
so that a prepended tag, or a player joining at that segment, has the new
track from the start. A cut on an inserted tag makes the tag's segment
start when it is inserted, so a cue or a marker is on time. Both are sync
cuts ([segmenting.md §7](segmenting.md#7-output-wide-cuts)), not
discontinuities: at the media time of the chunk that carried the metadata,
or of the next chunk after `insert_tag`.

The **alignment condition** holds when the output has one variant, or when
every variant is free-split ([segmenting.md §2](segmenting.md#2-segments-in-memory)).
Then a sync cut keeps the grid aligned across variants.

`cut_on_metadata` decides for both:

- `null` (default): a metadata cut when the alignment condition holds and
  some variant's ID3 mode is not none (a cut no tag uses only makes a short
  segment); a tag cut when the alignment condition holds.
- `true`: always cut. With variants that are not free-split, segment
  boundaries and counts can then differ between variants
  ([segmenting.md §7](segmenting.md#7-output-wide-cuts), alignment).
- `false`: never cut.

A tag inserted on one variant (a `streams()` entry) cuts only when the
output has one variant: a sync cut for it would cut every variant.

The decision is made when the metadata or the tag arrives, from the
encoders' answers at that time. Metadata equal to what was sent (§1) causes
no cut. Before every variant's encoder has returned its init segment, no cut
is made: the metadata or tag belongs to the first segment.

Without a cut, metadata and tags wait for the next segment: in-stream ID3 is
still on time; a prepended ID3 tag or an inserted tag comes at most one
segment duration late.
