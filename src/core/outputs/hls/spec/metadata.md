# Metadata and tags

Part A. As built. Two things travel with segments besides media: track
metadata, carried as ID3, and tags the script inserts into the media
playlist. This file owns both, and the extra cuts they cause. How an encoder
builds or carries an ID3 tag is
[encoder-contract.md §7](encoder-contract.md#7-insert_id3) and its
implementations.

## 1. Track metadata

Each variant holds a metadata state: none, **pending** `m`, or **sent** `m`.
When the source's metadata `m` reaches the output
([operator.md §4](operator.md#4-per-frame-loop)), for each variant: if its
state is sent with an equal `m`, nothing; otherwise it becomes pending `m`
(a newer pending replaces an older one). Cover art is not exported.

Metadata is never given to the encoder's in-stream metadata entry; it
reaches the variant only as ID3 (§2).

## 2. ID3

Per variant, `id3_enabled` is decided once by the encoder's `init`
([variants.md §2](variants.md#2-building-a-variant)), from the variant's
`id3` and `id3_version` fields. `replay_id3` defaults to `true`.

When a segment opens ([segmenting.md §4](segmenting.md#4-opening-a-segment))
and `id3_enabled`:

| State     | `replay_id3` | Metadata given | State after |
| --------- | ------------ | -------------- | ----------- |
| pending m | any          | m              | sent m      |
| sent m    | true         | m              | sent m      |
| sent m    | false        | empty          | sent m      |
| none      | any          | empty          | none        |

The encoder's `insert_id3` is called with the segment's start as (frame
number, offset in ticks within that frame) and the metadata list. If it
returns bytes, they are written at the very start of the segment, before
the encoder's header. An encoder may instead carry the tag in its own
stream and return nothing. `insert_id3` is called at every segment open,
also with empty metadata: packed-audio segments always start with a tag
carrying their timestamp
([encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3)).

## 3. Inserted tags

`insert_tag(tag)` (on the output: every variant; on a `streams()` entry: that
variant) trims `tag` and adds it to the variant's pending tags. It may be
called from any thread; concurrent calls are not lost.

When a segment opens, it takes all pending tags and the pending list
becomes empty. The tags are written in that segment's playlist entry,
between `EXTINF` and the URI ([playlists.md §2](playlists.md#2-media-playlist)),
in the order the list holds them, which is the reverse of insertion order.
Pending tags are persisted ([persistence.md](persistence.md)).

Tags are written as given: no check that they are valid playlist lines.

## 4. Extra cuts

A variant has a **reason** to cut early when, checked in this order:

1. `id3_enabled` and its metadata is pending: reason `to insert new
metadata`;
2. it has pending inserted tags: reason `to insert pending extra tags`.

With a reason, the next split point after the open segment's start that is
not due for a grid cut ends the segment there
([segmenting.md §5](segmenting.md#5-handling-a-split) step 3), so the new
segment carries the tag from its start. Without a split point, metadata and
tags wait for the next cut of any kind.
