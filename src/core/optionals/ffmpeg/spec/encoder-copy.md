# Copy stream encoder

This file describes the stream encoder of a `%f.copy` track: it remuxes the
packets carried as `ffmpeg.copy` content without decoding them. It follows the
contract numbering of [encoder.md §12](encoder.md#12-stream-encoder-contract) (§1–§11), and adds §12 on
source switching (this §12 is specific to the copy encoder, not the contract's
§12), keyframe waiting and the per-source state shared by the copy
tracks of one encoder. It relies on the copy content layout (chunks with a
source id, a time base, a length and positioned packets; stream parameters on
the content) of [content.md](content.md), and on the bindings' reserved copy
streams and `write_packet`
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)). Every
timestamp rule is in [timestamps.md §6–§7](timestamps.md#6-copy-streams-per-track).

## 1. Interface

| Operation               | Behaviour                                                                    |
| ----------------------- | ---------------------------------------------------------------------------- |
| `mk_stream`             | initialises the reserved stream (§2)                                         |
| `encode`                | §3–§7                                                                        |
| `flush`                 | nothing                                                                      |
| `codec_attr`, `bitrate` | from the stream, set at `mk_stream`; none before                             |
| `video_size`            | width and height of the video parameters, set at `mk_stream`; none otherwise |

Options: the track `keyframe` choice only (`wait_for_keyframe`, the default, or
`ignore_keyframe`, [encoder-format.md §4](encoder-format.md#4-track-arguments-common-rules)).

## 2. Stream creation

At build, a stream is reserved in the container, so its index follows track
order. At `mk_stream frame` (the container's first frame):

1. The field's content parameters are read. Content with no parameters makes
   this fail with an OCaml `Invalid_argument "option is None"`
   ([findings](findings/encoder.md)).
2. The reserved stream is initialised with the codec parameters (audio, video
   or subtitle). For video, the content's average frame rate is set on the
   stream and the width and height are kept for `video_size`.
3. `intra_only` = the codec descriptor's intra-only property; it stays true
   when the codec has no descriptor. It starts true before `mk_stream`.

The stream keeps these parameters for the container's lifetime. Packets from a
later source with other parameters are written to it as they are (§12.7).

## 3. Input

Each frame of positive length: every chunk of the field, in order. A chunk
with no packets only updates the stream head (§12.4). A chunk with packets:
each `(position, packet)` in order goes through §12.2 (accept or drop), then
§5–§7. A packet of another media kind than the stream is an assertion failure.

## 4. Conversion

None. Each accepted packet is duplicated; the duplicate gets new pts, dts and
duration ([timestamps.md §6](timestamps.md#6-copy-streams-per-track)) and is written. Flags and data are
kept.

## 5. Timestamps

[timestamps.md §6](timestamps.md#6-copy-streams-per-track) (per stream) and
[§7](timestamps.md#7-copy-streams-across-tracks) (across streams).

## 6. Writing

`write_packet` with the main tick time base `1/main_rate`; the bindings rescale
to the stream's time base and interleave or not per the container's
`interleaved` ([encoder.md §3](encoder.md#3-building-an-instance)). Errors propagate out of
`encode`. Example (from the implementation): switching an mpegts output from an
mp4-sourced H.264 stream to a TS-sourced one fails in the muxer's bitstream
filter with `Invalid data found when processing input`, and the output stops.

## 7. Keyframes

For an accepted packet that is a key packet, or any packet when `intra_only`:
if the track has an HLS hook, the muxer is flushed (not for intra-only), then
the hook runs ([encoder.md §8.2](encoder.md#82-keyframe-hooks)). This happens before the packet
is written.

## 8. Flush and end

Nothing is held: `flush` does nothing. Packets are in the muxer once written.

## 9. Properties

§1.

## 10. Options and errors

No codec options. Errors: §2 step 1, §3 (assertion), muxer errors (§6).

## 11. Logs

Logger `ffmpeg.copy.encoder`:

| Level     | Message                                   | When                                                      |
| --------- | ----------------------------------------- | --------------------------------------------------------- |
| important | `Packet has no dts!`                      | an accepted packet without dts; it is written             |
| important | `Dropping packet with non-monotomic dts!` | [timestamps.md §6.3](timestamps.md#63-monotonicity-check) |

Packets dropped while waiting for a keyframe or readiness (§12.2) are dropped
silently.

## 12. Source switching and shared per-source state

### 12.1 Model

Each packet chunk carries a **source id** (`stream_idx`); who allocates one
and when is in [content.md §5](content.md#5-stream_idx-and-time_base). The file decoder allocates a new
one at every seek, so a seek, like a reconnect, is a source switch: the track
rebases its timestamps ([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)) and, with
`wait_for_keyframe` and a codec that is not intra-only, waits for a keyframe
(§12.2). A change of source id in a field is a **source
switch** for that copy track. Ids start at 1; id 0 is used by the encoder as a
"no source yet" placeholder.

The encoder keeps, per source id, one shared **source record**:

| Field         | Meaning                                                                                     |
| ------------- | ------------------------------------------------------------------------------------------- |
| `idx`         | the source id                                                                               |
| `last_start`  | the offset added to this source's timestamps, shared by every copy track on it (main ticks) |
| `position`    | the greatest `adjusted dts + duration` written from this source by any track (main ticks)   |
| `ready_count` | number of contributions (§12.3)                                                             |
| `ready`       | the readiness flag read by §12.2                                                            |

and a constant `total_count` = **the number of copy tracks in the format**,
whatever source they come from. Drop and encoded tracks do not count.

Per copy track the state is: `current` (the source record it is on, initially
a placeholder record for id 0 with `last_start` = −2⁶³, not ready),
`started` (false), `waiting` (false), `last_dts` (none), `last_position` (0),
`head` (none, §12.4).

### 12.2 Accepting a packet

For a packet with source id `s`:

1. If `s` differs from `current`'s id, the track switches:
   1. `waiting` = (`copy_opt` is `wait_for_keyframe`) and not `intra_only`.
   2. `current` is released (§12.5).
   3. `last_position` = `current.position` at this moment.
   4. The record for `s` is looked up or created with `last_start` = −2⁶³ and
      a contribution when not `waiting` (§12.3).
   5. **That record's `ready` is then set to `not waiting`**, overriding §12.3.
   6. `current` = that record; `started` = false.
2. Accept: when `waiting`, iff the packet is a key packet; otherwise iff
   `started` or `current.ready`.

A rejected packet is dropped silently: no timestamp, position or head update.

On the first accepted packet of a source (`started` false), the track begins
the source ([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)): it looks the record up again
with its computed `last_start` and a contribution when it **was** waiting, then
sets `started` = true and `waiting` = false.

### 12.3 Contributions and readiness

Looking up a record with a contribution:

1. If no record for the id is held (§12.5), a fresh one is made: `ready_count`
   0, `last_start` as given, `position` 0, `ready` false.
2. A contribution adds 1 to `ready_count`.
3. If `ready_count` = `total_count`, `ready` becomes true (equality only).
4. `last_start` = max(`last_start`, given).

Each track contributes once per source: at the switch when not waiting, at its
first accepted packet when waiting.

The intent stated beside this code is: when several copy tracks come from the
same source, a switch waits until all of them are ready, so that no audio
starts before the video's first keyframe; tracks from different sources do not
wait for each other. What happens, by case (with `wait_for_keyframe`, a video
codec that is not intra-only and an audio codec that is):

| Case                                                                                                                                               | Video track               | Audio track                                                                                         |
| -------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------- | --------------------------------------------------------------------------------------------------- |
| same source, audio switches first in the frame                                                                                                     | waits for its keyframe    | sets `ready` true at its switch and starts at once                                                  |
| same source, video switches first                                                                                                                  | sets `ready` false, waits | sets `ready` true at its switch and starts at once                                                  |
| different sources                                                                                                                                  | waits for its keyframe    | starts at once; the count of its source never reaches `total_count`, so only step 1.5 lets it start |
| both `ignore_keyframe`                                                                                                                             | starts at once            | starts at once                                                                                      |
| non-waiting track whose first packet of the source is dropped by [timestamps.md §6.3](timestamps.md#63-monotonicity-check), sibling switched since | —                         | waits until the count reaches `total_count`                                                         |

So the readiness count holds a non-waiting track back only in the last row.
Within a source, an intra-only track does not wait for its siblings' keyframes
(observed, from the implementation: two mp4 files, AAC + H.264, the second
cut 0.6 s before a keyframe; the audio of the second source is muxed from the
switch while the video waits 0.6 s). Field order decides who switches first
([encoder.md §5](encoder.md#5-encoding-a-frame)). Reason for step 1.5 (history, #3841 /
GH3840): with audio and video from different sources the count never reached
`total_count` and nothing was ever written.

### 12.4 Stream head of a sparse source

`head` records the time a source spends with no packets before its first one,
per track:

| Chunk         | `head` before    | `head` after                                                      |
| ------------- | ---------------- | ----------------------------------------------------------------- |
| empty, id `s` | `Started s`      | unchanged                                                         |
| empty, id `s` | `Pending (s, n)` | `Pending (s, n + chunk length)`                                   |
| empty, id `s` | anything else    | `Pending (s, chunk length)`                                       |
| with packets  | any              | unchanged until the first accepted packet, which sets `Started s` |

Its use is in [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase). Empty chunks of the
**previous** source after its last packet change nothing, so trailing silence
of a sparse track is not carried over.

### 12.5 Release and lifetime of source records

Releasing a record (at a switch):

- if `ready_count` ≤ 1: the record is removed from the store;
- else `ready_count` decreases by 1.

The count can be 0 at release (a track that switched while waiting and left
before its keyframe never contributed); removing at ≤ 1 keeps it from going
negative, which would leave the set permanently unready (code comment).

Observable lifetime: a source record exists while at least one copy track has
it as `current` and it has not been removed by a release. A track that looks up
an id whose record is gone gets a fresh record, with no `last_start`, no
`position` and no readiness from its siblings. This happens when a sibling
releases the record at ≤ 1 while another track still has it as `current` and
has not begun (it gets a fresh record at its first accepted packet), and when
a record no track holds is forgotten, at an unspecified later time. The store belongs to the encoder
instance and survives an Ogg container restart ([encoder.md §6](encoder.md#6-metadata));
records of the previous container's tracks may then still be found by the new
tracks until reclaimed. Part B is in
[language-notes/encoder.md](language-notes/encoder.md).

### 12.6 Reconnects and new decoders

A reconnect ([io.md §13](io.md#13-timestamps-across-reconnects)) and a seek in the file decoder
([decoder.md §12](decoder.md#12-seeking)) allocate a new source id, so for a copy track
each is a source switch: §12.2 applies (keyframe wait when configured) and
timestamps are rebased ([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)).

### 12.7 Parameters across sources

The stream's codec parameters are those of the first frame (§2). A new source
with different parameters (other codec, other extradata, other packetisation)
is muxed with the old parameters; nothing checks or logs it.
