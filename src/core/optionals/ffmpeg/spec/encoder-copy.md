# Copy stream encoder

This file owns the stream encoder of a `%f.copy` track, which remuxes the
packets carried as `ffmpeg.copy` content without decoding them. It answers
every clause of the stream-encoder contract of
[encoder.md §12](encoder.md#12-stream-encoder-contract) with the same
numbering (§1–§11). Its own §12 owns source switching for copied tracks:
which tracks switch together, keyframe readiness, what is held or dropped
while a switch waits, the lifetime of the state kept per source, and the
check of codec parameters at a switch. It relies on the copy content layout
of [content.md](content.md) (runs with a source id, a time base, a length and
positioned packets; codec parameters on each run), and on the bindings'
copy streams and `write_packet`
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).
Every timestamp rule is in [timestamps.md §6–§7](timestamps.md#6-copy-streams-per-track).

## 1. Interface

| Operation               | Behaviour                                                                 |
| ----------------------- | ------------------------------------------------------------------------- |
| `mk_stream`             | creates the stream (§2)                                                   |
| `encode`                | §3–§7 and §12                                                             |
| `flush`                 | §8                                                                        |
| `request_keyframe`      | ignored (§7)                                                              |
| `codec_attr`, `bitrate` | from the stream, once §2 has run; none before                             |
| `video_size`            | width and height of the video parameters, once §2 has run; none otherwise |

Options: the track `keyframe` choice only (`wait_for_keyframe`, the default,
or `ignore_keyframe`,
[encoder-format.md §4](encoder-format.md#4-track-arguments-common-rules)).

## 2. Stream creation

The stream is created at `mk_stream`, in track order with the other
streams, at the container's first frame: the first frame in which every
copy field has a run with known parameters
([encoder.md §5](encoder.md#5-encoding-a-frame),
[§12.2](encoder.md#122-stream-creation)). Frames before it write nothing for
any stream.

1. The codec parameters are those of the field's first run with known
   parameters in that frame ([content.md §4.2](content.md#42-run-layer),
   [§3.1](content.md#31-ffmpegcopy)). Copy content with packets and no
   known parameters does not exist ([content.md §4.3](content.md#43-invalid-content)).
2. The stream receives the codec parameters (audio, video or subtitle). For
   video, the content's average frame rate is set on the stream, and the width
   and height are kept for `video_size`.
3. The track records whether the codec is **intra-only**: the codec
   descriptor's intra-only property. A codec with no descriptor counts as
   intra-only, so that a track never waits for a key flag its codec may never
   set. A subtitle track counts as intra-only: each subtitle packet stands
   alone.
4. When the muxer wants codec headers out of band (FFmpeg's global-header
   format flag) and the parameters carry no extradata, the track logs a
   warning naming the track and the fixes: `flags="+global_header"` on the
   encoder that produced the packets, or the
   `ffmpeg.filter.bitstream.extract_extradata` bitstream filter upstream. The
   stream is created anyway; the muxer judges it.

The stream keeps these parameters for the container's lifetime. The
parameters can differ between sources: a later source is checked against
them at its switch, and an incompatible one is reported to the instance,
which restarts the container or stops the output (§12.7).

Conformance:

- An output whose copy field starts with empty runs without known
  parameters creates no stream and writes nothing until a frame brings a
  run with known parameters; encoded streams do not start ahead of it.

## 3. Input

For each frame of positive length, the track reads every run of its field in
order. Each run, empty or not, carries a source id and its source's codec
parameters, and updates the track's source (§12.1). A packet's position in
the frame is its run's start plus its position in the run
([content.md §4.2](content.md#42-run-layer)); runs carry absolute starts, so
gaps between runs are kept. The packets of a run, in order, go through §12.2 (accept,
hold or drop), then timestamps (§5) and writing (§6–§7).

A packet of another media kind than the stream's is invalid content
([content.md §4.3](content.md#43-invalid-content)); content typing makes it
unreachable. The copy track treats it as an item fault
([faults.md §4](faults.md#4-item-faults-escalate)): dropped with a warning
naming the track, and counted (§10).

## 4. Conversion

None. Each written packet is a duplicate of the accepted packet with the
timestamps of [timestamps.md §6](timestamps.md#6-copy-streams-per-track).
Flags, side data and payload are kept.

## 5. Timestamps

[timestamps.md §6](timestamps.md#6-copy-streams-per-track) (per track) and
[§7](timestamps.md#7-copy-streams-across-tracks) (across tracks).

## 6. Writing

`write_packet`. The value the muxer stores is the value the monotonicity rule
checks ([timestamps.md §6.3](timestamps.md#63-monotonicity-check)): a track
MAY convert each packet to the stream's time base itself, once the container
header is written, and write it in that time base. A packet the muxer or its
bitstream filter rejects as invalid data is an item fault, dropped and
logged as [encoder.md §5.2](encoder.md#52-data-faults-and-output-faults)
states, and counted (§10). A failing byte sink is an output fault (same
section).

## 7. Keyframes

Every packet goes through the instance's hook
([encoder.md §8.2](encoder.md#82-keyframe-hooks)) right before it is written,
with its key flag; for an intra-only codec every packet counts as key. The
hook runs at write time, so a key packet held during a switch (§12.2) reaches
it when it is written, not when it arrives.

`request_keyframe` is ignored: a copied stream has no encoder to ask, and
its key packets are wherever the source put them.

## 8. Flush and end

The only stage a copy track has is its held packets, kept while its source
group waits to start (§12.3). `flush`, before an output stop and before a
container restart ([encoder.md §13.2](encoder.md#132-what-a-restart-does)),
starts every waiting group as the wait limit does (§12.3), so held packets
reach the muxer before the container closes
([draining.md](draining.md#2-the-rule)). After that the track holds nothing;
the container close has no codec of this track to drain. A restart ends the
track and its groups; the new container's track starts fresh, on a key
packet unless its codec is intra-only (§12.5).

A switch, seek-induced or not, does not end the copy track: the track goes on
with the next tenure. Held packets of a group that never starts are
discarded by the switching rule (§12.3), not by a drain: writing them would
start one stream of a source without its siblings.

Packets discarded on purpose are those of §12.2 and §12.3 (before a keyframe,
before the group start, from a group that never starts) and the join overlap
of [timestamps.md §6.3](timestamps.md#63-monotonicity-check). Each discard is
counted and logged (§11).

Conformance:

- The last packet of every copied stream of a source reaches the output,
  including when the output stops while a switch is waiting.

## 9. Properties

§1.

## 10. Options and errors

No codec options, so the copy track has no configuration error of its own.
Every fault it detects is classed by [faults.md §2](faults.md#2-classes). A
copy track is part of an output, not a source: it cannot end its upstream
source's track, so its item faults escalate inside the output (below).

| Fault                                                                                                | Class                                                  | Outcome                                                                                   | Owner                                                     |
| ---------------------------------------------------------------------------------------------------- | ------------------------------------------------------ | ----------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| decreasing or equal DTS within one source, PTS below DTS                                             | item fault                                             | packet dropped, warning, counted                                                          | [timestamps.md §6.3](timestamps.md#63-monotonicity-check) |
| DTS the stream time base cannot represent                                                            | item fault                                             | packet dropped, warning, counted                                                          | [timestamps.md §6.3](timestamps.md#63-monotonicity-check) |
| packet the muxer or its bitstream filter rejects as invalid data                                     | item fault                                             | packet dropped, warning, counted                                                          | §6                                                        |
| packet of another media kind, packet in a run with id 0                                              | item fault                                             | packet dropped, warning, counted                                                          | §3, §12.1                                                 |
| `max_consecutive_faults` consecutive item faults on one tenure, and every further run past the bound | escalation                                             | the source's group restarts with a keyframe wait (§12.3); warning; reported to the output | §12.3                                                     |
| incompatible copy source, no clean container restart                                                 | output fault                                           | the output stops or restarts under the output's policy (user decision, D2)                | §12.7                                                     |
| a failing byte sink                                                                                  | output fault                                           | [encoder.md §5.2](encoder.md#52-data-faults-and-output-faults)                            | §6                                                        |
| a group's keyframe wait reaches `max_keyframe_wait`                                                  | output-side wait ([faults.md §5](faults.md#5-waiting)) | the group starts without its late members; warning; reported to the output                | §12.3                                                     |

`max_consecutive_faults` is the bound of
[faults.md §4](faults.md#4-item-faults-escalate), used for every item fault of one tenure: the count
resets when a packet of the tenure is written. Every dropped packet counts:
packets carry media. Warnings are rate-limited by `fault_log_interval`
([faults.md §4](faults.md#4-item-faults-escalate)). A data fault never stops
the output; the incompatible source of §12.7 is the only data case that can.
Discards that the switching rules intend (before a keyframe, before the
group start, a group that never starts, join overlap) are not faults; they
are counted and logged at info or warning level (§11).

## 11. Logs

The wording below is an example; the levels and the facts each message
carries are binding.

| Level   | Facts                                                                        | When                                                                                                          |
| ------- | ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| info    | track, source id, number of packets dropped before its keyframe              | a tenure becomes ready after dropping packets (§12.2)                                                         |
| info    | source id, number of held packets dropped before the group start             | a group starts and trims held packets (§12.3)                                                                 |
| info    | track, source id, number of packets dropped as join overlap                  | [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                     |
| warning | track, the two fixes                                                         | global headers wanted, no extradata (§2)                                                                      |
| warning | source id, the tracks still waiting                                          | a group starts at the wait limit (§12.3)                                                                      |
| warning | source id, the tracks                                                        | a group restarts after `max_consecutive_faults` drops (§12.3)                                                 |
| warning | track, source id, number of held packets dropped                             | a group that never starts is discarded (§12.3)                                                                |
| warning | track, both parameter sets                                                   | incompatible parameters at a switch, container restarted (§12.7)                                              |
| error   | track, both parameter sets                                                   | incompatible parameters at a switch, output stopped (§12.7)                                                   |
| warning | track, source id, the fault and its values, the count since the last warning | an item fault (§10), at most once per `fault_log_interval` ([faults.md §4](faults.md#4-item-faults-escalate)) |

## 12. Source switching and per-source state

### 12.1 Model

Every run of copy content carries a **source id** (`stream_idx`); who
allocates one and when is in
[content.md §5](content.md#5-stream_idx-and-time_base). Ids are never
reused. A new decoder, a seek, a reconnect, a declared timestamp
discontinuity, a filter graph generation and a new inline record each
produce a new source id. The encoder relies on one property of source
ids: within one source id, the producer's timestamps run on one timeline
that never restarts. A producer whose timestamps restart allocates a new id;
the inline producer's id records follow this rule
([inline-codecs.md §5](inline-codecs.md#5-stream-index)). Packets of one id
whose timestamps restart are invalid input
([timestamps.md §6.3](timestamps.md#63-monotonicity-check)).

A track's **current source** is the id of the last run it read, empty runs
included. A run whose id differs from the current source is a **switch** for
that track. A **tenure** is the span of a track's consecutive runs with the
same id: it opens at the switch and closes at the next switch or when the
container ends. A frame here is one call of `encode`.

The **group** of a source id `s` is the set of copy tracks of the container
that hold a tenure on `s` (§12.3 says which tenures count). One group holds
the state shared by the tracks of one source:

| State      | Meaning                                                                                                                            |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| started    | whether the group has started (§12.3)                                                                                              |
| offset     | the offset applied to every timestamp of the source, fixed once ([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)) |
| wait start | the output position at which the group's first tenure opened                                                                       |

Each track keeps, for the container's lifetime: its current tenure (source
id, whether it is ready, its held packets), and the timestamp state of
[timestamps.md §6](timestamps.md#6-copy-streams-per-track).

Id 0 means "no source" ([content.md §5](content.md#5-stream_idx-and-time_base)).
A run with id 0 opens no tenure; a packet in such a run is invalid content
([content.md §4.3](content.md#43-invalid-content)), because no producer
emits one, and the track drops it as an item fault (§10). Before its first run with a
source a track has no current source.

### 12.2 Accepting a packet

A tenure **needs a keyframe** when the track's keyframe choice is
`wait_for_keyframe` and its codec is not intra-only (§2). Otherwise the
tenure is **ready** from the moment it opens, even before it has a packet.

For each packet of a tenure:

1. A tenure that needs a keyframe and is not ready drops every packet up to
   its first key packet. The key packet makes the tenure ready and is
   accepted. Reason: packets before a keyframe reference pictures the output
   never received, so a player cannot decode them.
2. A packet of a ready tenure whose group has started and has an offset is
   written at once ([timestamps.md §6](timestamps.md#6-copy-streams-per-track)).
3. Otherwise the packet is **held** by the track, in arrival order, until its
   group starts (§12.3).

The keyframe choice of one track affects only that track's own readiness.
`ignore_keyframe` makes the tenure ready at once; it does not exempt the
track from waiting for its group.

### 12.3 Readiness and group start

Copied tracks of one source start together, once each of them is ready.
Tracks of different sources start independently, each on its own readiness.
Reason: a stream that starts before its sibling's first keyframe plays audio
over a picture the player cannot show yet, and the two drift apart in
players that start presentation on the first decodable frame; tracks of
different sources have no keyframe in common to wait for.

**Membership.** At the end of each frame, after every track has read its
field ([encoder.md §5](encoder.md#5-encoding-a-frame)), the members of the
group of `s` are:

- the tracks whose tenure on `s` is open;
- the tracks whose tenure on `s` closed during this frame and was ready when
  it closed.

A tenure that closes before it becomes ready leaves the group: it contributes
nothing, and the group no longer waits for it. Evaluating membership at the
end of the frame makes the result independent of the order in which tracks
read their fields.

A producer feeds every field it contributes to in every frame, with an empty
run when it has no packet for that field
([content.md §4](content.md#4-two-chunk-layers), which owns this
guarantee). The tracks of one source
therefore open their tenures in the same frame.

**Start.** At the end of each frame, a group that has not started starts
when every member is ready. It also starts:

- at the **wait limit**: when the output position has advanced by
  `max_keyframe_wait` since the group's wait start, with the members that are
  ready, and with a warning naming the tracks still waiting. This is the
  output-side wait of [faults.md §5](faults.md#5-waiting): the group proceeds
  without the missing keyframe and the event is reported to the output
  ([encoder.md §5.2](encoder.md#52-data-faults-and-output-faults)), whose
  callbacks carry it to the script;
- at output stop or container restart (§8), as at the wait limit.

`max_keyframe_wait` protects memory (packets held by ready members) and
bounds the delay a ready stream suffers for a sibling with no keyframe.
Recommended value: 10 s, above the keyframe interval of common live and
file sources.

**Trim.** When a group starts, `T` is the latest PTS among the key packets
that made members ready (packets with no PTS use their DTS; converted to a
common unit, [timestamps.md §1.2](timestamps.md#12-encoder-side-conversion)).
When no member needed a keyframe, nothing is trimmed. Otherwise:

- a member that needed a keyframe drops its held packets before its last
  held key packet whose PTS is at most `T`, so that its first written packet
  is a key packet;
- every other member drops its held packets whose PTS is below `T`.

The trimmed packets are counted and logged (§11).

**Gap at the switch.** When the group's members are every stream of the
output (a **whole-output switch**), the time the group waited for its
keyframe is removed from the copy timeline: its first packet lands at the
previous end of the output, with no PTS/DTS gap. When any other stream
continues through the switch (a copy track on another source, or a
re-encoded stream), that time stays in the timeline as a gap, which keeps
the switched streams in sync with the continuing ones. The arithmetic is
[timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase).

**Writing.** The group's offset is then fixed
([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)) from the
members' first held packets, and each member writes its held packets in
order. A group that starts with no held packet (a sparse source, §12.4)
fixes its offset at the end of the first frame in which a member holds a
packet. Once fixed, the offset never changes.

**Late members.** A tenure that opens on a group that has already started,
and a member still waiting when the group started at the wait limit, is a
late member. It waits for its own keyframe when it needs one (§12.2), and
writes with the group's offset. Its timestamps follow
[timestamps.md §6.3](timestamps.md#63-monotonicity-check), join overlap
included.

**Group restart on a run of item faults.** After `max_consecutive_faults`
consecutive item faults on one tenure (§10; a non-increasing DTS is the
common case, [timestamps.md §6.3](timestamps.md#63-monotonicity-check)), the
group restarts as a new group on the same id: every member's tenure reopens
with the usual keyframe wait, a warning names the source and the tracks, and
the event is reported to the output. Every further run past the bound
restarts the group again ([faults.md §4](faults.md#4-item-faults-escalate));
the output keeps streaming. Reason: a group restart repairs a timestamp
restart, and a source that keeps faulting costs only its own packets.

**A group that never starts.** At the end of a frame, a group that has not
started and has no open tenure is discarded: every held packet of it is
dropped and logged at warning level. A track that writes packets of a later
source drops its held packets of an earlier source that has not started:
they would precede what it has already written.

Walk-through, with `wait_for_keyframe`, a video codec that needs keyframes and
an intra-only audio codec:

| Case                                                                                                                                                       | Result                                                                                                                                        |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| one source to audio and video, video mid-GOP, either field order (copy-only output: whole-output switch, no gap; with another continuing stream: gap kept) | audio held; video drops up to its keyframe; at the keyframe the group starts, audio before the keyframe's PTS is trimmed, both start together |
| audio from source A, video from source B                                                                                                                   | two groups; audio starts at the end of its first frame; video starts on its keyframe                                                          |
| audio and video from A, then audio from B and video from C                                                                                                 | groups B and C each start on their own readiness                                                                                              |
| video leaves the source before its keyframe                                                                                                                | the video tenure leaves the group; audio, now the only member, starts at the end of that frame                                                |
| video receives no keyframe for `max_keyframe_wait`                                                                                                         | audio starts at the limit with a warning; video starts later on its own keyframe, with the group's offset                                     |
| a third copy track from an unrelated source in the same output                                                                                             | in its own group; changes nothing for the others                                                                                              |
| audio-only copy output                                                                                                                                     | every tenure is ready at once; no packet is discarded at a switch                                                                             |
| both tracks `ignore_keyframe`                                                                                                                              | both ready at once; the group starts at the end of the frame                                                                                  |
| a source whose video ends before its first keyframe, then source B                                                                                         | the first group never starts or starts at the limit; group B starts normally                                                                  |

Conformance:

- Copy-only output, same source cut mid-GOP: no PTS/DTS gap at the join on
  either stream. With a re-encoded or otherwise continuing stream in the
  output: the gap of the wait stays, and the new source stays in sync with
  the continuing stream.
- Same source, cut mid-GOP: on every stream, the first packet of the new
  source is at or after the new source's first video keyframe, and the first
  video packet is a key packet. The output is identical with the fields
  declared in either order, and with an unrelated third copied stream added.
- Different sources: both streams are present and non-empty; the audio's
  first packet is not delayed by the video's wait.
- An audio-only copy output never discards a packet at a switch.
- A source whose video ends before its first keyframe, followed by a normal
  source: the output resumes on the second source.

### 12.4 Sparse sources

A sparse track (subtitles) may spend a long time in a tenure with no packet.
Its tenure is ready at once (§2), so it never holds its group. The time
before its first packet and after its last packet is carried by positions
alone; [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)
places the first packet of a source at its position in the output timeline,
so neither the head nor the tail of a sparse source shifts later cues.

Conformance:

- A subtitle stream whose first cue is 20 min into its source, copied after
  another source, has that cue 20 min after the join, whether or not the
  previous source's last cue ended early, and whether or not another copied
  stream comes from either source.

### 12.5 Lifetime of per-source state

The group of a source exists from the frame in which its first tenure opens
until the end of the frame in which its last member leaves (§12.3), or until
the container ends. Nothing else keeps state per source. Consequences:

- The state a group holds is never lost while a member still needs it: a
  member that has not started keeps the group alive.
- No state outlives the tracks of its source: a playlist of any length holds
  at most one group per copy track.
- A source id seen again after its group ended starts a new group, rebased
  like any switch.
- A container restart (Ogg chaining, an HLS restart,
  [encoder.md §13](encoder.md#13-container-restart)) ends every group with
  the old container. The new container's timestamps do not depend on the old
  one's groups; across an HLS restart only each stream's end carries over,
  so DTS keeps increasing ([timestamps.md §6.5](timestamps.md#65-restarts)).
- The behaviour depends only on the sequence of runs and packets, never on
  when memory is reclaimed.

Conformance:

- A copy encoder over a playlist of a few thousand short audio+video files
  holds a constant amount of per-source state.
- Every scenario of §12.3 gives identical output when run twice with memory
  reclamation forced at different moments, including an inline encoder put
  to sleep and woken with a copy encoder downstream, and an Ogg in-stream
  metadata restart with copied streams.

### 12.6 Reconnects and new decoders

A reconnect ([io.md §13](io.md#13-timestamps-across-reconnects)), a new
decoder, a seek in the file decoder
([decoder.md §12](decoder.md#12-seeking)), a timestamp discontinuity in a
format that declares them (MPEG-TS-style, chained Ogg;
[decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)), a
filter graph generation and a new inline record each start a new source id,
so each is a switch for
every copy track it feeds: §12.2–§12.3 apply, and timestamps are rebased
([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)). A seek
discards the decoder's held data upstream and is not drained
([draining.md §5](draining.md#5-discarded-on-purpose)).

### 12.7 Parameters across sources

Incompatible sources are rejected first by the content-format merge
([content.md §3.1](content.md#31-ffmpegcopy)), which owns what the copy
parameters contain and what "compatible" means. This section is the runtime
backstop for a source that reaches the encoder anyway.

At every switch, before any packet of the new tenure is accepted, the track
compares the source parameters of the new run with the parameters of its
stream header, by the `compatible` predicate of
[content.md §3.1](content.md#31-ffmpegcopy) (where the type level merges
them: [§3.1.1](content.md#311-where-copy-parameters-are-merged)). On
incompatible parameters:

the track accepts no packet of the new tenure and reports the source to the
instance ([encoder.md §13.3](encoder.md#133-incompatible-copy-source)):

- when the container defines a clean restart (Ogg chaining, a new HLS
  segment with its own initialisation), the instance restarts before the
  first packet of the new source
  ([encoder.md §13.2](encoder.md#132-what-a-restart-does)); the new stream
  takes the new run's parameters;
- otherwise the instance fails with an error that names the track and prints
  both parameter sets, and the output stops or restarts under the output's
  policy (`output.url` reconnects, which is a clean restart,
  [io.md §11.3](io.md#113-errors); MPEG-TS to a file has none and stops).
  This is an output fault ([faults.md §2](faults.md#2-classes)), the one
  data case that can stop an output (user decision).

A clean restart logs both parameter sets at warning level; a stop logs them
at error level (§11).

When `settings.ffmpeg.content.copy.relaxed_compatibility_check` is on, the
check uses the relaxed form of the same predicate: same media and same codec
id. Every other difference reaches the muxer, which judges the packets: a
packet it rejects is an item fault, and a run of them restarts the group
(§12.3). The
setting is an advanced escape hatch ([content.md §13](content.md#13-settings)).

Conformance:

The type merge rejects most incompatible sources when they are opened
([content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)). The
tests below reach the encoder through a path that bypasses the merge: the
output of a bitstream filter or of `track.ffmpeg.side_data`, which has its
own format (content.md §3.1.1, last row).

- Copy H.264 from an MP4 file, then from an MPEG-TS file, each through a
  bitstream filter that passes it unchanged, to an MPEG-TS file: the output
  stops at the switch with an error printing both parameter sets. The muxer
  never receives packets in the wrong form. The same to an HLS output: a
  clean restart, logged at warning.
- The same with an AAC source followed by an MP3 source: a codec change.
- With the relaxed setting on, a codec change is still incompatible
  (restart or stop, as above). For the same codec id, packets the muxer
  rejects are dropped as item faults and the output keeps streaming.
