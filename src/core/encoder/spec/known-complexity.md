# Known complexity

What the history of the encoders and their I/O shows to be hard, written
without the implementation: each entry gives the Situation, What went wrong,
Why it is hard, How to tell, the count and traces, and its reconciliation
with the as-built text of this tree. The normative pass answers every entry
with a rule; the reimplementation turns every entry into a conformance check.

Neighbouring trees keep their own lists, linked where an entry relates:
[FFmpeg](../../optionals/ffmpeg/spec/known-complexity.md),
[harbor](../../outputs/spec/harbor/known-complexity.md),
[HLS](../../outputs/hls/spec/known-complexity.md),
[SRT](../../optionals/srt/spec/known-complexity.md).

Entries whose reconciliation is "Not covered" or "Contradicts" are gathered
in [findings/known-complexity.md](findings/known-complexity.md). Language
notes from the history are in
[language-notes/known-complexity.md](language-notes/known-complexity.md).

## Themes

Entries are kept per area, each with its own traces. Where several areas met
the same difficulty, the normative pass answers it once:

| Theme                                                                              | Entries                                           |
| ---------------------------------------------------------------------------------- | ------------------------------------------------- |
| The destination's capabilities (seekable, a file, one stream) and who owns the I/O | KC-16, KF-1, KF-12, KF-13, KN-16, KN-17           |
| Headers for a reader joining late, at every moment                                 | KC-5, KF-14, KN-18                                |
| Final bytes and draining at stop and at disconnection                              | KC-2, KF-2, KF-3, KN-2, KN-19                     |
| Encoder delay, padding and continuity across splits                                | KC-3, KF-8                                        |
| Chained streams and in-band versus out-of-band metadata                            | KC-4, KC-15, KF-15, KF-16, KN-3, KN-4, KN-5, KN-6 |
| Instance lifetime: per run, per connection, per reopen                             | KF-11, KN-1                                       |
| Blocking I/O on the streaming thread, timeouts                                     | KF-10, KF-20, KN-9, KN-10, KN-13                  |
| Failure classification: retry, stop the output, stop the script                    | KC-9, KF-6, KN-12                                 |
| When and where a format is validated; static type against built value              | KC-10, KC-11, KC-12, KF-17, KF-18, KF-19          |
| Latency held inside the encoder                                                    | KC-6, KC-7                                        |
| State shared between instances, concurrent calls                                   | KC-8, KF-10                                       |
| Options and printed forms that stop meaning what they say                          | KC-18                                             |

## Area C: Native encoders and Ogg

Covers `%wav`, `%avi`, `%ndi`, `%external`, `%mp3`, `%shine`, `%fdkaac`,
`%flac`, `%ogg` with its muxer and its Vorbis, Opus, Speex, FLAC and Theora
streams, and their format languages. Links are relative to
`src/core/encoder/spec/`.

**Sources read.** As-built files of the area at 2e6bab6a5 (wav, avi, ndi,
external, mp3, shine, fdkaac, flac, ogg, ogg-streams, ogg-\*, formats,
encoder) and findings `native-codecs.md`, `ogg.md`, `simple-encoders.md`.
History: `git log --follow` on every current encoder file of the area
(wav, avi, external and its language file, lame, shine, fdkaac, flac, ogg
encoder and muxer, vorbis, opus, speex, theora, ogg flac, ndi, mp3 and opus
language files), plus the pre-move paths `src/encoder/*`,
`src/core/encoder/encoders/*`, `src/ogg_formats/*`, `src/core/ogg_formats/*`,
2008-10 (first generic Ogg encoder, c41f14e7e) to 2026-11 (b82c1e6c8); diffs
read for every commit named in the traces. GitHub: `gh issue list` and
`gh pr list --state all` for ogg, vorbis, opus, flac, lame, mp3, shine, aac,
fdkaac, wav, avi, external encoder, `%external`, gapless, pre-skip,
granulepos, chained, ogg header, id3, ndi, theora, speex, icecast ogg,
metadata ogg (≈190 distinct relevant items); ≈60 issues and PRs read with
comments. `CHANGES.md`, all releases, grepped for the area's names; area
regression tests (`GH2867`, `GH3239`, `GH3276`, `GH3316`, `GH3675`, `GH4116`,
`GH5546`, `fallible_ogg`, `external-encoder`) traced to their issues.
Neighbouring lists checked and linked, not restated: FFmpeg, harbor, HLS,
SRT known-complexity files.

### KC-1. Ending a logical stream that received no data, or whose codec holds none

- **Situation** — A link must be ended (a metadata change, a stop, a
  restart) at a moment chosen by the caller, not by the codec: before any
  piece was encoded, right after the previous link started, or when the
  codec holds nothing to flush. Some tracks of a multi-track link may have
  received nothing at all.
- **What went wrong** — Empty links were produced and rejected: a skeleton
  stream with no tracks, a leading skeleton-only link before the real first
  link (one demuxer then refuses the file: "changing stream parameters"), an
  Opus link with headers and no audio that broke playback, and every
  non-Vorbis stream ended by an empty end-of-stream packet that later
  decoders (and the Opus encapsulation RFC) reject as invalid data. The
  stand-ins built to give a codec "something" to flush raise in their turn:
  a silent sample built with the stream's channel count instead of the
  codec's, a black video image built with the wrong plane sizes.
- **Why it is hard** — A codec's end-of-stream needs either pending data or
  a library call that produces a final page; the container allows an empty
  end page but not an empty packet; each codec library offers a different
  primitive, and the multi-track case multiplies them. The empty case only
  arises on unusual orderings (metadata at position 0, a piece shorter than a
  video frame before a stop), which plain tests never produce.
- **How to tell** — For every Ogg codec alone and in every audio+video
  combination: call `encode_metadata` before the first `encode`; call `stop`
  right after `encode_metadata`; call `encode_metadata` twice with nothing
  between; end a link with a video track that got no image. Binding: no call
  raises; the bytes parse as whole pages; no packet is empty; every logical
  stream that has a beginning-of-stream page has an end-of-stream page; no
  link has zero tracks; a reference demuxer reads the whole file.
- **Count and traces** — 6 recurrences, 2008–2026: fc38d93f8 (2008, "avoid
  empty ogg streams as much as possible… think of a better API"), 3fd8a5e7b
  (2008, invalid skeleton when ended before any track), 1ac518346 /
  6c266d2be (2018, #180, empty Opus link), 02da14b41 (2023, #3062, empty EOS
  packets illegal), and still open as findings D2, D3, D4, D9.
- **Reconciliation** — Not covered / Contradicts. The behaviour is described
  ([ogg.md §5](ogg.md#5-encode_metadata), [§9](ogg.md#9-stop),
  [Ending a link](ogg.md#ending-a-link), each codec's §10), and it is the
  defect: the leading skeleton-only link (§5) and the raising Theora
  stand-in (§9) are what history shows to be bugs. Missing: a rule that an
  instance never emits a link with no track and that the empty-track case
  produces a valid end with the codec's own channel count.

### KC-2. Draining at `stop`: the encoder's queue, the codec's look-ahead, the resampler

- **Situation** — At `stop`, samples sit in up to three places: the
  encoder's own queue of samples short of one codec frame, the codec's
  look-ahead or delay line, and a rate converter in front of the codec. The
  caller then takes the bytes `stop` returns and closes the destination.
- **What went wrong** — The last tens of milliseconds of every file and of
  every reconnect were missing: up to one codec frame lost by the queue
  (LAME, shine, Opus, Speex), the whole delay line of shine and FDK-AAC
  never emitted (FDK: 2048 samples LC, 7106 HE-AAC v2), the resampler's held
  samples dropped (FLAC at 48 kHz from 44.1 kHz: 21 samples). Separately, a
  caller that ignored the bytes `stop` returned produced empty FLAC files
  after every reopen.
- **Why it is hard** — Each stage drains differently (a flush call that pads
  with silence, a flush call that only flushes bits, an end-of-input
  marker), and the bindings' flush entry points do not say what they drain.
  A queue added to feed a fixed-frame codec introduces a new place to lose
  data, invisible to a listening test because the loss is at the very end.
  A byte-count check passes because something was flushed.
- **How to tell** — Encode a known count of samples whose last 100 ms
  carry a distinctive signal, through every encoder, at the frame rate and
  at a different encoder rate; decode with a reference decoder applying the
  stream's own trimming. Binding: the decoded stream contains the last
  input sample's signal; decoded length minus signalled delay and padding
  equals the input length to within one sample at the output rate; the bytes
  `stop` returns are non-empty for every codec that holds data, and reach the
  destination.
- **Count and traces** — 8: 923597b1d (2012, shine flush added), a0d098f51
  (2013, "Correctly flush lame encoder", CHANGES 1.1.1), a69e60c52 /
  286e41a07 (2020, LAME flush call changed), 96cf5526a (2023, #3064, the
  fixed-frame queue whose remainder is dropped), d324eb145 (2023, #3328,
  #3316: caller dropped `stop` bytes → empty FLAC files), #2841 (2023,
  open), findings native-codecs 1–4, ogg D2 and the resampler gap. Related:
  [FFmpeg known complexity §10](../../optionals/ffmpeg/spec/known-complexity.md).
- **Reconciliation** — Contradicts [draining.md §2, §4](../../source/spec/draining.md#2-the-rule):
  [mp3.md §9](mp3.md#9-stop), [shine.md §9](shine.md#9-stop),
  [fdkaac.md §9](fdkaac.md#9-stop), [ogg.md §9](ogg.md#9-stop) describe the
  losses as the behaviour; history and the draining rule make them defects.

### KC-3. Encoder delay and padding are not signalled

- **Situation** — Every lossy codec delays its output (look-ahead, priming)
  and pads its last frame. A decoder removes them only if the stream says how
  much: a pre-skip field, an end granule position, an encoder-info frame.
  Files are reopened, streams restarted and links chained, so each start and
  each end is a place where delay and padding appear.
- **What went wrong** — Silence at the start and end of every MP3 file
  (LAME's 576 + decoder's 529 samples, shine's ≈528), audible at each
  reopen of a rotated recording and at each loop of a short sound; an Opus
  pre-skip field fixed at 80 ms whatever the encoder's real look-ahead, so
  73.5 ms of real audio is discarded at the start of every link, that is at
  every metadata change; an Opus end granule one sample past the data.
- **Why it is hard** — The correct value lives inside the codec library and
  must be queried, not configured; the container field is filled by a
  different layer; a wrong value is inaudible on music and only measurable
  with a sample-accurate comparison. The encoder-info frame was disabled
  once because the stream is not seekable to rewrite it.
- **How to tell** — Encode silence then a click at a known sample, and stop
  on a known sample; decode with a reference decoder honouring the stream's
  signalling. Binding: the click is at its input index to within one sample;
  decoded length equals input length; for chained Ogg, the same holds for
  every link, and the concatenated decode has no gap at link boundaries.
- **Count and traces** — 5: 1eeb06b16 (2012, Opus encoder with the fixed
  pre-skip default), 96cf5526a (2023, encoder-info frame disabled), #2841
  (2023, open), #1317 (2020), #5476 (2026); findings native-codecs gap 1,
  ogg D1, D2.
- **Reconciliation** — Not covered. [ogg-opus.md §3, §10](ogg-opus.md#10-end-of-stream)
  and [mp3.md §9](mp3.md#9-stop) state the values; the as-built has no
  requirement that delay and padding be signalled, nor a section for it in
  the encoder numbering ([encoder.md §8](encoder.md#8-section-numbering-of-implementation-files)).

### KC-4. A new link at every metadata change, and the consumers that cannot follow

- **Situation** — In-band metadata in Ogg is carried by ending the link and
  starting a new one whose comment header holds the new values. Consumers
  are browsers, desktop players, relays (Icecast and its forks), the
  project's own and a third-party demuxer.
- **What went wrong** — Players stopped or disconnected at every song
  change; one browser reported growing timestamp gaps and finally a demuxer
  error; VLC played half a second of silence at each loop of a short file;
  a widely used demuxer read no metadata from later links (and, once fixed,
  appended them); a relay fork mismatched header counts. Each was first
  judged a client bug, and some were; one turned out to be our empty
  end packets (KC-1).
- **Why it is hard** — Chaining is valid but rarely exercised; the only
  testing consumer for years was our own decoder, which accepted what we
  produced. Granule positions restart at 0 in every link, so any consumer
  timing on them sees a jump. Whether the fault is ours or the client's
  needs a reference demuxer and the RFCs.
- **How to tell** — Produce a stream with a metadata change every second,
  with each Ogg codec; read it with an independent demuxer. Binding: every
  link is reported as a new stream with its metadata; the total decoded
  duration equals the input duration (minus signalled trimming); no
  validator warning; the same with identical and with empty metadata.
- **Count and traces** — 8: #180 (2014–2018), #588 (2018), #2917 and #3010
  (2023, metadata of later links unreadable), 02da14b41 (2023, #3062), #5198
  (2026), #5476 (2026), #4785 (2025, FFmpeg's Ogg). Related:
  [harbor K1](../../outputs/spec/harbor/known-complexity.md).
- **Reconciliation** — Covered for the behaviour
  ([ogg.md §5](ogg.md#5-encode_metadata), [§13 item 7](ogg.md#13-where-the-bytes-go)):
  every metadata change, even empty or identical, makes a link. Not covered:
  no option to carry metadata out of band only (asked in #5306), and no
  statement of what a consumer is promised across a link boundary.

### KC-5. `header` and `restart_offset` must be right at every moment, for every encoder

- **Situation** — A shared encoder serves readers that join late, and a
  connection-oriented output serves a peer that reconnects. The caller asks
  the instance for the bytes a newcomer needs (`header`) and for where a new
  stream starts inside returned bytes (`restart_offset`). The answers change
  as links and processes restart.
- **What went wrong** — Every late Ogg listener got an empty or stale
  header (the header was a stored value updated at some transitions and read
  once by the caller); after the stored value became a call, a burst still
  began in the previous link; a native FLAC listener gets frames with no
  `fLaC` marker or stream info; an external encoder's newcomer gets no
  header although the process emitted one; a reconnecting SRT peer of an
  Ogg/Opus stream got pages without headers until the encoder was stopped on
  disconnect. Between `encode_metadata` and the next `encode`, the Ogg
  header lacks the secondary header pages.
- **Why it is hard** — Only the encoder knows where its decodable starts
  are; for the formats whose header is "just the first bytes" (FLAC,
  processes, WAV) the encoder was designed as if nobody joined late. The
  window where `header` is incomplete is short and only a joiner arriving in
  it notices.
- **How to tell** — For every encoder, after each of: creation, first
  `encode`, `encode_metadata`, the following `encode`, a process restart:
  take `header()`, append the bytes the next `encode` returns, and decode the
  result from scratch. Binding: decodes with the current link's or process's
  parameters for every encoder whose stream needs a header; `restart_offset`
  points at the start of each new stream inside the bytes, including a
  process restart.
- **Count and traces** — 7: 354656e7b (2010, stored header introduced),
  318cba9f7 (2023, #3265 / #3276, `GH3276`), f25b5b045 (2026, #5546 /
  #5547, `GH5546`), #5555 (2026), #5542 / #5567 (2026, FFmpeg's captured
  header empty), #4395 / #4399 (2025); findings native-codecs 5, ogg D10,
  simple-encoders 14, 15. Related, not restated:
  [harbor K1](../../outputs/spec/harbor/known-complexity.md),
  [SRT KC6](../../optionals/srt/spec/known-complexity.md).
- **Reconciliation** — Covered as built
  ([encoder.md §3.4, §3.5](encoder.md#34-header),
  [ogg.md §6, §7, §13 items 2–3](ogg.md#6-header)). Not covered: `header`
  for native FLAC and `%external`, `restart_offset` for a process restart,
  and `header` between `encode_metadata` and the next `encode`; the old
  normative "complete at every moment" is contradicted by the code
  ([encoder.md §10](encoder.md#10-the-previous-normative-text-against-the-code)).

### KC-6. Interleaving and page completion hold bytes for an unbounded time

- **Situation** — The Ogg muxer releases a stream's page only when every
  other stream holds a page with a later end time, and a codec layer
  releases a page only when about 4 KB or 255 packets have accumulated. A
  stream can be sparse (a still image, discontinuous transmission) or
  compress to almost nothing (lossless audio of silence).
- **What went wrong** — Five rewrites of the interleaving in one month
  before it held; later a refactor raised CPU use; an `%ogg(%flac)`
  stream of silence produced data "but not flushed in time", so the Icecast
  server timed the source out and the output reconnected each time a
  fallback to a silent source happened; audio waits ≈10 s for a still
  video's page.
- **Why it is hard** — Ordering by end time needs a page from every stream
  before releasing any; page completion depends on the content's bitrate,
  not on time; both are invisible with a tone generator, which every quick
  test uses. A low-rate period appears only in production (silence between
  tracks).
- **How to tell** — Encode silence for 10 s with every Ogg codec, and audio
  plus a still image; record when bytes leave the instance. Binding: no
  interval between two returned non-empty byte runs exceeds a stated bound
  (a page duration bound in seconds), and pages remain in non-decreasing end
  time across streams.
- **Count and traces** — 8: ce22d71b4, 1a2135f5e, 61c4f67a2, 3d9e50e6f,
  1338471df (2009-03, "fixed ogg muxing once and for all"; CHANGES 0.9.2
  "muxes pages according to their ending time"), c523cb413 (2019, CPU),
  #2965 (2023) and #3770 (2024–2025, ogg/flac on silence); finding ogg gap
  "interleaving stalls", DTX gap. Related:
  [SRT KC21](../../optionals/srt/spec/known-complexity.md),
  [FFmpeg §8](../../optionals/ffmpeg/spec/known-complexity.md).
- **Reconciliation** — Covered as built ([ogg.md Interleaving](ogg.md#interleaving),
  [Page sizing](ogg.md#page-sizing), [§13 item 5](ogg.md#13-where-the-bytes-go)).
  Not covered: no latency bound exists, and the page-fill parameter meant to
  bound it has no effect (KC-18).

### KC-7. A library's output callback is not a framing unit

- **Situation** — A codec library that does its own container framing hands
  its output through a write callback, a page as a header write then a
  body write. The binding copies each write through a fixed buffer and the
  host reassembles pages.
- **What went wrong** — Writes larger than the binding's buffer reached the
  host as several pieces; the host paired consecutive pieces as header and
  body; at the end of a link an odd piece was dropped, truncating the
  end-of-stream page, and an internal assertion stopped the stream
  intermittently after hours.
- **Why it is hard** — Mid-stream the bytes still come out in order, so
  files look fine and play; only the last page of a link is cut, and only
  when its body exceeds the buffer size, which depends on content and
  compression level.
- **How to tell** — Encode `%ogg(%flac)` at the highest compression with a
  link change every second on varied content; parse every page. Binding:
  every page has a valid checksum and the length its segment table states;
  every link ends with an end-of-stream page; no instance call raises.
- **Count and traces** — 2 in this form plus one earlier binding header
  check: 02da14b41 (2023, FLAC moved to the library's own Ogg framing),
  #5011 (2026), #5518 / f85faa9a5 (#5522, 2026); #1370 (2020, Speex header
  size check spamming logs).
- **Reconciliation** — Not covered. [ogg-flac.md §2, §8](ogg-flac.md#8-packets-granule-positions-and-pages)
  speak of "pages" handed over; the invariant that the host must frame by
  content, not by callback count, is only in the language notes.

### KC-8. Concurrent calls on one instance, and state shared between instances

- **Situation** — An instance is called from the clock thread (`encode`),
  from a server thread (`header` for a joiner), from a stop path (`stop` on
  a command or error), and since 2026 instances run in parallel on several
  domains. Some codec bindings keep process-wide state (a static header
  buffer, a shared random serial generator).
- **What went wrong** — A `stop` arriving while an MP3 `encode` ran crashed
  the process inside LAME (bit-reservoir assertion); the stored header was
  read while being rewritten; an external encoder deadlocked shutdown.
  Remaining per the findings: Opus header buffer and Ogg serial generator
  shared across instances, unsynchronised.
- **Why it is hard** — The original design stated that all encoding ran on
  one thread and did not protect state; the assumption stayed after outputs
  gained their own threads. Crashes are rare, depend on timing, and look
  like library bugs.
- **How to tell** — Run many instances of each encoder in parallel and, on
  each, call `header` and `stop` from other threads while `encode` runs.
  Binding: no crash, no exception other than documented ones; byte streams
  of parallel instances are identical to a sequential run's; serials are
  distinct within a link.
- **Count and traces** — 5: 354656e7b (2010, "not protected because
  encoding is done in main thread for now"), #3038 / 96cf5526a (2023,
  #3064), 318cba9f7 (2023), 1d0e4b88c (2023, #3029), b82c1e6c8 (2026,
  parallel dedicated encoders); findings ogg gaps "serial uniqueness",
  "Opus header buffer shared".
- **Reconciliation** — Covered for one instance
  ([encoder.md §4](encoder.md#4-serialisation-protect)); not covered for
  process-wide state ([ogg.md §11](ogg.md#11-concurrency-and-failures)
  describes it without a rule).

### KC-9. The external process: start, end of output, restart, stop

- **Situation** — `%external` feeds a child process on stdin and collects
  its stdout on another thread. The process may refuse to start, exit early,
  crash, be restarted on metadata or on crash, or keep its output pipe open
  through a grandchild. `stop` must wait for the tail.
- **What went wrong** — An audio-only process failed to start because the
  video branch was evaluated unconditionally; shutdown never completed
  because end of output was detected only by a zero read that never came;
  the process restarted on every metadata whatever the option said; a
  documented periodic restart option silently stopped working in a rewrite;
  an early exit loses the bytes already collected and fails the clock; a
  crash with restart enabled still fails the clock (broken pipe); each
  restart on metadata drops ≈80 ms.
- **Why it is hard** — Three asynchronous events (input writes, output
  reads, process exit) must be ordered; end of output, process exit and
  "the encoder asked to stop" are distinct; restarts interleave with
  `encode` calls from the clock. Tests use `cat`, which never exits early.
- **How to tell** — Drive the encoder with processes that: exit after N
  bytes; exit without reading; keep stdout open in a child after exiting;
  ignore stdin close; are restarted on metadata at a high rate. Binding:
  every byte the process wrote reaches the caller; `stop` returns within a
  bound or reports an error; no input sample is dropped across a restart, or
  the drop is signalled; a restart is reported as a stream start.
- **Count and traces** — 7: db90c4e13 (2009, added), ee2e412b0 (2016,
  rewrite drops periodic restart), 0f4eec6cc (2019, #789, restart on every
  metadata; CHANGES 1.4.0), 04ca200a2 (2023, #3020), 1d0e4b88c (2023,
  #3029), findings simple-encoders 1, 2, 3, 4, 21, 22.
- **Reconciliation** — Covered as built ([external.md §9, §10, §11](external.md#9-stop)).
  Contradicts the old normative "`stop` drains and returns final bytes"
  ([encoder.md §10](encoder.md#10-the-previous-normative-text-against-the-code)).
  Not covered: restarts as stream starts (KC-5), signalling of dropped input.

### KC-10. When a configuration error surfaces, and what it says

- **Situation** — A format value is checked by its builder at evaluation,
  by deferred values when forced, by the factory at output start, by the
  codec library at its first encode, or never. Library builds differ: the
  free FDK build has no HE-AAC, a codec module may be absent.
- **What went wrong** — Errors moved from evaluation to the first `encode`
  when instance creation was delayed for dynamic parameters, so a typo is
  found at the first frame; HE-AAC failed at run time with "Invalid
  configuration: aot" on distributions shipping the free FDK build; 32-bit
  FLAC stopped a stream with a bare internal error; a missing codec module
  gave a bare exception, then "unsupported format" without the reason;
  libFLAC rejections are silent (0-byte file); shine refuses at first encode
  with no message; a Vorbis ABR default fails at first encode.
- **Why it is hard** — Every layer validates part of the space, the
  library's acceptance depends on its build, and moving validation for one
  feature (deferred values) moves it for all. Tests use valid defaults.
- **How to tell** — For each encoder, a table of invalid combinations
  (rate, channels, bit depth, profile × rate, unsupported profile in the
  library build, missing module). Binding: each is rejected before the first
  byte is produced, with a message naming the parameter and the format's
  position; `--check` rejects every one whose values are literal; no
  rejection is silent.
- **Count and traces** — 9: b25675d46 (2019, #857), f16149d50 (2019),
  f73c08d2b (2021, #1858), e4d6693a7 (2021, #1942), 841e9ddcd (2023, #3155),
  #1073 (2020, CHANGES 1.4.2), #3934 (2024), #4053 and #4114 (2024); findings
  native-codecs 7, 11, asymmetry 1, 4; ogg D6, D12; simple-encoders 11, 18.
- **Reconciliation** — Covered as built ([formats.md §4, §5](formats.md#4-evaluation-building-the-format-value),
  [encoder.md §2.3](encoder.md#23-instance-creation)). Not covered: no
  single point where a format is known valid, and no rule against silent
  library rejection.

### KC-11. The format's static type against what the value builds

- **Situation** — A format expression's type (channel count, presence of
  video) is computed from its parameters before evaluation; the value is
  built from the same parameters at evaluation. Parameters can be literal,
  boolean flags (`mono`, `stereo=false`, `video=true`), or expressions.
- **What went wrong** — `%external(video=true)` typed as audio-only, so a
  video source was rejected (with "Unknown position"); `%mp3(stereo=false)`
  with a mono source failed type checking (a cookbook example);
  `stereo=<bool>` support had to be restored; `%mp3(mono)` crashed at run
  time in the layer matching content to type; `video=false` is rejected; an
  unsupported channel count crashes the type checker with no position;
  `width=`/`height=` turn video on in the value but not in the type.
- **Why it is hard** — Two independent interpretations of one parameter
  list (type and value) drift whenever one gains a parameter; channel
  counts are needed by the type before any setting is read (KC-12).
- **How to tell** — For each encoder, enumerate its parameters that affect
  content (channels, mono/stereo flags in both forms, video on/off, size)
  and check: binding, the type `--check` reports equals the content the
  built value encodes; every rejected value is rejected with a position; no
  input crashes type checking.
- **Count and traces** — 7: e36d88245, b1989cab0 (2021), #2867 (2023),
  78ae7d596 (2023, #2868), #3217 / c0f95a132 (2023), #3239 (2023,
  `GH3239`); findings ogg D7, simple-encoders 10, 13, native-codecs gap 7.
- **Reconciliation** — Covered as built ([formats.md §6](formats.md#6-typing));
  the mismatches above are the defects; no rule says the two
  interpretations come from one source.

### KC-12. Defaults that depend on settings the script sets later

- **Situation** — A format written in a script takes its default sample
  rate, video size and channel count from the frame settings, which the same
  script may set after writing the format.
- **What went wrong** — WAV headers declared 44.1 kHz while the frame ran at
  48 kHz; a frame rate setting was ignored; making AVI's rate lazy was
  merged and reverted; Theora width and height were removed once because
  reading them forced the frame size before the script set it; channel
  defaults were fixed to 2 because the type needs them early, except for NDI.
- **Why it is hard** — Evaluation order of the script and of format
  defaults is not visible to the user, and a forced value cannot be unforced;
  the failure is a plausible but wrong header value.
- **How to tell** — For every encoder: set the frame rate, size and channels
  after writing the format, then start. Binding: the bytes declare the
  settings' values; a setting that cannot be honoured is an error at
  startup, not a silent default.
- **Count and traces** — 6: 8be2fe991 (2009), #870 / 87aee0975 (2019),
  #933, #935 reverted by #938 (bdb6f1525, 2019), 767026060 and 14557ac08
  (2019); finding simple-encoders 17.
- **Reconciliation** — Covered ([formats.md §5](formats.md#5-deferred-values)).
  Not covered: channels are not deferred and fall back to a fixed 2, which
  is the asymmetry history keeps reopening.

### KC-13. Piece bounds and units

- **Situation** — The caller hands an instance a frame and a piece of it
  (start, length) in the stream's time unit; audio codecs count samples,
  video codecs images; the piece may be zero-length or shorter than a codec
  frame, or a frame may hold several images.
- **What went wrong** — The WAV encoder ignored the piece's start; five
  encoders passed the start in time units instead of samples; a rate
  converter at ratio 1 returned the caller's buffer instead of a copy; Opus
  dropped input when the remainder was shorter than a frame; AVI writes all
  images of a frame in one chunk and an empty chunk for frames without one;
  a zero-length block to Vorbis means end of input.
- **Why it is hard** — Two units coexist and agree at the default
  frame/rate settings, so the bug appears only at other rates or with
  offsets; chunking into codec frames needs a carry-over that must survive
  every call shape.
- **How to tell** — Feed each encoder the same signal cut into pieces of
  random lengths (including 0 and 1 sample) and random offsets, at two
  frame rates and two encoder rates. Binding: the decoded signal equals the
  one from a single whole-piece run (bit-exact for PCM, sample count exact
  otherwise); video chunk count equals image count.
- **Count and traces** — 6: dd7e124bd (2012), f2179463f (2019), a3249cf86 /
  ff8a55ae1 (2022), 8af9836af (2024, #4078); findings simple-encoders 5, 20;
  ogg gap "zero-length audio block".
- **Reconciliation** — Covered as built (each file's §3); not covered: the
  zero-length and sub-frame cases are described per encoder with no common
  rule ([encoder.md §3.1](encoder.md#31-encode)).

### KC-14. Rate conversion inside the encoder

- **Situation** — A codec supports a set of rates (Opus: 8–48 kHz in five
  steps; shine, FDK, LAME their own) and the frame runs at another; the
  encoder converts. The user configures "samplerate" without knowing
  whether it is the input, the codec or the decoder's rate.
- **What went wrong** — Opus picked a rate by rounding to the next
  supported one, then was changed to validate the given value; users heard
  Opus as "badly resampled" and the parameter's meaning had to be explained;
  the converter at ratio 1 aliased the caller's buffer (KC-13); held samples
  are lost at stop (KC-2).
- **Why it is hard** — A converter is a stateful stage between the piece
  and the codec, with its own latency and tail; each encoder built its own.
- **How to tell** — For each encoder and each supported rate different
  from the frame's: a sine at a known frequency. Binding: decoded frequency
  and duration match; no aliasing above the stated bandwidth; the converter
  drains at `stop` (KC-2).
- **Count and traces** — 3: fda9c677d (2012), #759 (2019), ff8a55ae1
  (2022); findings native-codecs 4, ogg resampler gap.
- **Reconciliation** — Not covered: the as-built mentions the converter per
  encoder; no common description of it as a stage with a tail.

### KC-15. Metadata in the bytes: which values, when, and in what form

- **Situation** — Metadata reaches an encoder as initial metadata at
  creation and as `encode_metadata` later, filtered by an export list. An
  encoder can put it in band (ID3v2 at the start, Vorbis comments per link,
  per-segment ID3 in HLS) or not at all.
- **What went wrong** — The initial ID3v2 tag was discarded, then written
  only at the first non-empty metadata, then written from the initial
  metadata which callers pass empty: an empty tag in every file; a scheme
  hiding a message in MP3 frame header bits was added and later removed;
  Vorbis streams carried only a fixed set of tags; HLS needed ID3 per
  segment for every encoder and its ID3 stream came out empty on one
  branch; ICY metadata was spliced into Ogg streams.
- **Why it is hard** — Who holds the current metadata at creation (caller or
  source) is undefined; formats differ on whether metadata may appear
  mid-stream; an empty tag is valid and passes checks.
- **How to tell** — Start each encoder with a titled source through every
  output kind; change metadata mid-stream. Binding: the first in-band
  metadata, where the format has any, carries the source's title; later
  metadata appears where the format allows it and nowhere else; no empty
  tag is written.
- **Count and traces** — 9: a112095d3 (2010, LS-441), d6eaee270 (2011),
  ee0b1fd15 (2012), #418 / 0c8b7a078 (2017), de2b2c75c (2019), 96cf5526a
  (2023), 2ba7ba739 (#2924), #3154 / #3604 (2023), #5084 (2026), #5542
  (2026); findings native-codecs 9, 10, asymmetry 7. Related:
  [HLS KC18](../../outputs/hls/spec/known-complexity.md).
- **Reconciliation** — Covered as built ([encoder.md §2.3](encoder.md#23-instance-creation),
  [mp3.md §10.2](mp3.md#102-id3v2-tag)). Not covered: who supplies initial
  metadata; the caller-side empty metadata is the defect.

### KC-16. Headers that describe the whole stream, on a destination that cannot seek back

- **Situation** — WAV and AVI headers carry sizes, FLAC stream info carries
  the total sample count and checksum, a VBR MP3 needs a table of contents;
  all are known only at the end. The instance never seeks; files are
  reopened and rotated.
- **What went wrong** — A length parameter was added so WAV could declare a
  size up front, and nothing enforces it (truncated decode or "packet
  corrupt"); on reopen, later WAV files had no header at all; FLAC files
  have no duration and no checksum; AVI has no index; sizes above 2 GiB wrap.
- **Why it is hard** — The encoder is defined apart from its destination,
  so it cannot know whether seeking back is possible; placeholder values
  are accepted by tolerant players.
- **How to tell** — Write each format to a seekable file, stopped at an
  arbitrary point, and through a pipe. Binding: on the file, the header's
  sizes, counts and checksum match the content; on the pipe, the header
  declares "unknown" rather than a wrong value; every reopened file starts
  with a complete header.
- **Count and traces** — 4: 70216bd07 (2011), #3675 / #3676 (2023,
  `GH3675`), findings simple-encoders 8, 16, 25, native-codecs 6, gap 3.
- **Reconciliation** — Covered as built ([wav.md §13](wav.md#13-where-the-bytes-go),
  [flac.md §13](flac.md#13-where-the-bytes-go),
  [encoder.md §7](encoder.md#7-where-the-bytes-go-what-the-seam-assumes)).
  Not covered: this is the main case for giving the encoder its
  destination's capabilities.

### KC-17. Images whose geometry is not the one configured

- **Situation** — Video encoders get images of the frame's size, with
  strides chosen by whatever produced them, odd dimensions, or a size
  detected from the input; the format declares its own width and height.
- **What went wrong** — AVI asserted on strides and was later fixed to copy
  row by row; AVI output flashed green; Theora raised "invalid data" on some
  inputs and not others and was disabled in binary builds; Theora crashed
  streaming to Icecast; keyframes were reported mismarked; odd sizes give a
  per-image payload that disagrees with the header.
- **Why it is hard** — The common test sizes have tight strides and even
  dimensions; the size in the format, the frame and the image are three
  values that agree by default.
- **How to tell** — Encode images with padded strides, odd width and height,
  and a size different from the format's. Binding: the decoded image equals
  the input (or the scaled input), header sizes equal the payload, or the
  instance rejects the mismatch at creation with a message.
- **Count and traces** — 7: 2c3801924 (2012), #549 (2018), #799 (2019),
  1830e7e5f (2022), #3311 (2023), #4116 (2024, `GH4116` skipped); findings
  simple-encoders 5, 6, 7, ogg D3 and the Theora size gap.
- **Reconciliation** — Covered as built ([avi.md §3](avi.md#3-encode),
  [ogg-theora.md §7](ogg-theora.md#7-data-input-and-conversion)); not
  covered: no rule about who adapts geometry.

### KC-18. Options and printed forms that silently stop meaning what they say

- **Situation** — Format options are documented, parsed and printed; the
  code that honours them changes through rewrites and vendoring.
- **What went wrong** — A periodic-restart option for `%external` stopped
  having any effect in a 2016 rewrite and is still documented; the Ogg
  page-size option has no effect since the vendored binding lost its build
  flag; printed formats do not parse back (Theora printed as `%ogg()`,
  Opus complexity as a string, a typo in Opus's and Speex's printed forms).
- **Why it is hard** — Nothing fails: the option parses and the output is
  valid, only not as asked. Round-trip and effect tests do not exist.
- **How to tell** — For every option, two runs differing only in that
  option. Binding: the bytes differ in the way the option documents; for
  every format value, printing then parsing gives an equal value.
- **Count and traces** — 4: ee2e412b0 (2016), 083632852 (2013) with
  d772ceee5, #5067 (2026); findings simple-encoders 3, ogg D5, D8.
- **Reconciliation** — Covered as built ([ogg.md Page sizing](ogg.md#page-sizing),
  [formats.md §7](formats.md#7-printing)); the inert options contradict
  their documentation.

## Area F: The seam, file and process outputs

**Sources read.** As-built: `README.md`, `encoder.md`, `formats.md`,
`output-encoded.md`, `output-file.md`, `output-pipe.md`, `tests.md`,
`findings/README.md`, `findings/seam.md`, `findings/file-callers.md` (spec at
2e6bab6a5). History: `git log --follow` on the pipe-output file (144 commits,
2011-02 "Added back output.pipe" to 2026-11), the seam file (2017-08 split to
2026-11), the format-evaluation file (2021-09 to 2026-09), the `pipe` operator
(2016-05 to 2026-10, plus its 2008–2010 predecessor and removal), the
encoded-output base's metadata commits back to the 2009 `insert_metadata`
rework; `git log -S` on `ifencoder`, `current_metadata <- `, `O_APPEND`,
`with_file_output`, `Data_first`; `--grep` on the issue numbers below.
Issues/PRs: `gh issue list --search` on "output.file reopen", "output.file
append", "output.file filename", "on_close", "output.external", "pipe
operator", "%ifencoder", "encoder stereo mono", "metadata interpolation",
"empty file", "flush", "truncate", "fifo", "encoder format type", "unsupported
format", "encoder parameter"; read: #116 #386 #405 #484 #553 #607 #614 #789
#876 #885 #899 #933 #993 #1111 #1331 #1470 #1569 #1786 #1858 #1942 #2069
#2070 #2072 #2437 #2506 #2523 #2593 #2600 #2726 #2756 #2837 #2841 #2842
#2850 #2867 #2924 #3038 #3064 #3206 #3217 #3251 #3255 #3265 #3276 #3316
#3472 #3504 #3591 #3639 #3675 #3723 #3934 #4053 #4422 #4819 #4830 #4879
#4899 #5546. `CHANGES.md` (all releases) for `output.file`, `output.external`,
`pipe`, `reopen`, `on_close`, `append`, `%ifencoder`. Regression tests GH2756,
GH2756-2, GH2842, GH2850, GH3316, GH3675, GH4819, GH4819-bis,
output_reopen_on_error_delay. Neighbouring lists: the HLS one (KC5, KC14,
KC22, KC23, KC24 relate); the FFmpeg, harbor and SRT lists are empty at this
snapshot, so seam entries that touch late joiners (KF-14) are kept here.

---

### KF-1. The bytes of one instance belong to one destination

- **Situation** — A file output closes destination N and opens N+1 on a
  reopen trigger. The instance for N is stopped and a fresh one created for
  N+1. The order of "switch destination", "stop old instance", "create new
  instance" and "encode the next piece" decides where each byte lands.
- **What went wrong** — With WAV and a time-based reopen, only the first file
  had a header; every later file was headerless audio. The new instance's
  first output (its header) was produced while the old destination was still
  current, or was produced before the switch and dropped. Reported missing
  milliseconds at file boundaries were traced to the same ordering.
- **Why it is hard** — Encoders put their header in the first bytes they
  return, not in a separate call, so "first output of an instance" and
  "first bytes of a file" must coincide. Any trigger evaluated after encoding
  a piece, rather than before, shifts one piece of output across the
  boundary. Nothing in the bytes reveals which instance produced them.
- **How to tell** — For each format with a leading header (WAV, Ogg, FLAC,
  MP3 with ID3v2) and each reopen trigger (metadata, timed, explicit, error
  retry): every closed file starts with that format's header and decodes on
  its own; the sum of samples over all files equals the samples pulled.
- **Count and traces** — 2: #3675 (980e26714, test GH3675), #2841 comment
  linking its missing data to #3675.
- **Reconciliation** — Covered: [output-file.md §2](output-file.md#2-instance-lifecycle)
  (one instance per opened file, created at the first non-empty piece after
  open) and [§3](output-file.md#3-call-sequence) (reopen checks run before
  `encode`).

### KF-2. The final bytes at close are part of the file

- **Situation** — Closing a destination (reopen, stop, error) stops the
  instance, which returns its last bytes (codec flush, container trailer,
  buffered remainder).
- **What went wrong** — FLAC files after the first reopen were empty or a few
  kilobytes: the stop result was discarded, and an encoder that buffers
  heavily had written almost nothing before it. MP3 and WAV looked fine, so
  the loss was format-dependent. Separately, failures of that last write and
  of `stop` itself are swallowed, so a truncated last file is not reported.
- **Why it is hard** — Most encoders return little at stop, so dropping it
  goes unnoticed except for encoders whose output is mostly deferred. Close
  runs on error paths too, where a second exception must not mask the first,
  which pushes implementations to swallow everything.
- **How to tell** — Per encoder: a file closed by each close path holds
  exactly the concatenation of every `encode` result then the `stop` result
  (a recording double for the instance makes this byte-exact). A failure
  injected in the final write is reported somewhere observable.
- **Count and traces** — 2: #3316 (d324eb145 / backport a08e83d48, #3328,
  test GH3316), ff43b5550 (stop-bytes write made silent). Findings
  file-callers 18.
- **Reconciliation** — Covered: [output-file.md §2](output-file.md#2-instance-lifecycle)
  close steps 1–3 and [§4.4](output-file.md#44-close-and-on_close); the
  swallowing is described there as-built (finding 18 is the agenda).

### KF-3. A file that received nothing

- **Situation** — A file output opens its destination at start, at reopen and
  at error retry; the source may produce nothing (fallible, failing from the
  start, or a reopen on the very first metadata).
- **What went wrong** — Empty files at every start of a failing fallible
  output (2019); later, files with a header and no data when the source never
  produced (2026). Today a reopen on the first track's metadata still leaves
  a 0-byte file behind, with `on_close` called for it.
- **Why it is hard** — "Open the file" and "start the stream" are separate
  events; a header is cheap to produce early and tempting to write at open.
  Fixing the header-only case (lazy instance) does not fix the 0-byte case
  (eager open).
- **How to tell** — Start an output on a source that never produces, and one
  whose first metadata triggers a reopen: no file without at least one
  encoded sample is left, or the spec states which empty files are expected
  and whether `on_close` fires for them.
- **Count and traces** — 3: #876 (6b9ed7e0a), #4899 (e87290d32), findings
  file-callers 16.
- **Reconciliation** — Covered: [output-file.md §2](output-file.md#2-instance-lifecycle)
  (lazy creation, "a file that never received a non-empty piece ... is left
  empty").

### KF-4. Ask for the file name exactly once per file, and only then

- **Situation** — The name is a script getter, often time-based. The output
  needs the name to log, to create directories, to open, and to report at
  close.
- **What went wrong** — The getter was called several times per reopen; near
  a second boundary it answered different names, producing two files for one
  open. Earlier, names given as plain strings with strftime codes were
  expanded once (or, after the codes were removed, never), so users saw a
  name frozen at script start. Today a failed open pins the name: retries
  open the same unwritable path even when the getter now answers another.
- **Why it is hard** — A getter is a function with side effects and a clock;
  every extra call is a chance to diverge, and caching the answer needs a
  clear invalidation point. Two different points (after a successful close,
  after a failed open) both need it.
- **How to tell** — Count getter calls: exactly one per successful open, and
  one per retry after a failed open; no call at construction. A getter that
  changes its answer between retries is followed.
- **Count and traces** — 4: #2842 (caef89dd8, backport 5c4daae3d, test
  GH2842), re-reported as #4830 (not a bug); #3639, #3723, #2593 (6df0185bb,
  strftime removed from names); findings file-callers 2.
- **Reconciliation** — Covered: [output-file.md §5.1](output-file.md#51-file-naming)
  states the call-once rule and the pinned name after a failed open (finding
  2 is the defect).

### KF-5. Metadata in file names and in the initial header silently dead

- **Situation** — The output keeps "the current metadata" to interpolate
  `$(title)` in names or commands, and to pass as initial metadata to each new
  instance (ID3v2, FLAC comments, Ogg/FFmpeg container tags).
- **What went wrong** — Maintainers recommended `"$(title).ogg"` with
  `reopen_on_metadata` as the way to name files by track. A 2023 rework of
  the reopen logic removed the only assignment of the current metadata; since
  then every `$(key)` expands to empty, every reopen with an unchanged name
  truncates the previous file, and MP3 files start with an empty ID3 tag. No
  test noticed; one 2017 attempt to copy the stored metadata was reverted the
  same day.
- **Why it is hard** — A value read in two places and written in one, with
  defaults that look valid (empty string, empty tag). The failure produces
  well-formed files with the wrong names and no tags.
- **How to tell** — Track A then B with `reopen_on_metadata` true and a name
  using `$(title)`: two files named after A and B; with `%mp3(id3v2=true)`
  each carries its title. Also after a timed reopen: the new file carries the
  metadata current at its open.
- **Count and traces** — 2 (removal and a reverted attempt): 2ba7ba739
  (#2924), 889d123fc / e3761ef26; intent stated in #484. Findings
  file-callers 1, 11.
- **Reconciliation** — Contradicts: [output-file.md §2](output-file.md#2-instance-lifecycle)
  ("empty initial metadata, always") and [§5.1](output-file.md#51-file-naming)
  step 3 describe a regression, not a design.

### KF-6. A retry delay that is honoured on every failure path

- **Situation** — The output opens a file or starts a process; the open, a
  write or the process may fail. A script policy returns a delay before the
  next attempt, or escalates.
- **What went wrong** — Twice the delay was ignored: a failing process or an
  unwritable path was retried on every streaming cycle (more than twice per
  second; 156 times in three seconds in the 2026 test), flooding logs. The
  first fix covered failures during encoding only; the second, years later,
  covered failures during the open that runs at start, before the output
  counts as started. A directory-creation failure also reached the policy
  with the wrong error kind.
- **Why it is hard** — Failures happen in different lifecycle states (not yet
  started, started and open, open and writing, closing) and each state has
  its own code path back into the cycle; a delay stored in one path is not
  consulted by another. Errors from different layers arrive as different
  exception families.
- **How to tell** — For each failure point (directory creation, open,
  process spawn, write, close) at each state (first start, after a reopen,
  after a previous error): with a policy returning `d`, attempts are spaced
  by at least `d`, the policy sees kind `system` for system errors, and a
  `null` policy escalates to the clock's error handler.
- **Count and traces** — 3: 7260c7f43 (retry added, #1506), #2756 / #2850
  (0d11087af, tests GH2756, GH2756-2, GH2850), #2437 (2731e5a0d, test
  output_reopen_on_error_delay).
- **Reconciliation** — Covered: [output-file.md §6](output-file.md#6-failures)
  (error-retry path; cycle skipped while the date is in the future).

### KF-7. Reopen triggers of different nature behind one surface

- **Situation** — A file is split on an event in the stream (metadata), on a
  polled condition (time), on an explicit request, or after an error. Each
  has its own delay semantics and its own effect on callbacks and metadata.
- **What went wrong** — One unified callback (2023) made the common hourly
  split require a hand-written latch; users' scripts broke with type errors;
  it was split back into three callbacks a release later. A hidden default
  minimum file age of 120 s made `reopen_when` appear to fire every two
  minutes. Today `on_reopen` fires for three of the four paths and only the
  metadata path hands the triggering metadata to the new file.
- **Why it is hard** — An event trigger fires once; a polled condition stays
  true for a whole second (or minute) and must be debounced; an error retry
  is not a reopen from the script's point of view. One surface for all three
  hides the differences, three surfaces drift apart.
- **How to tell** — For each trigger, table and test: whether it fires once
  per condition, what delay applies, whether `on_reopen` runs, what metadata
  the new file starts with. `reopen_when={0m}` gives one file per hour with
  defaults.
- **Count and traces** — 5: #2924 (2ba7ba739), #3206, #3229 / #3236
  (26a7d05e6, 6e3047f5d), #3251, #2837. Findings file-callers 10, 11.
- **Reconciliation** — Covered: [output-file.md §5.2](output-file.md#52-reopen-triggers)
  and [§5.3](output-file.md#53-what-a-reopen-does-to-the-bytes).

### KF-8. Where a split lands, and what it costs at the seam

- **Situation** — A long recording is split into files by time or by an
  external event, and users expect the files to concatenate back to the
  original.
- **What went wrong** — Files had padding at start and end and audible
  discontinuities at each split; concatenating them byte-wise gave noise.
  Splits made on network connect/disconnect events put the end of one DJ's
  set at the start of the next DJ's file, because audio still buffered
  upstream played after the event. Time-based splits are decided by the wall
  clock when a piece is encoded, not by the stream position.
- **Why it is hard** — Each file is a fresh encoder, so each pays the codec's
  priming delay and end padding; seamless splitting needs either gapless
  signalling in each file or one encoder across files. The trigger and the
  audio it refers to are separated by buffers whose depth the output does not
  know.
- **How to tell** — Split a known signal every N seconds with each lossy
  encoder: decoding and concatenating the files (with their declared
  delay/padding honoured) reproduces the input sample count. A split asked
  at stream time T starts the new file within one frame of T.
- **Count and traces** — 5: #2841 (open), #2726, #2837 (comments on
  concatenation), #1470, #2070.
- **Reconciliation** — Not covered: the spec states where a reopen is
  evaluated (per piece, wall clock) but says nothing on continuity across
  files: samples added or lost per split, and whether files carry gapless
  delay/padding information.

### KF-9. What `on_close` sees and what its failure means

- **Situation** — A script hook runs after each close with the file name; it
  is used to move, upload, probe or patch the finished file.
- **What went wrong** — Introduced to "write the WAV duration once the file is
  complete". Moving the file inside the hook crashed the output (the output
  still touched its state for that file after the hook). Hook errors were
  swallowed in 2023, then made fatal again in 2026 because swallowing hid real
  failures. A raising hook today leaves the old name pinned for the next open.
- **Why it is hard** — The hook runs in the middle of the output's state
  transition, on the streaming thread; the file must be complete (trailer
  written, descriptor closed) but the output must no longer depend on it.
  Whether a hook error is a stream error is a policy question.
- **How to tell** — In the hook: the file is complete and decodable, can be
  renamed or deleted without affecting the output, and a raised error goes to
  the same retry policy as other output errors and does not affect the next
  file's name.
- **Count and traces** — 4: f619ee56e (added), #3038 comments (moving the
  file), d324eb145 (errors swallowed), #4879 (c620ddafe, errors raised).
- **Reconciliation** — Covered: [output-file.md §4.4](output-file.md#44-close-and-on_close)
  and §5.1 (exception leaves the name set).

### KF-10. Stop, start and reopen from another thread while the clock encodes

- **Situation** — Scripts start and stop recordings from timers, telnet or
  HTTP handlers, while the clock thread is in the middle of encoding a frame
  for that output.
- **What went wrong** — LAME aborted with an internal buffer inconsistency
  when `stop` ran during an `encode`; later a closed channel was written to
  ("option is None", "Bad file descriptor"); in 2026 a shutdown from a script
  thread produced a partial frame that tripped an assertion and killed the
  process about 30% of the time.
- **Why it is hard** — The output's lifecycle is driven by two parties (the
  clock and the script) and the instance by one. Per-instance locking stops
  the codec crash but not the output-level race: the destination and the
  instance can be torn down between two pieces of one frame.
- **How to tell** — Toggle start/stop (and `reopen()`) from a script thread
  every few milliseconds for thousands of cycles with each encoder: no crash,
  no write to a closed destination, every closed file complete.
- **Count and traces** — 5: 86329b9dd (telnet reopen serialised, 2019),
  #3038 (163b83bf9; #3064 / 96cf5526a, per-instance lock), caef89dd8 (atomic
  state), #4819 (3475eb852, #4821, tests GH4819, GH4819-bis).
- **Reconciliation** — Covered: [encoder.md §4](encoder.md#4-serialisation-protect)
  and [output-file.md §7](output-file.md#7-threads-and-locks); the
  start/stop race itself is the generic output's.

### KF-11. Every operation in every lifecycle state

- **Situation** — The output's state has more than two values: closed,
  open-without-instance, open-with-instance, in an error delay. Metadata,
  pieces, stop and close can arrive in any of them.
- **What went wrong** — A run of crashes, each on an unexpected state:
  metadata arriving before an instance existed, close on an already closed
  destination, stop when never opened, close failing again while handling an
  error ("degraded mode"), an assertion when an external output restarted
  after silence.
- **Why it is hard** — Lazy creation, error retry and reopen each add a state;
  each operation was written against the state its author had in mind. The
  instance-level "stop exactly once" is also not enforced.
- **How to tell** — A state × operation table with the defined outcome of
  each cell, and a test that drives every cell with a recording instance.
- **Count and traces** — 6: 6b9ed7e0a (#876), #993, 59d173086 (#3047),
  163b83bf9, 0dfb50633, e442c1aa4. Findings seam C3.
- **Reconciliation** — Covered: [output-file.md §2](output-file.md#2-instance-lifecycle)
  (slot `none`/`pending`/`created`, close only when open) and
  [encoder.md §3.7](encoder.md#37-stop) (second `stop` implementation-defined).

### KF-12. An encoder that owns its destination bypasses the output

- **Situation** — For most formats the output writes the bytes; one encoder
  family opens, seeks and closes the file itself, the output only naming it.
- **What went wrong** — With that family: directories were not created,
  `perm`, `dir_perm` and `append` were ignored, append did not work (a library
  bug), file-specific options written into the format leaked into the
  script's shared format value, reopening flooded the log, `flush` is
  silently ignored. Each output-level option had to be re-plumbed through the
  format.
- **Why it is hard** — The seam has no notion of "this instance writes the
  destination itself"; the output learns it from the format kind and passes
  the destination, the append mode and permissions as format fields. Every
  new output option must be implemented twice. Format values are shared, so
  per-file rewrites must copy.
- **How to tell** — Run the file-output option matrix (directory creation,
  perm, dir_perm, append, flush, reopen) for both a bytes-returning format
  and a self-writing format: same observable outcome, or a documented
  difference; the script's format value prints the same before and after.
- **Count and traces** — 5: #2069 (8519aaf71), #2072 (a71822bf0), 1c17b384a,
  #2070, findings file-callers 8, 9, 22.
- **Reconciliation** — Covered: [output-file.md §1](output-file.md#1-formats-accepted-and-how-the-factory-is-obtained)
  and [§4.3](output-file.md#43-ffmpeg-file-variant),
  [encoder.md §7](encoder.md#7-where-the-bytes-go-what-the-seam-assumes);
  seam gap G6 is the agenda.

### KF-13. Append is the concatenation of complete streams

- **Situation** — `append=true`, across reopens or across output lifetimes.
- **What went wrong** — Each instance writes a full stream from its header,
  so appended files hold repeated WAV headers, repeated FLAC stream headers,
  a second ID3 tag mid-file; only chained Ogg and headerless formats give a
  valid file. Users splitting then concatenating hit the same wall. The only
  append regression test checks that a recreated output starts, not the
  bytes.
- **Why it is hard** — True append needs the encoder to know it continues an
  existing stream (skip header, possibly rewrite sizes), which the seam does
  not express; seekable containers cannot be finalised in append mode.
- **How to tell** — Append two runs per format and decode the result with a
  standard tool: total duration equals both runs, or the format is refused
  or warned about at configuration.
- **Count and traces** — 3: #2072 (append broken for the self-writing
  family), #4819 (tests GH4819, GH4819-bis), #2837 comments. Findings
  file-callers 14.
- **Reconciliation** — Covered: [output-file.md §5.4](output-file.md#54-append).

### KF-14. A joiner's header is a live query, and must match where it joins

- **Situation** — Outputs whose consumers join late (network listeners,
  peers) ask the instance for the header to send before the current bytes,
  and for the offset of the latest restart (new logical stream).
- **What went wrong** — The header was a field captured at instance creation
  and so empty or stale for encoders that produce it later: Ogg streams to
  listeners were undecodable after a release. Later, a late listener got the
  current link's header followed by bytes of the previous link. Today the
  header is empty for one family with delayed codecs, and for native FLAC
  and external process encoders whose stream does start with a header.
- **Why it is hard** — The header changes over an instance's life (chained
  streams) and some encoders learn it only after encoding; a file consumer
  never asks, so a wrong answer is invisible to file tests.
- **How to tell** — Per encoder: join at random byte offsets (before first
  output, mid-link, just after a metadata change) and decode `header() ++
bytes from the restart offset`; it decodes for every encoder whose stream
  has a header.
- **Count and traces** — 3: #3265 (318cba9f7, #3276, test GH3276), #5546
  (f25b5b045), verification-callers V1. Findings seam C1, A1.
- **Reconciliation** — Covered: [encoder.md §3.4](encoder.md#34-header) and
  [§3.5](encoder.md#35-restart_offset) describe the as-built answers; C1 and
  V1 are the defects.

### KF-15. Metadata order relative to the bytes of a piece

- **Situation** — A frame is cut into pieces at metadata positions; for each
  piece the output hands the metadata and the audio to the instance, and
  sends the resulting bytes.
- **What went wrong** — Early on, inserting metadata re-entered sending and
  reset the encoder mid-frame; in 2022 the first track's metadata never
  reached the server after a (re)connection because metadata was handled
  before the bytes were sent, and the order was then made per-output
  (data-first for in-band server metadata, metadata-first for encoders).
- **Why it is hard** — Two consumers want opposite orders: an encoder that
  restarts its stream on metadata needs it before the piece, a protocol that
  sends metadata out of band needs the preceding bytes out first. A metadata
  at the very end of a frame belongs to no piece.
- **How to tell** — With a recording instance: for a frame with metadata at
  0, mid-frame and at the end, the call log shows one metadata per piece in
  the declared order, and the end-of-frame metadata is delivered with the
  next frame (or the spec says it is dropped).
- **Count and traces** — 3: 681a5a092 / 335bb7841 (#386, 2009), #2506
  (33f8c962f, core test output_encoded_test). Findings file-callers 13, 19.
- **Reconciliation** — Covered: [output-encoded.md §3.1–§3.3](output-encoded.md#3-call-sequence);
  the end-of-frame metadata being "never handed on" is stated as-built.

### KF-16. Metadata through a process of unknown latency

- **Situation** — The `pipe` operator sends audio to a child process and reads
  audio back; metadata and track marks must reappear on the returned audio at
  the right place.
- **What went wrong** — The process output's own header was not read, so
  audio was garbled; then, because the delay through the process is unknown,
  the operator restarted the process at every metadata, producing clicks and
  level jumps in audio processors (Stereo Tool) at every track; a
  time-based replay was added as the alternative. A break-count assertion
  fired after a rewrite. Today the header sent after a restart carries the
  process's rate rather than ours, and metadata positions are cut in the
  wrong unit when main and audio rates differ.
- **Why it is hard** — The process is a black box with internal buffering:
  neither its latency nor its framing is known, and restarting it to
  resynchronise destroys its state. Rates read from and written to the
  process are easy to conflate.
- **How to tell** — Pipe through an identity process with a fixed delay:
  metadata come back within one frame of their input position without a
  restart (or with a restart, the spec says so); a resampling process keeps
  the header sent to it at our rate across restarts.
- **Count and traces** — 4: #607 / #614 (f9970afe6), #885 (943a76168, #899),
  #1786 (5557ba3e3), findings file-callers 4, 5, 20.
- **Reconciliation** — Covered: [output-pipe.md Part 2 §3–§5](output-pipe.md#part-2-the-pipe-operator);
  the rate and unit errors are findings 4 and 5.

### KF-17. Two parsers of one format: static type and built value

- **Situation** — A format's channel count and track presence are part of its
  static type, computed from the written parameters before evaluation; the
  value is built separately from the same parameters at evaluation.
- **What went wrong** — `stereo=true|false` and `mono=…` rejected after a
  typing rework (cookbook examples broke); `%external(video=true)` typed as
  audio only; a list of formats with different channel counts was given a
  common supertype that erased them; formats of different track sets were
  accepted for one another so an encoder silently dropped tracks (2026);
  generalisation of format types broke when two outputs were registered in a
  different order. Today the type reads the first deciding parameter and the
  builder the last, so a script passes `--check` and fails at run time, and
  some parameters accepted by one are rejected by the other.
- **Why it is hard** — The type must be known before the value exists, so
  the parameter grammar is implemented twice; the language's convenience
  subtyping (lists, records) is unsound for a type that fixes data layout.
- **How to tell** — For every format name and every parameter combination in
  a generated corpus: the statically computed type equals the type of the
  built value, and any parameter rejected by one is rejected by the other at
  the same phase.
- **Count and traces** — 6: #1331, #2867 (78ae7d596), #3217 (c0f95a132,
  e442c1aa4), #3472 (25d5a3f1f), 9f45cf6db, cb5959873 (typers split from
  builders). Findings seam D1, D2, D3, G2.
- **Reconciliation** — Covered: [formats.md §6, §6.2](formats.md#6-typing);
  D1–D3 are the defects.

### KF-18. When a format is validated

- **Situation** — A format value passes through parsing, static typing,
  evaluation, instance creation (at output start) and the library's own
  configuration (at first encode or connect). Some defaults depend on
  settings a script may change after writing the format.
- **What went wrong** — Building formats at parse time hard-coded defaults
  that ignored later settings (sample rate); moving construction to run time
  moved errors from "before start" to "while streaming", with the stated
  question "how bad is this?" left open. Invalid sample rates now pass
  `--check` and evaluation and fail the clock at start; a codec profile the
  system library lacks fails only when sending data; a missing inner Ogg
  codec is found at output start.
- **Why it is hard** — Some checks need values that exist only later
  (settings, the library actually linked); others are cheap early. Without a
  rule, each option lands wherever its author validated it, and the same
  class of mistake surfaces at different times with different error kinds.
- **How to tell** — For a catalogue of invalid configurations, record the
  phase and error kind at which each is reported; the catalogue matches a
  stated rule (e.g. everything not depending on runtime settings fails at
  `--check` or evaluation, with a position).
- **Count and traces** — 5: #1569 / #1858 (f73c08d2b), #933, #1942
  (e4d6693a7), #4053, seam D10. Findings seam G1, G3.
- **Reconciliation** — Covered: [formats.md §4–§5](formats.md#4-evaluation-building-the-format-value)
  and [encoder.md §2.3](encoder.md#23-instance-creation) as-built; the rule
  itself is for the normative pass.

### KF-19. "Is this encoder available" answered by kind, not by codec

- **Situation** — Scripts and tests branch on whether an encoder is compiled
  in; error messages must tell a user which optional library is missing.
- **What went wrong** — Tests failed on builds without an encoder until a
  conditional was added; users on minimal builds get a bare "unsupported
  format" with no hint at the missing library (the dedicated error is
  unreachable); the conditional answers yes for an Ogg codec whose library is
  absent, and swallows every exception as "absent". Nothing tests it.
- **Why it is hard** — Availability is a property of the codec inside a
  container, not of the container plug-in; a check that builds a default
  format conflates "absent" with "defaults invalid".
- **How to tell** — With a registry double that withholds each codec in
  turn: the conditional takes the else branch, and the evaluation error names
  the missing component.
- **Count and traces** — 3: #116 (577299a19, 5f8ff8f73), #3934, seam D6/D10,
  G4.
- **Reconciliation** — Covered: [formats.md §1](formats.md#1-syntax) and
  [output-pipe.md Part 3](output-pipe.md#part-3-language-hooks); D6, D10 are
  the defects, [tests.md §7](tests.md#7-ifencoder) the gap.

### KF-20. Process and blocking destinations run on the streaming thread

- **Situation** — An output writes to a child process's input or to a FIFO or
  slow device; open, write and close are blocking calls on the thread that
  animates the clock.
- **What went wrong** — A process that could not start gave an internal
  configuration exception as its message; a process exiting early surfaces as
  a broken pipe about a second later, after the buffers fill; the external
  output is logged and named as the file output, and lacked start/stop
  callbacks until 2026; a FIFO with no reader blocks the clock's start;
  closing waits for the process to exit on the clock thread; a full disk took
  the whole process down.
- **Why it is hard** — Process lifecycle (spawn, early exit, exit status,
  waiting) and pipe buffering are asynchronous by nature, while the output
  model is a synchronous write per cycle. Errors appear frames after their
  cause, and two outputs sharing one implementation inherit each other's
  identity.
- **How to tell** — With process doubles (one recording stdin, one exiting
  early, one never reading) and a FIFO without reader: the error names the
  command, is reported within a bounded time, follows the retry policy, and
  the clock's latency stays bounded (or the spec states the block).
- **Count and traces** — 5: #2600, #2756, 98ec71f70, #4422, findings
  file-callers 3, 17.
- **Reconciliation** — Covered: [output-pipe.md Part 1 §4, §6](output-pipe.md#part-1-outputexternal)
  and [output-file.md §4.5](output-file.md#45-non-regular-files); the
  naming is finding 3.

### KF-21. Binary metadata on the encoder path

- **Situation** — Cover art and similar binary entries travel with metadata;
  some encoders can embed them, most outputs should not carry them.
- **What went wrong** — Cover support was added to the encoder path; later,
  memory grew with every track carrying a cover, and an export list for cover
  keys was added per output. Today file outputs export covers by default
  against the generic output rule, and one cover key is exported even when
  covers are disabled.
- **Why it is hard** — Two lists (exported keys, cover keys) must agree; each
  output decides its default; the cost is invisible until long runs.
- **How to tell** — For each output and both settings of the cover option,
  the keys reaching the encoder are exactly the declared set; no cover key
  outside the cover list is exported.
- **Count and traces** — 2: de2b2c75c (#1058), #3504 / #3591 (a86be5bcd).
  Findings file-callers 6.
- **Reconciliation** — Covered: [output-encoded.md §3.2](output-encoded.md#32-which-metadata-is-handed-on-and-in-what-form)
  (both lists and the per-output defaults); finding 6 is the defect.

---

## Area N: Network outputs and the `%ffmpeg` destination

**Sources read.** As-built files `output-icecast.md`, `output-harbor.md`,
`output-srt.md`, `output-url.md`, `output-hls.md`, `output-encoded.md`,
`ffmpeg.md`, `findings/net-callers.md`, `findings/ffmpeg.md`,
`findings/verification-callers.md`; the four neighbouring known-complexity
lists (harbor, HLS, SRT, FFmpeg), whose entries are linked and not repeated.
History: all 220 commits of the Icecast source client (`git log --follow`,
2008-10 to 2026-11, through `src/outputs/` and `src/core/outputs/`), its
shared helpers (`icecast_utils.ml`, 2010–2026), the vendored protocol
library (`src/modules/cry`, 2026), the `%ffmpeg` encoder under its old and
new paths (2019–2026), `output.url` (2021–2026), the encoded-output base
(`-S data_first`, `--grep '#2506'`), `-S O_APPEND`, `-S '"truncate"'`,
`-S send_last_metadata_on_connect`, `-S icy_song`, `--grep interrupt`.
`CHANGES.md` from 0.3 to 2.5.0 (every `icecast`, `shoutcast`, `icy`,
`output.url`, `append`, `header`, `mp4` line). Issues and PRs (`gh`
searches: icecast reconnect, icecast metadata, shoutcast, icy metadata,
output.icecast, output.url, rtmp, ffmpeg mp4 stream, moov, faststart,
matroska icecast, ffmpeg header harbor, id3 mid-stream, xing, ffmpeg append,
ffmpeg ogg metadata chain), read in full: #180, #305, #320, #377, #490,
#496, #541, #548, #727, #1092, #1100, #1394, #1486, #2069, #2109, #2506,
#2632, #2731, #2819, #2917, #2937, #2965, #3021, #3185, #3265, #3427,
#3635, #3770, #3906, #4408, #4442, #4501, #4669, #4672, #4785, #5198,
#5284, #5306, #5344, #5484, #5519; PRs #2997, #3626, #4317, #5541, #5691.
Tests: `tests/streams/icecast_*.liq`, `test_icecast_*.liq`,
`tests/regression/{LS503,GH4629,GH2850,fallible_ogg,shoutcast-args}.liq`.

Already covered elsewhere, not repeated here: late joiners on a shared
encoder ([harbor K1](../../outputs/spec/harbor/known-complexity.md#k1-a-late-listener-must-join-at-a-point-a-decoder-can-start-from)),
per-listener ICY state ([harbor K8](../../outputs/spec/harbor/known-complexity.md#k8-icy-negotiation-and-per-listener-metadata-state)),
SRT peers and the shared encoder ([SRT KC6](../../optionals/srt/spec/known-complexity.md#kc6-every-connection-is-a-new-stream-and-a-shared-encoder-is-not)),
HLS init segments and container restarts
([HLS KC21–KC23](../../outputs/hls/spec/known-complexity.md#encoder-seam)),
and FFmpeg timestamps
([FFmpeg list](../../optionals/ffmpeg/spec/known-complexity.md)).

---

### KN-1. The encoder's lifetime follows the connection, not the output

- **Situation** — A source-client output has two lifecycles: the output's
  own start and stop, and a connection to a remote server that fails and is
  re-established on its own, with a delay, while the output stays started.
  The encoder instance, its headers and its codec state must belong to one
  of them.
- **What went wrong** — Restart logic was rewritten again and again: the
  encoder was reset through the output's reset, then directly; output and
  connection start/stop were merged, which broke delayed reconnection; an
  Ogg stream stopped for good after one disconnection; metadata arrived
  while no encoder existed (between a failure and the next attempt) and
  crashed the output. Hooks for connect/disconnect had to be added because
  the output's own start/stop said nothing about the connection.
- **Why it is hard** — The same object (the output) carries two state
  machines whose transitions interleave; any operation (metadata, stop,
  encode) can arrive in the gap where the connection is down and no encoder
  exists. Formats that tolerate a mid-stream start (MP3) hide the bug; Ogg
  does not.
- **How to tell** — With an Ogg encoder, cut the server three times; every
  new connection's body must start with the format's full headers and
  decode alone. Inject metadata, stop and restart while disconnected and
  waiting to retry; none may raise. Connect/disconnect notifications must
  pair one to one with connections.
- **Count and traces** — 8: a0bfb0120, 813b41c62, 2bee80b3f, 7327a5914,
  09dda268d, 681a5a092, db37af639; CHANGES 1.0.0 LS-559 ("permanent stop
  after disconnection on Ogg streams"). Same difficulty on SRT:
  [SRT KC6](../../optionals/srt/spec/known-complexity.md#kc6-every-connection-is-a-new-stream-and-a-shared-encoder-is-not).
- **Reconciliation** — Covered:
  [output-icecast.md §2](output-icecast.md#2-instance-lifecycle) (one fresh
  instance per connection) and §3.3 (metadata dropped without an instance).

### KN-2. Teardown must finish even when closing fails

- **Situation** — A connection dies (peer gone, network down). Teardown
  writes closing bytes (a chunked terminator, the encoder's final bytes),
  closes the socket, releases per-connection resources, then schedules a
  retry. Each closing write can fail on the dead socket.
- **What went wrong** — A failing close raised out of the error handler,
  the retry was never scheduled, and the output stayed "started" forever,
  logging "will reconnect" and never doing it, while the rest of the script
  played on. In chunked mode the terminator write failed before the state
  was marked disconnected, so the output believed itself connected and
  silently dropped everything. A failed connection did not release its
  socket: one descriptor leaked per attempt until the process ran out.
  Memory grew over days of failed attempts (headers accumulated per
  attempt).
- **Why it is hard** — Teardown runs precisely when the transport is
  broken, so every step of it can fail, and the failure path is the least
  tested one. The symptom is silence, not an error: nothing is logged after
  the last "will reconnect".
- **How to tell** — For every protocol variant (plain/chunked, HTTP/ICY,
  TLS), kill the server mid-write, bring it back: the output must reconnect
  within the retry delay. Fail the connection 10 000 times against a closed
  port: open descriptors and resident memory stay flat. Assert the output's
  observable state (connected or not) matches the socket's.
- **Count and traces** — 7: 29bd3a662 (#3363), 5a1139b9b (#3427),
  a7a6a1a42, b5fe63022 (#5519), #548 (CHANGES 1.3.4), e4c6ffc2f (#490),
  #541.
- **Reconciliation** — Covered:
  [output-icecast.md §5](output-icecast.md#5-reopen-reconnect) (teardown
  ignores errors at each step). Not covered: no bound is stated on
  resources held across failed attempts.

### KN-3. Connecting lazily couples metadata order to the first bytes

- **Situation** — A source-client output connects only when there are
  bytes to send, so as not to connect for a source that fails at once. The
  encoded-output base hands, per piece, a metadata and the piece's data in
  some order. Out-of-band metadata needs an established connection; in-band
  metadata must precede the data it labels.
- **What went wrong** — The first track's title never reached the server
  at start-up: the metadata came before the connection existed and was
  dropped. The global order was flipped to data first; years later, in-band
  consumers (HLS timed ID3) found their first segment without metadata, and
  the order became per-output (metadata first by default, data first only
  for the out-of-band mode). The piece during which the connection is made
  is never encoded: the first piece of every connection is lost.
- **Why it is hard** — One ordering serves two contradictory needs (in-band
  wants metadata first, out-of-band wants a connection first), and lazy
  connection makes "connected" a side effect of sending. The lost piece is
  40 ms: inaudible, invisible to any test that does not count bytes.
- **How to tell** — Start a source with metadata at position 0 against a
  recording server: the first out-of-band update carries it. With in-band
  metadata, the first bytes after the headers belong to the labelled track.
  Count encoded samples per connection against samples produced: equal from
  the first frame after the connection decision.
- **Count and traces** — 4: d6ab2226f (#2506), edeba8e1c (#5117), CHANGES
  2.0.4 ("connect when data is available"), finding net-callers D3.
- **Reconciliation** — Covered:
  [output-icecast.md §3.2](output-icecast.md#32-per-piece) and
  [output-encoded.md §3.3](output-encoded.md#33-order-within-a-piece). The
  lost first piece (D3) is described, not ruled: the normative pass must
  decide it.

### KN-4. The server's current metadata after a (re)connection

- **Situation** — The remote server holds "now playing" per connection. A
  reconnection (server restart, network cut) or a late start leaves it
  empty until the next metadata change, which for a long track or a live
  DJ may be an hour away. Metadata may also be filtered down to the keys the
  server displays.
- **What went wrong** — After a server restart the title stayed blank
  until the next track. A live DJ's initial metadata never reached the
  server. A wrapper that invented metadata for tracks without any sent an
  update with nothing displayable, and the server showed "Empty Title"
  between tracks.
- **Why it is hard** — Metadata is an event stream, but the server needs
  state; the output must keep the last value and re-assert it on every
  connection, and decide what an update with no displayable key means
  (clear, or ignore).
- **How to tell** — Set a title, restart the server, wait for the
  reconnection: the server shows the title before the next change. Send a
  metadata whose keys are all filtered out: the server's title is
  unchanged.
- **Count and traces** — 5: #2506 (second half), #3906, #2109, 5cb4ad6e0
  (#4270), a36cd9ffe (#4408).
- **Reconciliation** — Covered:
  [output-icecast.md §3.3](output-icecast.md#33-metadata-step)
  (`send_last_metadata_on_connect`). Not covered: whether an update left
  with no key after filtering is sent at all; and the replay's errors are
  swallowed silently (finding G4).

### KN-5. In-band metadata restarts a chained stream; clients cope badly

- **Situation** — A metadata change can be carried in band (the encoder
  ends its logical stream and starts a new one with new headers: an Ogg
  chain) or out of band (a request to the server; the encoder never knows).
  Which one is used is chosen per format, and the server and the players
  each handle chains differently.
- **What went wrong** — Players (browsers, VLC, ffmpeg-based clients)
  stalled, glitched or dropped at every track change on Ogg/Opus streams.
  Opting into out-of-band updates had to stop the encoder from restarting
  too. An FFmpeg-encoded stream restarted the whole output on each metadata
  and crashed on track change; the fix inserted metadata at the container
  level, then Ogg needed real chains back. Only one FFmpeg Ogg muxer name
  chains; the others drop the metadata. A server spliced ICY blocks into Ogg
  bodies (harbor K8). Users still ask for out-of-band titles without chains.
- **Why it is hard** — The chain is valid by the format but poorly
  supported; whether to restart is the encoder's property while the choice
  of channel is the output's; and the audible damage depends on the client.
- **How to tell** — For each format and channel choice, record the body
  over three metadata changes: with out-of-band metadata there is exactly
  one logical stream; with in-band, one link per change, each decodable
  alone and in order. Count both for every muxer alias of a container.
- **Count and traces** — 8: 90327a8a1, 1357a0abb, CHANGES 1.0.0 beta2 ("ICY
  metadata updates when streaming AAC"), cd541d499 (#2997), f8a9536b1
  (#1100), f4dee266a (#1729), #180, #5306 (open); finding ffmpeg F5.
- **Reconciliation** — Covered:
  [output-icecast.md §1.3, §3.3](output-icecast.md#13-icy-metadata-mode),
  [ffmpeg.md §13.5](ffmpeg.md#135-chaining-ogg-restarts-and-the-destination).
  Not covered: the listener-serving output has no out-of-band-only mode
  (#5306), and the muxer-alias asymmetry (F5) is unruled.

### KN-6. The out-of-band update is a separate, synchronous request to an unreliable endpoint

- **Situation** — Each out-of-band metadata update opens a new connection
  to the server's admin endpoint, in one of several dialects, and waits for
  the answer on the thread that streams.
- **What went wrong** — A slow update (a third-party directory, a stalled
  connection fallback, a server in the same process) stalled the stream for
  seconds per track, until the server kicked the source for timeout or the
  clock reported a catch-up. Reusing one socket for updates was tried and
  reverted as non-trivial. Shoutcast's answer is undocumented: sometimes
  none although the update applied, so its errors were downgraded to "may
  have failed". Updates with a stream id other than 1 went to the wrong
  stream. An update endpoint answering 404 while the stream continued was
  part of a never-reconnecting failure.
- **Why it is hard** — Metadata looks small and is treated as a side
  channel, but it is network I/O with its own failures and latencies, done
  inside the streaming cycle; servers differ in endpoint, authentication and
  answer.
- **How to tell** — Point the update endpoint at a peer that accepts and
  never answers: the stream's bytes keep flowing at real-time rate. An
  endpoint that fails leaves the source connection up. With a stream id of
  2, the update names id 2.
- **Count and traces** — 7: 25930b1a7 reverted by db29cc846, 5ca59a740,
  9197f6f7e (#320), #377, #5484 (a7754004a), #3427.
- **Reconciliation** — Covered as built:
  [output-icecast.md §4.3, §6](output-icecast.md#43-out-of-band-metadata)
  (synchronous; failure keeps the connection). Not covered: no bound on how
  long an update may hold the stream, and no statement that it must not.

### KN-7. Character encoding of what is sent to the server

- **Situation** — Metadata values, and also the stream's name, genre and
  description, cross from the script's UTF-8 strings to a server protocol
  whose charset depends on the dialect (ICY historically Latin-1, Icecast
  UTF-8 with an explicit `charset`).
- **What went wrong** — Accents arrived mangled on Shoutcast; the stream
  name was not re-encoded with the metadata; the default charset changed
  twice (Latin-1 for ICY, then UTF-8 everywhere); a sanitiser elsewhere ate
  the bytes after an invalid UTF-8 byte, and the report blamed the output.
- **Why it is hard** — Several strings take different paths to the wire;
  the server and its web pages re-encode again; an invalid byte upstream
  looks like an output bug.
- **How to tell** — Send a title, name, genre and description containing
  multi-byte and non-Latin-1 characters through each dialect and each
  charset setting: the recorded request bytes match the declared charset
  exactly, and an unrepresentable character has a stated fate.
- **Count and traces** — 7: 777060c7b, 1c32d35a3, fd2047fb2, f97d6aafc
  (#1092), fa61f7cea (#2704), #5284 (open), CHANGES 1.0.0 beta2 ("encoding"
  parameter).
- **Reconciliation** — Covered for metadata values:
  [output-icecast.md §3.3](output-icecast.md#33-metadata-step). Not
  covered: re-encoding of name, genre and description, and characters the
  target charset cannot represent.

### KN-8. Stream properties announced before the encoder knows them

- **Situation** — The source-client request head announces bitrate,
  quality, sample rate and channels. It is sent at connection time, before
  the encoder has encoded anything; an encoder that creates its streams on
  the first frame does not know its bitrate yet.
- **What went wrong** — The server's bitrate field was missing for
  FFmpeg-encoded streams; it was "fixed" by asking the encoder, which only
  answers after its first frame, so the field is still never sent (checked
  in pass 1). The reported value itself was also wrong once. Wrong audio
  info was sent in 2007.
- **Why it is hard** — The information exists in the format for most
  encoders and only in the running instance for others; the announcement is
  once per connection and cannot be amended.
- **How to tell** — For every format with an explicit bitrate, the first
  request head carries that bitrate. A check that only looks for the field
  in some format passes while the FFmpeg one is absent.
- **Count and traces** — 4: CHANGES 0.9.0 (#226), 070eadce5 (#3185),
  8aad12080 (#3295); finding net-callers D2.
- **Reconciliation** — Contradicts:
  [output-icecast.md §1.4](output-icecast.md#14-stream-info-announced)
  describes the field as never sent for `%ffmpeg`, which is the bug #3185
  was meant to fix.

### KN-9. Blocking network I/O inside the streaming cycle holds every output of the clock

- **Situation** — Connect, handshake, writes and metadata requests run in
  the clock's cycle. Several network outputs share one clock, or share the
  scheduler's workers. One server becomes slow or unreachable.
- **What went wrong** — When one server went away, every other output of
  the same clock disconnected too (writes stalled past their timeout while
  the cycle waited). Users were told to give each output its own clock and
  buffer. A flaky upstream input made only the output with the largest
  writes disconnect. With as many blocking outputs as scheduler workers,
  nothing else (the embedded server, telnet) ran until connections timed
  out after 30 s.
- **Why it is hard** — A blocking call is invisible in the cycle's
  structure; its cost lands on whoever shares the thread, and only under
  network failure, which tests rarely simulate.
- **How to tell** — Two network outputs on one clock, one server stalled
  (accepts, never reads): the other output's bytes keep their real-time
  rate, or the specification says they need not and states the isolation
  mechanism. With N blocking outputs on N workers, an unrelated task still
  runs within its deadline.
- **Count and traces** — 7: #305, #2965, #4442, #3427 (16 s catch-up),
  e983941e5 (#5691), 3db6b5cc7, #377. Same principle on SRT:
  [SRT KC2](../../optionals/srt/spec/known-complexity.md#kc2-blocking-connection-work-on-the-thread-or-lock-the-stream-needs),
  [KC13](../../optionals/srt/spec/known-complexity.md#kc13-who-paces-the-clock-and-when);
  harbor: [K4](../../outputs/spec/harbor/known-complexity.md#k4-how-a-writer-learns-it-can-write).
- **Reconciliation** — Covered:
  [output-icecast.md §7](output-icecast.md#7-threads-and-locks) (its clock
  is animated by a thread). Not covered: what one stalled output may do to
  others on the same clock.

### KN-10. Bounding connects and writes, portably

- **Situation** — Every connect and write to a server must be bounded in
  time. The bound is implemented with the platform's socket primitives,
  which differ across systems and across plain/TLS transports.
- **What went wrong** — Connections hung with no timeout; socket timeout
  options proved unreliable across platforms and were removed in favour of
  readiness waits; the connect timeout first did not work on Windows,
  then reported a refused connection only through a different readiness
  set, so a refused first address stalled for the whole timeout on each
  connection and each metadata update; a TLS transport to a non-TLS server
  deadlocked.
- **Why it is hard** — "Timeout" is a composition of non-blocking connect,
  readiness waits and error retrieval whose details are per OS; the failure
  only appears on the platform nobody tests on.
- **How to tell** — On every supported OS and transport: connect to a
  black-holed address (returns within the connect timeout), to a refusing
  port (returns at once), to a dual-stack name whose first address refuses
  (falls back at once), and write to a peer that never reads (returns
  within the write timeout).
- **Count and traces** — 8: 8e93bff3c (LS-555), 7c70129c4, 7692153b5
  (LS-508), 2f7eaf3b1, a56806ff3, c17e9c8cf, a7754004a (#5484), CHANGES
  2.2.4 (#3681).
- **Reconciliation** — Covered:
  [output-icecast.md §4.2, §4.3](output-icecast.md#42-body) (`timeout` per
  write, connection timeout). Not covered: transport (TLS) and per-platform
  behaviour.

### KN-11. Address resolution and family preference

- **Situation** — A server host name resolves to IPv4 and IPv6 addresses;
  some servers listen on one family only.
- **What went wrong** — Host names failed to resolve at all for a release;
  a server version refused IPv6 sources; preferring IPv4 by default was
  committed and reverted within days; an explicit per-output and global
  preference was added later.
- **Why it is hard** — The right default depends on the network, not the
  program; a wrong default fails only for some users.
- **How to tell** — With a name resolving to a refusing address first and
  a listening one second, the output connects, under each preference
  setting, to the expected family.
- **Count and traces** — 5: #496, #2632, 290b88215, 460122172 reverted by
  dfb50dbb7 (#3626), 3472313eb (#4317). Same for SRT:
  [SRT KC16](../../optionals/srt/spec/known-complexity.md#kc16-address-resolution-and-dual-stack).
- **Reconciliation** — Not covered: the as-built Icecast file says nothing
  about resolution or preference.

### KN-12. Which failures reconnect, which stop the output, which stop the script

- **Situation** — A network output can fail at connect, during the
  handshake (authentication), during writes, inside the encoder, or at the
  destination (URL outputs). A user policy decides retry delay or giving up.
- **What went wrong** — Restart semantics changed from a boolean to a
  callback returning a delay; "no restart" still retried once; a 401 at
  reconnection bypassed the policy and crashed the script; an unreachable
  URL crashed instead of retrying; the URL output's restart delay was not
  applied, then applied only on encode; giving up with a null delay was
  reported to crash. Giving up today leaves the output started and silently
  dropping every frame. The retry delay is rounded up to the next whole
  second. Users could not see when a URL output was actually connected.
- **Why it is hard** — Failures arrive at different layers (socket,
  protocol, encoder, FFmpeg protocol) with different error types, and each
  path was routed to the policy separately; an unrouted path becomes a
  crash or a silent stall.
- **How to tell** — Inject each failure kind (refused, timeout, 401,
  write error, encoder error, destination error) at first connection and
  at reconnection: each reaches the policy exactly once, and the result
  (retry after d, give up, fail) is the one the policy returned, with d
  measured to 100 ms.
- **Count and traces** — 11: 02e22da11, 08ea33fe5, 63878e1e0 (LS-535),
  284b75815 (#3635), #1486, 45882c24c, 19d9004e2, da214513e (#2731),
  96e990fee (#2874), #4669, #4672; findings net-callers D4.
- **Reconciliation** — Covered:
  [output-icecast.md §5, §6](output-icecast.md#5-reopen-reconnect),
  [output-url.md §5, §6](output-url.md#5-reopen-reconnect). Not covered:
  encoder errors on the source client are not routed to the policy (they
  fail the output), an asymmetry with the URL output, and "give up" is not
  observable.

### KN-13. Silence on the wire: the server judges liveness by bytes

- **Situation** — A server drops a source that sends nothing for its
  source timeout. An encoder may hold bytes for a long time (a lossless
  codec encoding digital silence emits very little and flushes late), and a
  fallible output stops, and closes its connection, whenever its source is
  unavailable.
- **What went wrong** — An Ogg/FLAC stream disconnected (broken pipe) each
  time the input fell back to silence, while MP3 and AAC outputs of the same
  source stayed up; reports were misread for two years as a clock problem.
  A URL output to a live platform closed and reopened its connection at
  every gap between files, and the platform was not ready to accept again.
- **Why it is hard** — The cause (encoder buffering, source availability)
  is far from the symptom (a write error); silence is the input real
  streams fall back to, and the input tests avoid.
- **How to tell** — Feed digital silence for twice the server's source
  timeout through each encoder to a server with a short source timeout: the
  source stays connected. State whether a source gap closes the connection,
  and check it.
- **Count and traces** — 3: #2965, #3770, #2937.
- **Reconciliation** — Not covered: no maximum interval between bytes
  sent, and no statement of what a source gap does to the connection.

### KN-14. Protocol dialects

- **Situation** — "Icecast" is several protocols: HTTP SOURCE, PUT or
  POST, HTTP/1.0 or chunked HTTP/1.1, ICY (Shoutcast v1, on port+1, with
  password-line authentication) and Shoutcast v2 with stream ids and
  capability lines. Header spelling and the update endpoint differ.
- **What went wrong** — A header written without a space after the colon
  failed some servers; the stream id was ignored by the wrapper and every
  source went to id 1; metadata updates with an id other than 1 failed;
  making the main output Icecast-only broke Shoutcast v2 scripts; the
  chunked mode had its own never-reconnect bug; servers not following the
  spec (other Shoutcast versions, Rocket) refused connections.
- **Why it is hard** — Each server version is a dialect with undocumented
  details, and only real servers reveal them; tests run against one.
- **How to tell** — For each dialect, a recording fake server checks the
  head byte for byte, the authentication, the stream id in both the
  connection and the update, and the chunked framing including its
  terminator.
- **Count and traces** — 10: 76357b981, 96bfb33fb, 197a8b2c3, c573c0ed3,
  9197f6f7e (#320), cdb3c1369, ad3a0d2c9 (#2819), CHANGES 1.3.7 (#727),
  #3021, b5fe63022 (#5519).
- **Reconciliation** — Covered:
  [output-icecast.md §4.1, §4.3](output-icecast.md#41-request-head).

### KN-15. The content type a server and players accept is not the format's MIME

- **Situation** — The output announces a content type that the server uses
  to pick its own parser (to find pages, headers, burst points) and that
  players use to choose a decoder.
- **What went wrong** — A server did not treat `audio/ogg` as Ogg, so Ogg
  became `application/ogg` whatever it carried; FLAC had no MIME, then a
  misspelled one; FFmpeg formats needed a MIME table and later an explicit
  override; a user forcing `audio/ogg` with an FFmpeg FLAC encoder got a
  stream an older server mangled ("headers mismatch").
- **Why it is hard** — The MIME is a contract with third-party parsers that
  differ by version; the same container needs a different announcement for
  different servers.
- **How to tell** — For every format, the announced type is one a reference
  server parses into its burst logic (a late listener of the server decodes)
  and the override is passed verbatim.
- **Count and traces** — 7: e95887ee3, 66d507fa9, ac1c987c3, 79f22d1a8,
  f356c8499, 8d3f51e4d, #5198; finding seam D8.
- **Reconciliation** — Covered:
  [output-icecast.md §1.2](output-icecast.md#12-content-type).

### KN-16. The same format gives different bytes, or none, by destination

- **Situation** — A muxer adapts to its destination: seekable (a file) or a
  pure byte stream (network, pipe, append). Stream-start preambles (ID3v2,
  Xing/Info, sizes, an index at the end) are written only when the muxer
  can seek back or assumes the start of a file.
- **What went wrong** — MP3 listeners joining mid-stream got an ID3v2 tag
  and a Xing frame; the fix strips them for two network outputs only, so
  the SRT and pipe outputs still send them. MP4 to any byte stream writes
  nothing, forever, with one log line (checked in pass 1). Matroska to a
  stream has unknown sizes and no cues. Pushing video in a container to a
  source client failed outright when first tried.
- **Why it is hard** — Seekability is a property of the destination, not of
  the format, and nothing in the format value states it; the muxer silently
  produces a different file, or nothing.
- **How to tell** — For each container, drive it to a file, to a byte
  stream and to an appending file: either a decodable result or an error
  naming the container's need, at start, not a silent output. For each
  network output, the MP3 body of a late consumer carries no ID3v2 and no
  Info frame.
- **Count and traces** — 4 in history: 39a3914a7, #1394, harbor K1 (ID3
  mid-stream), #4785; findings ffmpeg F1, A1, A2, net-callers A1.
- **Reconciliation** — Covered:
  [ffmpeg.md §13.1, §13.6](ffmpeg.md#131-the-three-ways-a-container-reaches-its-destination).
  Not covered: a format needing a seekable destination is not rejected
  (F1, G1); the mp3 preamble rule is applied by two outputs only (A1).

### KN-17. Who owns the I/O: the output or the encoder

- **Situation** — Either the encoder returns bytes and the output writes
  them, or the encoder (FFmpeg) opens the destination itself from a URL and
  the output never sees a byte. The output's own file semantics
  (permissions, directory creation, append, name interpolation, reopen)
  then depend on what the encoder does with the URL.
- **What went wrong** — When file output was delegated to the encoder,
  `perms`, `dir_perms` and `append` were ignored and directories were not
  created. Today a container restart into a file drops protocol options it
  consumed before and fails mid-stream; a plain path and a `file:` path
  reach the same file with different append and chaining behaviour, and a
  plain path with no truncation overwrites from offset 0 leaving the old
  tail. A URL output gives the script no view of the connection.
- **Why it is hard** — Two layers can each open the destination; every
  property implemented in one is silently absent in the other, and a URL
  string mixes "where" with "how".
- **How to tell** — Run the same file-output scenario (new dir, perms,
  append to an existing longer file, reopen, Ogg metadata change) once
  with bytes returned and once with encoder-owned I/O: the resulting files
  and modes are identical, and the same for `file:` and plain paths.
- **Count and traces** — 2 in history: a71822bf0 (#2069, #2072), #4672;
  findings ffmpeg F2, F4.
- **Reconciliation** — Covered:
  [ffmpeg.md §13.2, §13.3](ffmpeg.md#132-what-each-caller-does-with-the-target).
  Not covered: no rule that the caller's file semantics hold whichever side
  opens the destination.

### KN-18. A header captured from a muxer is only what the muxer wrote in time

- **Situation** — Consumers that join after the start (shared listeners,
  SRT peers) are given the encoder's header. For an encoder that owns a
  muxer, the header is whatever bytes the muxer wrote before its first
  packet; some write late, and codecs with an encoder delay create their
  first packet late.
- **What went wrong** — The header was fixed at start and went stale, then
  read through a callback; a late Ogg/Opus listener got no `OpusHead`
  because the captured header was empty; a shared Matroska/AAC (or MP3)
  mount gives late listeners no container header at all (checked in pass
  1).
- **Why it is hard** — "Header" is a muxer-internal notion with no
  boundary signal; frequent in-band restarts in tests re-send headers and
  hide the missing capture.
- **How to tell** — For each container × codec (including delayed codecs),
  a consumer that joins after 1 s with no metadata change decodes from its
  first byte.
- **Count and traces** — 3: c6f8c699a (#3265, #3276), 33643ff90 (#5542);
  finding ffmpeg V1. Joining logic itself:
  [harbor K1](../../outputs/spec/harbor/known-complexity.md#k1-a-late-listener-must-join-at-a-point-a-decoder-can-start-from).
- **Reconciliation** — Not covered:
  [ffmpeg.md §6](ffmpeg.md#6-header) states the requirement on the muxer
  as unchecked; V1 shows it fails for delayed codecs.

### KN-19. The final bytes at a disconnection

- **Situation** — Stopping an encoder returns its last bytes (buffered
  audio, end-of-stream pages, trailer). A network output stops its encoder
  at every disconnection: a failure, or a clean stop.
- **What went wrong** — The source client throws the final bytes away on
  every disconnection, clean stops included ("not worth the trouble"):
  every Ogg body ends without an end-of-stream page and the last second is
  lost (checked in pass 1). SRT had the same defect and fixed it.
- **Why it is hard** — On a failure the bytes cannot be sent, so dropping
  them looks right, and the clean case inherits the shortcut; players
  tolerate a missing end page.
- **How to tell** — Stop the output cleanly while connected: the server's
  last bytes are the encoder's final bytes (an end-of-stream page for Ogg),
  and the audio received equals the audio produced.
- **Count and traces** — 2: finding net-callers D5; SRT #4395 (SRT KC6).
- **Reconciliation** — Contradicts:
  [output-icecast.md §4.4, §5](output-icecast.md#44-what-a-server-and-its-listeners-receive)
  describe discarding on every disconnection; the draining rule, harbor and
  SRT deliver final bytes on a clean end.

### KN-20. Checks that pass without checking the source side

- **Situation** — Source-client tests run a real or embedded server and
  look at what that server accepts.
- **What went wrong** — The no-restart test is skipped as too flaky; the
  Ogg/FLAC test passes as soon as the input starts; TLS tests had to be
  stabilised; nothing records the bytes the source sends, so a byte-range
  bug in the non-chunked writer stays latent only because every encoder
  today returns whole buffers.
- **Why it is hard** — Real servers are tolerant, resynchronise, and
  report nothing about the source's framing; a recording fake server has
  to be written for the purpose.
- **How to tell** — A recording fake server per dialect; every source-side
  test asserts the count of connections, the head bytes and the body bytes
  it recorded, and fails if it recorded none. Writer tests feed buffers
  with non-zero offsets.
- **Count and traces** — 3: `tests/streams/icecast_no_restart.liq`
  (`test.skip ()`), 6f76b71d7 (#5486); findings net-callers D1, G2. Same
  shape on SRT:
  [SRT KC20](../../optionals/srt/spec/known-complexity.md#kc20-tests-that-pass-without-proving-the-behaviour).
- **Reconciliation** — Covered:
  [output-icecast.md §8](output-icecast.md#8-tests) lists the gaps. Not
  covered: `icecast_no_restart.liq` and the TLS tests are not in that list.

---

### Open, not entered

- #5344 (MP3 source client on 2.4.x, HTTPS listeners of an Icecast 2.4.4
  server get EOF after the burst): unresolved, possibly server/TLS side.
