# Findings: network callers of the encoder seam

Kept out of the as-built text. "Checked" means something was run against
the binary at 02fa52985 (scratch experiments with a fake Icecast server,
`curl`, and `ffmpeg` as an SRT caller); "read only" means from the code.

## Defects

### D1. `cry` writes the wrong range when a fragment has a non-zero offset (latent)

Read only. `Cry.write_data ~offset ~length` (`src/modules/cry/cry.ml:318`)
sets `len = length` and starts at `offset`, so it writes bytes
`[offset, length)` instead of `[offset, offset + length)`: `length - offset`
bytes, and nothing at all when `offset ≥ length`. `output.icecast` passes
every fragment of the encoder's `Strings.t` with its own offset
(`icecast2.ml:580`), in the non-chunked mode. The chunked path
(`String.sub x offset length`) and the dump file are correct.

Latent today because every encoder reaching `output.icecast` returns views
with offset 0 (`Strings.Mutable.to_strings` copies). Scenario: any encoder
or seam change that returns a `Strings.sub`/`drop` view (as SRT and the HLS
segmenter already build) silently truncates or drops bytes on non-chunked
Icecast connections, while chunked connections and the dump file stay
correct.

### D2. `%ffmpeg` never announces its bitrate to Icecast

Checked. `info_of_encoder` asks `encoder.bitrate ()` of an instance that has
not encoded yet (`icecast2.ml:124`, called from `icecast_start` before any
encode); the FFmpeg instance creates its streams on the first frame, so the
answer is always none. `%ffmpeg(format="mp3", %audio(codec="libmp3lame",
b="128k"))` sends `ice-audio-info: channels=2;samplerate=44100`, while
`%mp3(bitrate=96)` sends `…;bitrate=96`. [ffmpeg io.md §12](../../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg)
says "bitrate is the encoder's reported bitrate divided by 1000 (when
known)": literally met, never effective. The interface comment
(`encoder.mli`, `[bitrate ()]`) says "streaming outputs announce it".

### D3. Icecast loses the first piece of every connection

Read only (no byte-level check). `Output.encoded#send_frame` calls `encode`
then `send`; `encode` returns empty while disconnected, and the connection is
established inside `send` (`icecast2.ml:535–597`). The piece during which a
(re)connection happens is never encoded: up to one frame (default 0.04 s) at
start-up and after every reconnection. Not stated anywhere; contradicts no
written rule but is a silent gap in a stream that is otherwise continuous
from the source's point of view.

### D4. Icecast retry delay is rounded by a whole-second clock

Checked (delay 3 s; reconnection ≈3.4 s after the failure log line).
`send` compares `Unix.time ()` (whole seconds) with a retry time computed
from `Unix.gettimeofday ()` (`icecast2.ml:532, 594`): the effective wait is
the delay plus up to one second; a `restart_in(0.5)` waits 0.5 to 1.5 s.

### D5. Icecast discards the encoder's final bytes on every disconnection, clean stops included

Checked. `icecast_stop` ignores what `stop ()` returns
(`icecast2.ml:669`, comment "not worth the trouble"). With `%vorbis`, the
body of a connection closed by shutdown ends on a non-EOS page, and the last
link's buffered audio is lost. The previous normative encoder.md §2 (HEAD)
says stop "drains it and returns its final bytes"; harbor and SRT deliver
them on a clean end ([harbor modes.md §11](../../../outputs/spec/harbor/modes.md),
[srt output.md §6](../../../optionals/srt/spec/output.md#6-stopping)),
Icecast does not even on a clean stop. Asymmetry and defect against the
draining rule.

## Asymmetries

### A1. Streaming-format step: Icecast and harbor yes, SRT and url no

Checked. `Icecast_utils.streaming_format` removes ID3v2 and Xing for
`%ffmpeg` mp3 only in `output.icecast` and `output.harbor`. `output.srt`
with the same format sends a 44-byte ID3v2.4 tag (`TSSE Lavf…`) at the start
of the first peer's stream; a late SRT peer gets none (it starts at an MPEG
frame). The previous normative encoder.md §3 (HEAD) put "no ID3v2 tag or Xing
frame in the middle of a live stream" on the encoder; as built it is a
caller-side rewrite of the format, done by two of the four stream callers.

### A2. Where in-stream metadata goes

Icecast with ICY (`audio/mpeg`, `audio/aac`, `audio/flac`, `audio/wav`):
out of band only, the encoder is never told (`icecast2.ml:545`), so an
encoder that could carry it in-band (FLAC? `%ffmpeg` mp3 ID3?) never does.
Icecast without ICY, harbor, SRT, url: `encode_metadata`. Harbor carries ICY
itself and also calls `encode_metadata`. The decision "metadata in-band or
out of band" is therefore owned by the caller and keyed on the guessed
content type, not by the encoder.

### A3. Late-joiner support is three different mechanisms

- Harbor shared: `header()` + `restart_offset()`, with a stream-start join
  rule; harbor dedicated: `header()` of a fresh instance (relies on the
  first output not repeating it); SRT: `restart_offset()` else `header()`;
  Icecast: none needed (one instance per connection, `header()` never
  called); url and HLS: never.
- Shared harbor never uses `encode_into`, while
  [harbor contracts.md §4](../../../outputs/spec/harbor/contracts.md#4-encoder)
  states "Harbor uses `encode_into` when offered". Only dedicated mode does
  ([dedicated-encoder.md §1](../../../outputs/spec/harbor/dedicated-encoder.md)
  is right). Read only.

### A4. Stop exactly once: not everywhere

The previous normative encoder.md §2 (HEAD) ("An instance is stopped exactly
once"), also cited by [srt output.md §4](../../../optionals/srt/spec/output.md#4-encoder):

- HLS drops every instance without stop after a streaming error
  (`abort_run`, `hls_operator.ml:878`), deliberately
  ([hls operator.md §6](../../../outputs/hls/spec/operator.md#6-errors-and-reopening)).
  For packed audio the inner MP3/AAC instance is then never stopped either.
- `output.url` (read only, to verify): `encode_with` stops the instance on
  an exception, then `failed` tries to move the state out of `Connected`; a
  concurrent `stop` that moved it first also calls `stop()` on the same
  instance (`url_output.ml:168, 227–232`). The FFmpeg instance tolerates a
  second stop (`` `Stopped -> Strings.empty ``, `instance.ml:471`), so the
  effect is nil today.
- HLS creates and stops a **probe instance** per variant at evaluation
  (`hls_operator.ml:83–89`), not mentioned by
  [encoder-contract.md §2](../../../outputs/hls/spec/encoder-contract.md#2-calls-and-order)
  (which says instances are created "at the start of every run");
  [variants.md §2](../../../outputs/hls/spec/variants.md#2-building-a-variant)
  step 3 mentions the creation but not the stop. Packed audio's probe
  builds and stops a real LAME/FDK instance at evaluation.

### A5. Initial metadata

Icecast and SRT create instances with empty initial metadata (Icecast then
replays the last metadata through `encode_metadata`, which for Ogg begins
with an empty link restart, checked: no bytes come of it); harbor and url
pass the last/current metadata as initial metadata. An encoder that writes
initial metadata as a header tag (ID3v2 for `%mp3`) gets it in one family
and not the other.

## Gaps

### G1. Zero-length pieces never reach these callers' encoders

The previous normative encoder.md §2 (HEAD) states "Callers hand an instance
zero-length pieces as well as full ones". As built, the encoded-output base
skips position-0 frames and zero-length intervals (`output.ml:263, 352`), and
url and HLS skip them too. No network caller ever hands one. Whether
encoders still handle them is untested from this side.

### G2. Nothing tests the Icecast source side's encoder behaviour

No test checks the bytes an Icecast server receives: the per-connection
fresh header, the loss of the first piece (D3), the discarded final bytes
(D5), the out-of-band request, `ice-audio-info` (D2), the chunked framing.
Existing tests use `output.icecast` only as a stimulus for harbor or
`icecast.server`.

### G3. Icecast's out-of-band request carries every source header

Checked. The metadata GET reuses the source connection's header table:
`ice-name`, `ice-public`, `ice-audio-info`, `Authorization`, `User-Agent`
and any user `headers` (`cry.ml` `update_metadata` passes
`source.headers`). Nothing states whether this is intended; Icecast ignores
the extra headers (to verify).

### G4. `send_last_metadata_on_connect` errors are swallowed silently

`icecast2.ml:651`: `with _ -> ()`, no log, unlike every other metadata
failure path (logged severe). Read only.

### G5. `output.url` sends a metadata change that happened during the connect

`url_output.ml:177` compares, by physical identity, the current metadata
with what the instance was built with and calls `encode_metadata` before the
held frames. [io.md §11.2](../../../optionals/ffmpeg/spec/io.md#112-start-encode-stop)
does not mention it. Read only.

## To verify

- What Icecast 2 does with listeners when its source reconnects with a new
  stream (moves them to the new stream; for Ogg, whether a listener's
  player sees a chained link). From memory.
- That FFmpeg's mp3 muxer writes no Xing frame to a non-seekable custom I/O
  (so `write_xing = 0` matters only for seekable outputs) and that the SRT
  late joiner's missing ID3v2 tag is because the FFmpeg instance's
  `header()` is empty after its first output. Observed only from bytes.
- Shoutcast v1/v2 accept the ICY handshake as built (password line,
  `OK2`, `icy-caps:1`) — from memory; no server was run.
