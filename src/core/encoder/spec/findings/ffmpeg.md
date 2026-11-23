# Findings: `%ffmpeg` at the seam and its I/O

Kept out of [ffmpeg.md](../ffmpeg.md). **FE** is the FFmpeg tree's
[encoder.md](../../../optionals/ffmpeg/spec/encoder.md). Scripts and logs of
the checks are under the agent scratch directory
(`spec-encoding/ffmpeg/t*.liq`, `t*.log`); every check used the binary at
`02fa52985`.

## Defects

### F1. A header write that always fails is never escalated: the output streams nothing, forever

**Checked** (`t2`: `%ffmpeg(format="mp4", %audio(codec="aac"))` to
`output.external` for 6 s: 264 FFmpeg errors `muxer does not support non
seekable output`, one `Invalid argument` line at important level, empty
output, no error raised, output still "running"; `t12`: the same through
`output.file(append=true)`, the existing 11-byte file untouched).

- `instance.ml:269-280` writes the header from the packet hook; the error is
  raised out of `Av.write_frame` (`av.ml:579-596`) and caught by
  `encoded_stream.ml:75-79` (`guarded`) as an item fault.
- The run counter is reset by `Faults.delivered` on every guarded call that
  did not fail, including pieces that produce no packet (AAC: 1024-sample
  frames, 882-sample internal frames), so `max_consecutive_faults` (100) is
  never reached.
- Contradicts FE §5.2 ("the header write fails → output fault, instance
  `Failed`") and FE §11.
- Scenario: any `Stream` caller (harbor, icecast, srt, external) or
  `output.file(append=true)` with MP4/MOV not fragmented.

### F2. A chaining restart into a file drops protocol options it consumed before, and fails mid-stream

**Checked** (`t19`: `output.file(%ffmpeg(format="ogg", blocksize=4096,
%audio(codec="libvorbis")), …)` with a metadata change after 1 s: `Unrecognized
options: container: blocksize=4096` raised from `encode_metadata`, the output
fails and the default `reopen_on_error` re-raises, stopping the script; the
file holds one link).

- First container: `Av.open_output` → `avio_open2` consumes the file protocol
  options (`av_stubs.c:1497-1503`). Restart: `restart` opens with
  `~append:true` (`instance.ml:414-418`) → appending custom I/O
  (`:141-149`) → protocol options left over → `Opts.unused_message`
  (`:169-174`) raises a configuration error.
- A configuration error after the output has streamed contradicts FE §4
  ("reported when the output starts").

### F3. An unknown muxer name is silently replaced by the file extension's muxer for a URL target

**Checked** (`t17`: `output.file(%ffmpeg(format="nosuchmuxer",
%audio(codec="libmp3lame")), "g_x.mp3", s)` writes a valid MP3, no message).

- `guess ()` (`instance.ml:126-130`) passes both the short name and the URL;
  the result is `None` or the extension's muxer; for a `Url` target either
  path opens (`:163-164`). The same format in a `Stream` target fails with
  `No ffmpeg format could be found for format="nosuchmuxer"` (`:153-156`).
- Contradicts FE §3 step 3.2–3.4 (a named format that is not found is an
  error) and the principle that a configuration error is not silently
  fixed.

### F4. `file:` prefix versus a plain path: same file protocol, different behaviour

**Checked** (`t12`, `t15`, `t16`).

- `output.url(url="/abs/x.ogg", %ffmpeg(format="ogg", …))`: metadata changes
  are dropped (1 link) while `url="file:/abs/x.ogg"` chains (3 links).
- `output.url(url="/abs/x.mp3", %ffmpeg(…, truncate=0, …))` overwrites from
  offset 0 without truncating: a 100000-byte file keeps its stale tail
  (corrupt result); `url="file:/abs/x.mp3"` appends (125121 bytes).
- Traces: `appended_path` (`instance.ml:97-104`) and `chains` (`:379-384`)
  test the 5-character prefix; FFmpeg's file protocol accepts both forms.

### F5. Only the muxer named `ogg` chains; `oga`, `ogv`, `opus`, `spx` drop metadata

**Checked** (`t22`: `format="opus"` to `output.file` with a metadata change:
one `OpusHead`, the new title absent; `format="ogg"` with libopus: two
`OpusHead`, title present).

- `chains` compares `muxer = "ogg"` (`instance.ml:380`). FE §13.1 states the
  rule for "Ogg"; the code applies it to one of FFmpeg's five Ogg muxers.

### F6. Leftover options are not reported in one error

**Checked** (`t20`: `%ffmpeg(format="adts", foo=3, %audio(codec="aac",
bar="x"))` → `Unrecognized options: container: foo=3`; `bar` is not named).

- The container check raises in `open_container` (`instance.ml:169-174`)
  before any stream is built; stream leftovers are checked separately
  (`encoded_stream.ml:53`). Contradicts FE §4 and its conformance item 4.1.

### F7. FE §3 step 3.4 describes a seek callback that does not exist

**Read only** (and `git log -S seek` on `instance.ml`: no version since the
rewrite `4bf0f1d12` passes `?seek`).

- FE: "A seek callback over that buffer is given only for HLS with
  `format="mp4"`." Code: both `open_output_stream` calls
  (`instance.ml:146`, `:162`) pass no `?seek`; every `Stream` and appending
  container is non-seekable. HLS MP4 works because `hls_defaults` forces
  `+frag_custom+dash+delay_moov`.

### F8. FE §1's factory signature and §13.1's `output.url` row do not match the code

**Read only** for the signature; **checked** for the chaining part (`t12`).

- FE §1: `factory : ?hls:bool -> ?interrupt:… -> pos -> name -> metadata`.
  Code: the registered factory takes `(pos, name, metadata)` only
  (`instance.ml:793-796`); HLS is a separate plug-in (`:797-802`); the
  interrupt reaches only instances `output.url` builds itself, outside the
  registry (`url_output.ml:85-87`).
- FE §13.1: "any muxer of `output.url`: a reconnect". Code: an Ogg
  `output.url` to a `file:` URL chains in place; to any other URL a metadata
  change is dropped (no reconnect); only an incompatible copy switch fails the
  instance, and `output.url`'s catch-all (`url_output.ml:162-169`) turns that
  into a reconnect.

### F9. Exceptions other than FFmpeg errors leave the instance in `Started`

**Read only.**

- `encode_frame` (`instance.ml:344-365`) sets `Failed` only for
  `Copy_encoder.Incompatible` and `Avutil.Error`. `Failure "…: too many
consecutive faults"` (the escalation FE §5.2 calls an output fault) and
  `Failure "Timestamp out of range"` propagate with the state unchanged.
  The next `encode` continues on the same container; `stop` drains it.
- A failed restart inside `encode_metadata` (`:465`, `restart` `:414-423`)
  leaves `t.current` pointing at the closed container with state `Started`;
  the next `encode` hits a closed FFmpeg container.
- Contradicts FE §1's state table (output fault → `Failed`).

## Asymmetries

### A1. The same format gives different bytes, or none, by destination

**Checked** (`t1`, `t12`, `t18`): MP4 moov at the end through `output.file`
and `output.url` to a path, refused through every `Stream` caller and through
`output.file(append=true)`; MP3 Info frame only through a seekable file;
Matroska sizes, Cues and duration only through a seekable file. Nothing in
the format, the seam or the output tells the user before the first packet
(see F1).

### A2. mp3 stream tags: harbor and icecast strip ID3v2, SRT and external do not

**Checked** (`t1`, `t3`): `output.external` and (by code) `output.srt` send
the ID3v2.4 tag (`ID3\x04`, 44 bytes, `TSSE Lavf63.1.102`); `output.harbor`
starts at the first audio frame. `output.srt` sends `header()`, which for MP3
is that tag, to every joining peer (`output_operator.ml:297-313`). The Xing
half of the rewrite is redundant: FFmpeg writes no Info frame to a
non-seekable output (`mp3_ext.mp3`: no `Info`/`Xing`). The previous seam text
(git `HEAD:src/core/encoder/spec/encoder.md` §3) required "no ID3v2 tag or
Xing frame in the middle of a live stream" of every stream encoder; the rule
lives in one output utility, applied to two of the four `Stream` callers that
serve live peers.

### A3. Flush granularity differs by destination

**Checked** (`t21`): `Stream` outputs receive bytes per packet (FFmpeg flushes
non-seekable I/O after each packet); `output.file` stays at 252 bytes for the
first 2 s of 128 kb/s MP3 (32 KiB I/O blocks). `output.file`'s `flush=true`
has no effect on `%ffmpeg` (its channel is a no-op).

### A4. Interrupts cover only `output.url`'s first container

**Read only.** The registered factory has no interrupt, so `output.file`'s
URL container and every appending container (including the second link of an
`output.url` Ogg on `file:`) write with no interrupt: a blocking write there
(NFS, FUSE, a full pipe-backed path) cannot be aborted by stop or shutdown.
io.md §3's table says `output.url` writes are aborted by the interrupt
"for the connection's lifetime".

### A5. Configuration errors of pipe outputs appear at the first non-empty piece

**Read only.** `output.file` and `output.external` build the instance lazily
(`pipe_output.ml:67-79`): leftover options, unknown muxers in a `Stream`
target and a failed open surface at the first piece of positive length, not
at output start. A fallible source that never becomes ready never reports
them. FE §4 states "reported when the output starts". The evaluation-time
check (`lang_encoder.ml:68`) only looks the factory up; it builds nothing.

## Gaps

### G1. No statement or check of destination seekability

Nothing states which formats need a seekable destination, and nothing
rejects one before its first packet (F1, A1). The seam's
[encoder.md §7](../encoder.md#7-where-the-bytes-go-what-the-seam-assumes)
says the instance "writes, seeks and closes that destination itself"; the
instance never seeks, FFmpeg does, and only on URL containers.

### G2. `header()` relies on muxers writing their header inside the header write

`on_packet` captures the bytes produced by `avformat_write_header` + flush
(`instance.ml:274-279`). A muxer that writes more header bytes at its first
packet (or later) would give a short `header()`; nothing checks it. Not
observed directly; see V1.

### G3. Protocol options are valid for some containers of an instance and not others

The container options table is consumed by three layers for a URL container
and two for a `Stream` or appending one. Which keys are protocol options is
FFmpeg's; Liquidsoap neither lists nor routes them (F2).

### G4. `stop` from `Failed` drops buffered bytes

Bytes muxed during the failing call stay in the byte buffer and are dropped
by `stop` (`instance.ml:472-475`): for a `Stream` caller the last packets
before the failure are lost even when they were muxed successfully.

### G5. No test for appending, truncation, plain-path URLs, protocol options, or shared harbor containers with a header

See [ffmpeg.md §14](../ffmpeg.md#14-tests).

## To verify

### V1. Shared `output.harbor` with `%ffmpeg(format="matroska", %audio(codec="aac"))`: listeners never receive the EBML header

**Checked behaviour, cause not established.** `t4`, `t6`, `t10`, `t11`: a
listener joining at 0.1 s, at 1.5 s with `burst=0`, and at 1.5 s with
`burst=1000000` (`buffer=2000000`) receives a stream starting with a Cluster
(`1F 43 B6 75`), no `1A 45 DF A3` anywhere; FFprobe cannot read it. Same
format with `dedicated_encoder=true`, through `output.external`, or with
`libopus`, `flac` or `mp2` in Matroska on a shared mount: the stream starts
with the EBML header. The encoder's byte stream starts with the header
(`output.external`), so the loss is either in harbor's shared store / join
logic or in `header()` for this codec; to be resolved by the harbor caller
(agent `net-callers`). `tests/harbor/` has no shared-mount `%ffmpeg` test with
a container header (only dedicated, `harbor_output_dedicated_chunks.liq`).
Also observed: a shared fMP4 mount (`+frag_keyframe+empty_moov`, audio only)
gave a listener 0 bytes in 2 s, consistent with fragments emitted only at
`stop` (V4) but also with the header not being sent.

### V2. FFmpeg flushes non-seekable I/O after every packet

From memory of `libavformat/mux.c` (`flush_packets` default −1: flush when
the I/O is not seekable). Consistent with `t21`.

### V3. FFmpeg's mp3 muxer writes the Info/Xing frame only on a seekable output

From memory of `libavformat/mp3enc.c`. Consistent with `t1`.

### V4. Audio-only fragmented MP4 with `frag_keyframe` cuts no fragment before the trailer

Observed (`t5`: 732 bytes at 3 s, one `moof` at `stop`; with
`frag_duration=1000000`, 33909 bytes at 3 s). The FFmpeg reason (fragments cut
at video key frames) is from memory.
