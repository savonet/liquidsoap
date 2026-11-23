# The `%ffmpeg` encoder at the seam (as-built)

How the `%ffmpeg` encoder answers the generic seam ([encoder.md](encoder.md)),
and where its bytes go. It owns: the two ways a `%ffmpeg` instance reaches its
destination (bytes returned, or a URL it writes itself), the rules that pick
one, the special cases every output applies to a `%ffmpeg` format, and the
consequences of each destination on the bytes (§13).

It relies on, and does not re-describe:

- the `%ffmpeg(...)` syntax, typing, extension and MIME:
  [encoder-format.md](../../optionals/ffmpeg/spec/encoder-format.md);
- the instance lifecycle, option checking, stream creation, draining, metadata
  and container restarts: [ffmpeg encoder.md](../../optionals/ffmpeg/spec/encoder.md)
  (cited below as **FE §n**);
- stream encoders: [encoder-internal.md](../../optionals/ffmpeg/spec/encoder-internal.md),
  [encoder-copy.md](../../optionals/ffmpeg/spec/encoder-copy.md); timestamps:
  [timestamps.md](../../optionals/ffmpeg/spec/timestamps.md);
- `output.url`, interrupts and the other outputs' `%ffmpeg` special cases:
  [io.md](../../optionals/ffmpeg/spec/io.md) §3, §11, §12;
- the container operations of the bindings:
  [avformat.md](../../../modules/synced/ffmpeg/spec/avformat.md);
- the HLS face: [encoder-ffmpeg.md](../../outputs/hls/spec/encoder-ffmpeg.md).

Where the code departs from those documents, the departure is listed in
[findings/ffmpeg.md](findings/ffmpeg.md), not here. Part B (OCaml, bindings,
file:line traces): [language-notes/ffmpeg.md](language-notes/ffmpeg.md).

## Vocabulary

- **Target**: the field `output` of a `%ffmpeg` format value: `Stream` or
  `Url u`. The script never writes it: parsing `%ffmpeg(...)` always yields
  `Stream`. Only outputs rewrite it (§13.2).
- **Byte buffer**: the instance's own growable byte buffer. In `Stream`
  target, the container writes into it; `encode`, `encode_into` and `stop`
  hand its content over and empty it.
- **Container**: one open FFmpeg muxer context with its I/O. An instance has
  one current container; a restart (FE §13) closes it and opens another.
- **Appending container**: a container whose I/O is a file descriptor the
  encoder opened itself with `O_WRONLY|O_APPEND|O_CREAT` (§13.3).

## 1. Format and options

- Syntax, every parameter, validation and typing: encoder-format.md §1–§10,
  §13. Two top-level names are interpreted by Liquidsoap: `format` (`"none"`
  means none) and `interleaved` (`true`, `false`, `"default"`); `metadata` is a
  list of string pairs. Every other labelled scalar becomes a **container
  option**, consumed by FFmpeg at open (§2).
- `output` is not a parameter. The format value starts with target `Stream`.
- Extension and MIME: encoder-format.md §12. The seam's `extension(format)`
  raises "not found" when the format has none; `mime(format)` always answers.
- Format-level bitrate estimate (`bitrate(format)` of the seam): not answered
  (raises "not found"). The instance's `bitrate()` (§8) is the only rate.
- `iso_base_file_media_file_format`: not answered (raises "not found").

## 2. Factory and instance creation

Two plug-ins are registered under the name `ffmpeg`:

| Registry | Accepts                                                                                                              | Factory arguments               |
| -------- | -------------------------------------------------------------------------------------------------------------------- | ------------------------------- |
| stream   | every `%ffmpeg` format                                                                                               | `(pos, name, initial metadata)` |
| HLS      | `%ffmpeg` formats whose `format` is `mpegts`, `webvtt`, `adts`, `mp3`, `ac3`, `eac3`, `mp4`, `mov`, `ipod` or `ismv` | `(pos, segment_duration, name)` |

A third entry point exists outside the registries: `output.url` builds the
instance directly from the format (target rewritten to `Url url`) with an
**interrupt** function, and wraps it with the seam's `protect` itself
([io.md §11.2](../../optionals/ffmpeg/spec/io.md#112-start-encode-stop)).
The registered stream factory never receives an interrupt: every other caller
gets an instance without one.

Creating a stream instance, in order:

1. Log name `ffmpeg.encoder.<name>`. A byte buffer of initial capacity 4096.
2. **Open the first container** (FE §3 step 3), for the target as the caller
   left it in the format (§13.2):
   1. Container options: the format's options, with `sample_fmt` and
      `channel_layout` names converted (FE §3 step 3.1). When the container is
      appending (§13.3), the key `truncate` is removed first.
   2. Interleaving: as written, else on when the format has more than one
      non-drop track.
   3. Muxer guess: from `format` (short name), the URL as file name (`Url`
      only) and `mime_type`.
   4. Open, by target (§13.1 has the table):
      - appending: open the descriptor first; no muxer guessed is
        `No ffmpeg format could be found for this file`;
      - `Stream`: no `format` is `Format is required!`; a `format` that names
        no muxer is `No ffmpeg format could be found for format="<f>"`;
      - `Url u`, not appending: FFmpeg opens `u`, with the guessed muxer if
        any (else FFmpeg guesses from `u` alone) and the interrupt if any.
        Each of the three messages is an `encoder`-kind error at the format's
        position. A failure of FFmpeg's open raises FFmpeg's error.
   5. Leftover container options (none consumed by the muxer, its private
      options or, for a `Url` opened by FFmpeg, the I/O protocol): the
      container is closed, the descriptor closed, and
      `Unrecognized options: container: k=v; …` is raised (kind `encoder`).
      Stream options are checked separately, later (encoder-internal.md).
   6. One stream encoder per non-drop track, in track order; no muxer stream
      yet (FE §5). Any failure closes the container and the descriptor and
      re-raises.
3. State `Built`, media position 0, header `Pending at 0`, no link start.
4. `encode_metadata(initial metadata)` (§5): before any packet, it sets the
   container metadata.

Every failure raises out of the factory; nothing is logged by the factory
itself. The caller decides when the factory runs (§13.2): this is when a
`Url` destination is opened, truncated or created.

The HLS factory (§12) builds the same instance with target forced to
`Stream` and the HLS defaults of FE §3 step 2.

## 3. `encode`

- Input, stream creation, per-track encoding, copy groups and restarts: FE §5,
  §13. A piece of length 0 can create the muxer streams; it writes nothing
  and does not advance the media position.
- Every caller of the stream face skips pieces of length 0 before calling the
  instance (pipe outputs, `output.url`); the instance does not rely on it.
- State checks: `Failed e` re-raises `e`; `Stopped` raises
  `Failure "Encoder used after stop"`.
- Output: the byte buffer's whole content, then the buffer is emptied.
  - `Stream` target: every byte the muxer wrote during the call, including
    bytes of earlier calls that were still in FFmpeg's I/O buffer and were
    flushed now (§13.4), and bytes produced by an `encode_metadata` restart
    since the last call.
  - `Url` target and appending containers: always empty. The muxer wrote to
    the destination during the call.
- Container header: written by the encoder itself just before the first
  packet reaches the muxer (FE §5), followed by a flush of FFmpeg's I/O, so
  that the header bytes are in the byte buffer before any packet byte (§6).

## 4. `encode_into`

Always present. Runs `encode`'s work, then calls the consumer once with a view
on the byte buffer (length and a copy function); the buffer is emptied after
the consumer returns or raises. No copy is made by the encoder. Same content
as `encode` would have returned.

## 5. `encode_metadata`

FE §6. As it touches the seam:

- The metadata list is replaced by the new entries plus the format's own
  `metadata` entries whose keys the new ones do not set.
- In `Failed` or `Stopped`: nothing else.
- Before the current container's header is written: set on the container.
- After: when the instance **chains** (§13.5), the container is restarted at
  once (Ogg link); its bytes stay in the byte buffer, or go to the
  destination, and come out of the next `encode` or `stop`. Otherwise:
  logged at debug level (`Metadata update not carried by this container`).
- Failures of the restart propagate (the state is not changed).

## 6. `header`

- `Pending`: empty bytes. `Captured h`: `h`.
- Captured once per container, at its first packet: the bytes the byte buffer
  received from the container's start offset up to the moment the header was
  written and flushed. For the first container that offset is 0 (or 0 again
  after each hand-over that happened before the first packet); for a restart
  it is the buffer length when the new container was opened.
- A restart sets `Pending` again, so `header()` returns the previous
  container's header until the new one's first packet, then the new link's.
- `Url` target, appending containers, HLS: the captured value is empty bytes
  (nothing reaches the byte buffer; HLS records an empty capture).
- Requirement on the muxer (not checked): every header byte is written by
  FFmpeg's header write. Bytes a muxer writes later (at its first packet)
  are not in `header()`.

## 7. `restart_offset`

- The offset, in the bytes the latest `encode`, `encode_into` or `stop`
  returned, where the latest restart's container begins; none when no
  restart happened since the previous hand-over.
- Two restarts between two hand-overs report the later one only.
- Only chaining restarts (§13.5) and restarts at a copy source switch on a
  chaining target set it. HLS restarts do not (that face has its own pieces).
- `Url` targets: a restart may happen (Ogg on `file:`), but the returned bytes
  are empty, so the offset is 0 in an empty output.

## 8. `bitrate`

Per non-drop stream: a copy stream's bitrate from its parameters, an encoded
stream's from its codec once its muxer stream exists. The sum when every
stream answers; otherwise none (always none before the streams are created).
Read under the instance lock; never fails.

## 9. `stop`

- Clears the reported restart offset first.
- `Stopped`: returns empty bytes.
- `Failed`: skips the stream encoders' drain and calls the container close
  directly (errors ignored; per the bindings, that close still flushes the
  codecs and writes the trailer unless FFmpeg's container is itself in error);
  state `Stopped`; returns empty bytes. Bytes still in the byte buffer are
  dropped (no one reads that buffer again).
- `Built` or `Started`: drains every stream encoder and the copy tracks (only
  if streams exist), then closes the container: FFmpeg flushes the codecs,
  writes the header if no packet did, writes the trailer, flushes its I/O and
  releases it; the appending descriptor, if any, is then closed. A drain error
  and a close error are each logged at severe level (`Draining failed: …`,
  `Closing failed: …`) and do not raise. State `Stopped`; returns the byte
  buffer's content (empty for `Url` and appending containers).
- A container that never received a packet and has only copy streams, or
  none, is closed with no header and no trailer (avformat.md).

## 10. Bytes produced

The encoder adds no byte of its own to the container: every byte is the
muxer's. What the encoder decides is **when** bytes move (§3), **what is
sliced out** of them (`header`, `restart_offset`, HLS init segments) and
**which destination properties the muxer sees** (§13.6), which changes the
container layout. Worked examples, from the binary at `02fa52985`
(`sine()`, 44100 Hz stereo, FFmpeg libavformat 63.1.102):

**MP3** `%ffmpeg(format="mp3", %audio(codec="libmp3lame"))`, empty metadata:

```
output.file (Url file:, seekable)
00000000: 4944 3304 0000 0000 0022 5453 5345 0000  ID3......"TSSE..
00000010: 000e 0000 034c 6176 6636 332e 312e 3130  .....Lavf63.1.10
00000020: 3200 0000 0000 0000 0000 0000 fffb 5000  2.............P.
          ^ ID3v2.4, 10 + 0x22 = 44 bytes      ^ offset 44: LAME/Info frame (CBR "Xing")
output.external (Stream, not seekable)
00000020: 3200 0000 0000 0000 0000 0000 fffb 9064  ... first audio frame at 44, no Info frame
output.harbor (Stream, format rewritten with id3v2_version=0, write_xing=0)
00000000: fffb 9264 3889 f27b 0ac7 4f7d 2000 0000  first audio frame at 0
```

The file's first 252 bytes (ID3 + Info frame) are on disk after the header
write; nothing more reaches the file until FFmpeg's 32 KiB I/O buffer fills
(§13.4); the Info frame is rewritten at `stop` by seeking back.

**MP4** `%ffmpeg(format="mp4", %audio(codec="aac"))`, top-level boxes:

| Destination                                                           | Boxes                                                                          |
| --------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| `output.file`, `output.url "<path>.mp4"`                              | `ftyp`(28) `free`(8) `mdat` `moov` (at the end)                                |
| `output.file`, `movflags="+faststart"`                                | `ftyp` `moov` `free` `mdat`                                                    |
| `output.file`, `movflags="+frag_keyframe+empty_moov"`                 | `ftyp` `moov` `moof` `mdat` `mfra`                                             |
| `Stream` (harbor, external, icecast, srt), `output.file(append=true)` | nothing: the header write fails (§11)                                          |
| `Stream` with `+frag_keyframe+empty_moov`, audio only                 | `ftyp`(32) `moov`(700) at once; the single `moof`/`mdat`/`mfra` only at `stop` |

**Matroska** `%ffmpeg(format="matroska", %audio(codec="aac"))`: to a file,
the Segment size is patched (`…18 53 80 67 01 00 00 00 …`), `Cues` are
written and the duration is known (2.043 s for 2 s of input); to a `Stream`,
the Segment size stays unknown (`18 53 80 67 01 ff ff ff ff ff ff ff`), no
`Cues`, duration unknown.

**Ogg chain** `%ffmpeg(format="ogg", %audio(codec="libvorbis"))` with a
metadata change every second for 3 s: three links (three `\x01vorbis`
identification headers, titles `t1`, `t2`, `t3`) in one file through
`output.file` and through `output.url(url="file:…")`; one link and only `t1`
through `output.url(url="/abs/path.ogg")` (§13.5).

## 11. Concurrency and failures

- Every caller holds the seam's per-instance lock (`protect`) around every
  operation; `output.url` wraps its directly built instance the same way.
  FFmpeg containers reject concurrent use of one container with an
  "Object in use!" error at once rather than waiting; the lock is what keeps
  that from happening.
- Threads per caller: `output.file`/`output.external`/`output.harbor`/
  `output.icecast`/`output.srt`: the instance is built and driven from the
  streaming thread (pipe outputs build it at the first non-empty piece);
  `output.url`: built on a scheduler task (the open of the URL), driven from
  the streaming thread, stopped from whichever thread stops the output.
  FFmpeg calls release the OCaml runtime lock while they block.
- Classes of failure: FE §5.2 and §11. What reaches the caller at the seam:
  - from the factory: every build failure, raised;
  - from `encode`/`encode_into`: an FFmpeg error from the muxer or a
    container restart, an incompatible copy source (an unrecoverable error)
    and the copy keyframe wait expiring all set state `Failed` and raise; any
    other exception (an item-fault run past `max_consecutive_faults`, a
    timestamp out of range) raises **without** changing the state;
  - from `encode_metadata`: a failed restart, raised, state unchanged;
  - from `stop`: nothing (logged).
- A header write that fails (a muxer refusing a non-seekable destination) is
  an item fault of the stream whose packet triggered it, not an output
  fault: the packet is dropped, the next packet retries the header write, the
  failure is logged by the stream (`Invalid argument`, important level, then
  rate-limited to one report per 10 s) and FFmpeg logs its own reason at
  severe level for every attempt (`muxer does not support non seekable
output`). The run counter is reset by any piece of that stream whose
  encoding did not fail, which includes pieces that produce no packet, so
  for an audio codec with frames larger than one internal frame the run never
  reaches the bound: the output keeps running and writes nothing (checked:
  6 s of AAC to `output.external`, 264 refused header writes, empty output,
  no error raised).

## 12. Segmenting (HLS) face

Owned by [encoder-ffmpeg.md](../../outputs/hls/spec/encoder-ffmpeg.md) and
FE §8. At the seam:

- Same instance machinery, target forced to `Stream`, its byte buffer shared
  with the split bookkeeping; every byte goes there.
- Before each candidate cut, the muxer is flushed (and an MPEG-TS muxer told
  to resend its tables), so a cut is a byte offset in that buffer.
- The init segment of an ISO-family container is carved out of the buffer
  by walking top-level boxes until `styp`, `sidx` or `moof`; other muxers
  have an empty init segment.
- A restart opens the next container on the same buffer, with
  `+frag_discont` added to a written `movflags` for the ISO family.
- The HLS output builds one instance per variant while building the variant,
  calls `init` then `hls_stop` on it (a probe that opens and closes a
  container), and builds the instances it streams with separately (HLS tree).

## 13. Where the bytes go

### 13.1 The three ways a container reaches its destination

| Kind          | Chosen when                                                                                               | Who opens the destination                                                                                                       | I/O object                                                    | Seekable                                      | Interrupt                                     | Bytes returned by the instance |
| ------------- | --------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | --------------------------------------------- | --------------------------------------------- | ------------------------------ |
| **Stream**    | target `Stream`                                                                                           | nobody: the encoder writes into its byte buffer                                                                                 | custom write function into the byte buffer, 32 KiB I/O buffer | no (no seek function is ever given)           | none                                          | all                            |
| **URL**       | target `Url u`, not appending                                                                             | FFmpeg's protocol for `u` (`file:` or a plain path → its file protocol; `rtmp:`, `srt:`, `icecast:`, `tcp:`, … → that protocol) | FFmpeg-owned I/O                                              | as the protocol says (the file protocol: yes) | the factory's, when given (`output.url` only) | none                           |
| **Appending** | target `Url u` with `u` starting with `file:` **and** (a restart, or container option `truncate` = int 0) | the encoder: `open(<u without "file:">, O_WRONLY\|O_APPEND\|O_CREAT, 0644)`                                                     | custom write function `write(fd, …)`, 32 KiB I/O buffer       | no                                            | none                                          | none                           |

Consequences shared by every row:

- Only FFmpeg ever seeks, and only on a URL container whose protocol is
  seekable. The encoder never seeks, never reads back, never tells the muxer
  anything about the destination beyond the I/O object.
- The muxer adapts to seekability by itself: MP4/MOV refuse a non-seekable
  destination unless fragmented; MP3 writes its Info frame only when
  seekable; Matroska leaves sizes unknown and writes no Cues; WAV leaves its
  sizes at `0xFFFFFFFF` ([output-file.md §4.3](output-file.md#43-ffmpeg-file-variant)).
  The same format value therefore gives different bytes, or no bytes, by
  destination (§10).
- The trailer is always written by the encoder's `stop` (container close),
  never by the output. A destination that was opened by FFmpeg is also closed
  by FFmpeg at that moment; an appending descriptor is closed by the encoder
  right after.
- Muxers that write files of their own (FFmpeg's `dash`, `hls`, `segment`,
  `tee`, `image2`) open them through FFmpeg next to the URL; the encoder and
  the output know only the URL.
- A muxer that needs no file (FFmpeg's `AVFMT_NOFILE` muxers, devices) in a
  `Stream` target fails at open ("the format needs no file"); in a `Url`
  target FFmpeg opens no I/O and the URL is only the muxer's argument.
- The open of a URL destination happens inside the factory: when the caller
  calls the factory is when the destination is created, truncated or
  connected (§13.2).

### 13.2 What each caller does with the target

| Caller                                | Target it gives                                                                 | Container options it adds                                                                           | Opens/creates                                                                                                                              | When the factory runs                                                      | What it does with returned bytes                       |
| ------------------------------------- | ------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------- | ------------------------------------------------------ |
| `output.file`                         | `Url "file:" ^ <current filename>` (the interpolated name, unchanged otherwise) | `truncate` = int 0 if `append`, else 1, replacing any script value                                  | creates the file (`O_WRONLY\|O_CREAT`, mode `perm`, no truncation; directories with `dir_perm`) and closes it at once, at each open/reopen | at the first non-empty piece after each open/reopen; one instance per file | ignores them (its write and flush are no-ops)          |
| `output.url`                          | `Url url`, the script's string verbatim                                         | none                                                                                                | nothing: FFmpeg opens `url`                                                                                                                | on a scheduler task per connection attempt; one instance per connection    | ignores them                                           |
| `output.external`                     | `Stream` (format unchanged)                                                     | none                                                                                                | the process                                                                                                                                | first non-empty piece after each (re)open                                  | writes them to the process's stdin                     |
| `output.harbor`                       | `Stream`                                                                        | for `format="mp3"`: `id3v2_version` = int 0 and `write_xing` = int 0, each unless the script set it | —                                                                                                                                          | caller's ([output-harbor.md](output-harbor.md))                            | stores and sends, slicing at `header`/`restart_offset` |
| `output.icecast`                      | `Stream`                                                                        | same mp3 rewrite as harbor                                                                          | —                                                                                                                                          | per connection                                                             | sends                                                  |
| `output.srt`                          | `Stream`                                                                        | none (no mp3 rewrite)                                                                               | —                                                                                                                                          | caller's ([output-srt.md](output-srt.md))                                  | sends; a joining peer gets `header()` first            |
| HLS                                   | `Stream`, forced                                                                | FE §3 step 2                                                                                        | —                                                                                                                                          | per variant                                                                | split pieces                                           |
| `input.harbor.dynamic` `copy_encoder` | builds a `%ffmpeg` value with target `Stream` for the script                    | the script's `options`                                                                              | —                                                                                                                                          | not a caller: it only makes a format value                                 | —                                                      |

The mp3 rewrite and the MIME rules: [io.md §12](../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg).
Rewritten values are copies: the script's format value is never mutated.

### 13.3 Append and truncate, exactly

The decision is the encoder's, from the target string and one option:

```
appending(container) =
  target is Url u
  and u starts with the 5 characters "file:"
  and (this container is a restart  or  option "truncate" = int 0)
```

- `output.file(append=true)`: `truncate` = 0 → the first container already
  appends. The descriptor is opened by the encoder (mode `0644`, unused: the
  output created the file). The container is non-seekable (§13.1), so the
  file gets the format's stream form (MP4 fails, MP3 has no Info frame,
  WAV sizes `0xFFFFFFFF`). Checked: an 11-byte file `PREEXISTING` followed by
  an Ogg stream of 20632 bytes gives 20643 bytes starting with `PREEXISTING`.
- `output.file(append=false)`: `truncate` = 1 → FFmpeg's file protocol
  opens with truncation. Checked: a longer existing file is replaced.
- Any container option other than `truncate` that only the file protocol
  consumes (`blocksize`, `pkt_size`, `seekable`, `follow`) is consumed by a
  URL container and left over by an appending one: an appending container
  with such an option fails to open with `Unrecognized options` (§13.5).
- `output.url(url="file:<p>")` with `truncate=0` in the format: appends, as
  above.
- `output.url(url="<p>")` (no `file:` prefix) with `truncate=0`: FFmpeg's
  file protocol opens `<p>` without truncation and writes from offset 0, so
  a longer existing file keeps its tail. Checked: a 100000-byte file of `Z`
  stays 100000 bytes, MP3 at its start, `ZZZZZZZZ` at its end.
- In a `Stream` target, `truncate` (and every protocol option) is left over:
  `Unrecognized options: container: truncate=1` at instance creation.

### 13.4 When bytes reach the destination

- `Stream` and appending containers: FFmpeg flushes its I/O after every
  packet when the I/O is not seekable, so each `encode` returns (or writes)
  everything muxed during the call. Checked: `output.external(flush=true)`
  with 128 kb/s MP3 grows 7149, 15090, 22613, 32226 bytes at 0.5 s steps.
  Muxers that hold packets (interleaving, fragmenting MP4) delay their bytes
  regardless.
- URL containers on a seekable file: FFmpeg writes in blocks of its I/O
  buffer (32 KiB). Checked: the same MP3 through `output.file` stays at
  252 bytes (header and Info frame, flushed by the header write) for the
  first 2 s. A crash loses up to that much; `on_close` sees the complete file
  because the trailer and the final flush happen in `stop`, before the
  output's close ([output-file.md §4.4](output-file.md#44-close-and-on_close)).
- Network URLs: the protocol's own buffering; `output.url` bounds a blocking
  write with its interrupt (io.md §3).

### 13.5 Chaining (Ogg restarts) and the destination

The instance **chains** when both hold:

- the current muxer's name is exactly `ogg` (FFmpeg's other Ogg muxers,
  `oga`, `ogv`, `opus` and `spx`, never chain: a metadata change after the
  header is dropped; checked with `format="opus"`: one `OpusHead`, the new
  title absent);
- the target is `Stream`, or `Url u` with `u` longer than 5 characters and
  starting with `file:`.

A chaining restart (FE §13.2) closes the current container (trailer written,
FFmpeg's I/O closed) and opens the next one **appending** (§13.3), whatever
`append` said, so the next link follows the previous one in the same file.
From the second link on, the container is non-seekable, has no interrupt and
does not consume protocol options; a protocol option in the format makes the
restart fail (checked: `blocksize=4096` with a metadata change after 1 s:
`Unrecognized options: container: blocksize=4096` raised from
`encode_metadata`, the output fails and, with the default `reopen_on_error`,
the script stops; the file holds one link).

A restart at a copy source switch (FE §13.3) uses the same rule; elsewhere
(`Url` not `file:`, any muxer other than `ogg`) an incompatible switch fails
the instance, and `output.url` turns that into a reconnect (io.md §11.3).

### 13.6 What the encoder assumes about its destination

- One destination per instance, fixed at creation; a restart goes to the same
  one. It never writes two destinations, except through a muxer that opens
  files itself (§13.1).
- A `Stream` destination is a pure byte sink: whatever the caller does with
  the returned bytes (store, send to several peers, drop) is invisible to it.
  It never learns whether a consumer joins late; `header` and
  `restart_offset` are offered to every caller.
- A `Url` destination is written, flushed, closed and (for seekable
  protocols) seeked by FFmpeg inside the instance's calls, on the caller's
  thread: a blocking protocol blocks that thread (io.md §3).
- The prefix `file:` is the only signal that a URL is a local file. It
  decides append and chaining; a plain path reaches the same file protocol
  without either.
- Nothing in the format or the seam says whether a destination is seekable;
  a format that needs seeking is not rejected before its first packet.

## 14. Tests

| Invariant                                                                                                               | Binding                                                                                | Incidental                                          | Failure modes simulated             | Doubles a new harness needs                   | Trace                                             |
| ----------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- | --------------------------------------------------- | ----------------------------------- | --------------------------------------------- | ------------------------------------------------- |
| A stopped `output.url` leaves a complete container at a file URL                                                        | the file parses and holds audio packets                                                | MPEG-TS/AAC, timings                                | none                                | a file URL                                    | ffmpeg conformance `io` (`url_lifecycle`)         |
| A failing URL with no restart stops that output once; others go on                                                      | `on_error` then `on_stop` once; another output keeps ≥ 3 s                             | `tcp://127.0.0.1:1`                                 | connection refused                  | an unreachable URL                            | `io` (`url_no_restart`)                           |
| A write stalled past `timeout` is an output fault; a shutdown during it is not                                          | `on_error` runs / does not run                                                         | WAV over TCP                                        | a peer that accepts and never reads | a stalled TCP peer                            | `io` (`output_stall*`)                            |
| An Ogg container chains links at a metadata change and at a copy source switch, both into a `Stream` and into a `file:` | link count 2; every packet of each source in its link; links decode alone and in order | Vorbis files, link splitter                         | none                                | a source with two tracks or a metadata change | conformance `ogg_stream`, `kc4`, `kc11`           |
| A late harbor listener of an mp3 `%ffmpeg` mount gets no ID3, Xing or Info                                              | absence of the three tags in the body                                                  | sine, 2 s                                           | late join                           | an HTTP client                                | `tests/harbor/harbor_output_join_points.liq`      |
| A Matroska stream reaches an Icecast listener with its EBML header first                                                | body starts with `1A 45 DF A3`                                                         | AAC + MPEG-4 video, liquidsoap's own Icecast server | none                                | an Icecast server                             | `tests/harbor/icecast_server_matroska_video.liq`  |
| A dedicated-encoder Matroska stream spanning many chunks decodes, plain and ICY                                         | FFmpeg decodes with `-xerror`                                                          | PCM in Matroska, metaint 1000                       | none                                | HTTP client                                   | `tests/harbor/harbor_output_dedicated_chunks.liq` |
| `output.file` `on_close` sees the finished file                                                                         | media tests read the file in `on_close`                                                | everything else                                     | none                                | —                                             | `tests/media/*`                                   |

Not covered by any test: appending (`output.file(append=true)`) with a
`%ffmpeg` format; truncation; a format that needs a seekable destination given
a non-seekable one; protocol options with a chaining restart; the plain-path
versus `file:` difference; a shared (non-dedicated) harbor mount with a
`%ffmpeg` container that has a header (see the findings).
