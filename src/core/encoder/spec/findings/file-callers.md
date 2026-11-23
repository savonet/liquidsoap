# Findings: file and pipe callers

Kept out of the as-built text. Each is marked **checked** (something was run;
what) or **read only**. Paths under `src/core/` unless stated. Scratch
scripts: `/private/tmp/claude-501/-Users-toots-sources-savonet-liquidsoap/7003a131-9f64-45cc-a903-0c65dccca755/scratchpad/spec-encoding/file-callers/`.

## Defects

1. **Pipe outputs never record metadata: filename/command interpolation and
   the factory's initial metadata are dead.** `current_metadata`
   (outputs/pipe_output.ml:61) is read by `get_encoder` (l.76) and
   `interpolate` (l.228) but never assigned since 2ba7ba739 (#2924, Feb 2023)
   removed `current_metadata <- Some m`. Scenario: `output.file(%mp3,
"/rec/$(artist)-$(title).mp3", s)` writes `/rec/-.mp3` for every track;
   `output.external(..., "cmd --title '$(title)'")` passes `''`;
   `%mp3(id3v2=true)` files start with an empty 10-byte ID3v2.3 tag, so no
   tags at all (LAME's `encode_metadata` is a no-op,
   optionals/lame/liquidsoap_lame.ml:119). **Checked**: `meta.liq`
   (`meta-$(title)-$(foo).wav` → `meta--.wav`), `tags.liq` (`%mp3(id3v2=true)`
   files start `49 44 33 03 00 00 00 00 00 00`, ffprobe sees no title; Ogg
   files do carry `title` because Ogg honours `encode_metadata`).

2. **A failed open pins the filename.** `prepare_filename` sets
   `current_filename` before `open_out_gen` (pipe_output.ml:360-375) and only
   `close_chan` clears it (l.387-399), which never runs for a file that did
   not open. Scenario: the getter first answers a read-only file, then a good
   one; every retry reopens the read-only one forever. **Checked**: `ro.liq`,
   getter switches at 1 s, errors "ro.wav: Permission denied" continue at
   1.3/1.9/… s, `ok.wav` never created. Same if `on_close` raises (the clear
   comes after it).

3. **`output.external` is named `output.file`.** `base` passes
   `~output_kind:"output.file"` for every pipe output (pipe_output.ml:56);
   `set_id ~force:false "output.external"` (outputs/output.ml:154) does not
   override it. Logs, the clock name (`clock.output.file`) and the telnet
   namespace say `output.file`. **Checked**: `ext_err.liq` without `id` logs
   `[output.file:3] Error while streaming` and `clock.output.file`.

4. **The `pipe` operator writes a header with the wrong rate after a
   restart.** `samplerate` (operators/pipe.ml:63) is both the rate written
   in the header to the process (l.71) and the rate read from the process's
   header (l.86). A process that resamples (input 44.1 kHz, output 48 kHz)
   gets a 48 kHz header on its second run while samples are still 44.1 kHz.
   **Read only.**

5. **The `pipe` operator mixes units when chunking.** `slen_of_len pos`
   (pipe.ml:160) treats a main-tick metadata position as a sample count, and
   `track_mark = Some pos` (l.163) compares a main-tick position with a byte
   offset. Correct only when main rate = audio rate (default 44.1 kHz audio,
   25 fps video: equal) and for position 0. Scenario: with a video rate that
   makes main ticks finer than samples, metadata chunks are cut at wrong
   offsets; a track mark coinciding with a mid-frame metadata is emitted as
   a plain metadata, and the break moves to the frame's end. **Read only.**

6. **Cover art reaches pipe outputs' encoders by default, against
   [output.md §3](../../../outputs/spec/output.md#3-a-streaming-cycle)**
   ("Cover art and other binary metadata entries are not handed to
   encoders"). `export_cover_metadata` defaults to `true` for `output.file`
   and `output.external` (pipe_output.ml:37-41) and is a parameter of
   `output.url`. Also `coverart` is in the export list but not in the cover
   list (stream/metadata_settings.ml:38-64), so it is exported even with
   `export_cover_metadata=false`. **Read only.**

7. **Reset is a no-op for pipe outputs, against
   [output.md §4](../../../outputs/spec/output.md#4-reset)** ("stop then
   start … An output for which a reset is a discontinuity says so"). The
   departure is real and stated nowhere outside the code
   (pipe_output.ml:96). **Read only.**

## Asymmetries

8. **Who writes the file depends on the format.** For every format but
   `%ffmpeg`, `output.file` writes the bytes and the encoder cannot seek; for
   `%ffmpeg` the encoder writes, seeks and closes the file and the output's
   `flush`, `perm` (only at creation) and channel are bypassed. The same WAV
   content gives a finalised header with `%ffmpeg(format="wav")` and
   permanent "unknown" lengths with `%wav`. **Checked**: `ffw.liq` (RIFF size
   `0x22ba` patched) vs `wav_append.liq` (`0xEFFFFFFF`).

9. **Append is signalled to FFmpeg through `truncate=0`.** `output.file`
   sets the option (pipe_output.ml:468-471); the FFmpeg encoder recognises
   `file:` + `truncate=0` and switches to an `O_APPEND` descriptor
   (optionals/ffmpeg/lib/encoder/instance.ml:94-138). A script that writes
   `%ffmpeg(truncate=0, ...)` to `output.url("file:...")` gets append too. In
   append mode the muxer loses seekability: `%ffmpeg(format="wav")` appended
   files have sizes `0xFFFFFFFF`; an MP4 would not finalise. **Checked**:
   `ffw.liq` (non-append then append twice: first header patched, the next
   two `ff ff ff ff`), `ff_append.liq` (raw: 8000 + 1600 bytes).

10. **`on_reopen` fires for three of four reopen paths.** The error-retry
    open goes through `prepare_pipe` in `encode` (pipe_output.ml:280), not
    `reopen`; neither `on_reopen` nor `on_start` fires. **Checked**:
    `ext_err.liq`, `on_reopen` never printed after a broken-pipe retry.

11. **Only `reopen_on_metadata` re-hands metadata to the new file.** After a
    `reopen_when`, `reopen()`, error retry or stop/start, the new instance
    starts with empty metadata (defect 1) and gets none until the next
    metadata in the stream; a time-split recording's second file has no
    title. **Read only** (follows from defect 1 and the call sequence).

12. **`output.external` with `self_sync=false` hides its source's sync.**
    `(`Static, self#source_sync false)`= not synced (pipe_output.ml:509,
source/source.ml:263), while`output.file`reports`source#cached_self_sync`. **Read only.**

13. **The encoded base never hands zero-length pieces, against the previous
    normative encoder.md §2** ("Callers hand an instance zero-length pieces as
    well as full ones"). `send_frame` skips `start = stop`
    (outputs/output.ml:352) and the pipe base also checks
    `0 < Frame.position` (pipe_output.ml:99). Encoders may rely on never
    seeing one from these callers while other callers may hand them. **Read
    only.**

## Gaps

14. **Data can be appended to anything.** `append=true` concatenates complete
    encoded files (two WAV headers, two FLAC `fLaC` blocks, chained Ogg, two
    ID3 tags); nothing warns. Only Ogg (chained) and headerless formats give
    a valid file. **Checked**: `enc_append.liq`, `wav_append.liq`.

15. **Every reopen with an unchanged name and `append=false` silently
    truncates the previous file**, including a fallible output whose source
    pauses. **Checked**: `meta.liq` (three tracks reopened into one name: one
    track left).

16. **Reopen on the first metadata leaves an empty file.** The file opened at
    start is closed before any byte when the first track's metadata triggers
    `reopen_on_metadata`; `on_close` is called for it. **Checked**:
    `tags.liq` (`tag0.bin`, 0 bytes).

17. **Opens and closes block the clock.** A FIFO without reader blocks the
    start until a reader appears (**checked**: `fifo.liq`, start delayed to
    the reader's open, "Latency is too high: we must catchup 1.30 seconds");
    `output.external`'s close waits for the process to exit (**read only**).

18. **Errors at the end are swallowed.** `close_encoder` swallows `stop`'s
    exception; `cleanup_pipe` swallows a failed write of the stop bytes;
    `output.external`'s close swallows `Sys_error` and ignores the exit
    status. A truncated last file or a failed process at stop is not reported.
    **Read only.**

19. **Two metadata at one position: the second is dropped.** `List.find_map`
    in `send_metadata` (outputs/output.ml:327-331). Whether a frame can hold
    two is the frame module's; nothing states it cannot. **Read only.**

20. **The `pipe` operator is a second WAV writer outside the seam**, with
    16-bit output only and its own restart-per-metadata protocol. No test
    covers it. **Checked** (behaviour only): `pipe.liq` via `tee`.

21. **`reopen_on_metadata` does not check the pipe's state.** The
    `encode_metadata` override (pipe_output.ml:290-293) calls `reopen`
    without looking at `is_open` or the error delay. Today this is harmless:
    while an error delay runs the whole cycle is skipped (l.268), and a failed
    open leaves the frame by exception. A redesign that keeps pulling during
    the delay would make it reopen early. **Read only.**

22. **`flush` is silently ignored with `%ffmpeg`.** The icecast-server dump
    passes `flush=true` together with `flush_packets=1`; only the latter
    matters for an FFmpeg format, the former only for attached non-FFmpeg
    formats. **Read only** (libs/extra/icecast_server/mount_dump.liq:55-63,
    attached.liq:34).

## To verify

23. FFmpeg's `file` protocol with `truncate=0` writes from offset 0 (the
    comment at instance.ml:94-96 says so; the code works around it, so the
    as-built text does not depend on it).
24. The 64 KiB OCaml channel buffer and the 1 s delay before `EPIPE` in
    `ext_err.liq` are consistent with a 64 KiB pipe buffer on macOS; Linux
    pipe sizes differ, so the delay is platform-dependent.
