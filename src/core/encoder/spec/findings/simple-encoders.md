# Findings: `%wav`, `%avi`, `%ndi`, `%external`

Kept out of the as-built text. "Old encoder.md" is
`git show HEAD:src/core/encoder/spec/encoder.md` (the normative text being
replaced).

## Defects

1. **`%external`: an early-exiting process loses its output and fails the
   clock.** _Checked_ (`process="head -c 10000"`, no restart: 2048 of 10 000
   bytes reached the file; clock failed with `Process_handler.Finished`;
   script exit 1). `encode` raises before `Strings.Mutable.flush buf`
   (`external_encoder.ml:140–153`), so collected bytes stay in the buffer
   and are never returned.

2. **`%external(restart_on_crash=true)` does not survive a crash.**
   _Checked_ (`head -c 50000`, several restarts then "Broken pipe in
   write()" escaped `encode`, clock failed, exit 1). Only
   `Process_handler.Finished` is caught (`external_encoder.ml:149–152`); a
   write to a process that exited but is not yet reaped raises
   `Unix_error EPIPE`.

3. **`restart_after_delay` is a no-op.** _Read only_ (parsed at
   `lang_external_encoder.ml:92`, printed by `to_string`, never consulted in
   `external_encoder.ml`). Dropped by ee2e412b0 (2016). Still documented
   in `doc/content/external_programs.md:86` and `CHANGES.md:2542`.
   Scenario: `%external(restart_after_delay=3600, process="lame …")`
   never restarts.

4. **`%external(restart_on_metadata)` drops audio at each restart, and can
   fail the clock.** _Checked_ for the drop (≈80 ms per restart: 1.8 s of
   PCM out of 2.06 s, 3 restarts). _Read only_ for the failure: the flag
   is cleared in `restart_decision` (`external_encoder.ml:45`) when the old
   process's end is handled, and `t.process` is `None` until the
   `` `Delay 0. `` respawn task runs (`process_handler.ml:240–245,356`); an
   `encode` in that window gets `Finished`, which is no longer excused.

5. **`%avi` puts all images of a frame in one `00db` chunk, and emits
   empty `00db` chunks for frames without an image.** _Checked_
   (`frame.duration=0.08`: chunks of 9216 bytes = 2 images, ffprobe reads 5
   video frames for 10 images; default 0.02 s: every other `00db` is
   empty). `avi_encoder.ml:45–87`. Players see the wrong number of frames
   and a wrong picture size per chunk.

6. **Liquidsoap cannot read its own AVI.** _Checked_
   (`input.external.avi("cat default.avi")`: `Avi.Read.Invalid("Wrong auds
fourcc.")`). Writer puts handler 0 in the audio `strh`
   (`avi.ml:122`), reader demands 1 (`avi.ml:254`); reader also only maps
   `00dc` to video (`avi.ml:292`) while the writer uses `00db`.

7. **`%avi` odd sizes: per-image payload disagrees with the header.**
   _Checked_ (33×17: 817 bytes per image, `strf` image size 841).
   Chroma planes use `width/2 × height/2` (`avi_encoder.ml:76–85`); the
   header uses `width*height*6/4` (`avi.ml:111`). Raw-video readers that
   trust the header misalign.

8. **`%wav(duration=…)` declares a length nothing enforces.** _Checked_
   (`duration=0.5` on a 0.56 s run: ffmpeg decodes 0.5 s and drops the
   rest; `duration=1.` on 0.56 s: "Packet corrupt"). The encoder never
   cuts, pads or rewrites (`wav_encoder.ml:37–48`). The data size is also
   not rounded to the block alignment (`int_of_float` of a float product).

9. **NDI timecodes drift.** _Read only_ (arithmetic).
   `timecode_base = 10_000_000 / main_rate` in integer division
   (`liquidsoap_ndi.ml:41`): at 44.1 kHz audio + 25 fps (main 44 100) the
   factor is 226 instead of 226.757, so timecodes run 0.33 % slow (≈12 s per
   hour); 0.16 % at 48 kHz.

10. **`%external(video=false)` is rejected.** _Checked_ ("Invalid value
    false for value mode. Only `true` or `false is allowed."). The second
match arm in `encoder_types.ml:163–169` catches `` `Bool false ``. The
    message itself is malformed (unbalanced backquote, "value mode").

11. **`%external` without `process` escapes as a bare OCaml exception.**
    _Checked_ ("Error -1: Exception raised:
    External_encoder_format.No_process", no position).
    `lang_external_encoder.ml:103–104`.

12. **`%ndi` messages: wrong text and doubled `%`.** _Checked_.
    `%ndi(%audio.none, %video.none)` reports "Invalid %%ndi encoder
    parameter!" (the script-level check at `encoder_types.ml:189` pre-empts
    "needs at least one audio or video field"); `raise_error` takes a plain
    string, so `%%` is printed literally.

## Asymmetries

13. **Script-level and value-level types disagree for `%external`.**
    _Checked_. `width=`/`height=` turn video on in the value
    (`lang_external_encoder.ml:67–82`) but not in the script-level type
    (`encoder_types.ml:154`); `--check` passes, start-up fails with a type
    error at "Unknown position". Similarly `%avi(mono)` passes the script
    type and fails at evaluation without a position.

14. **`header()` is empty for `%external`** although its output usually
    needs a header (e.g. an Ogg or WAV produced by the process). Old
    encoder.md §3: `header()` "MUST be complete for every format the
    encoder produces". A late harbor joiner of an `%external` stream gets
    no header. _Read only_.

15. **`restart_offset` is "none" for `%external` after a process
    restart**, although the bytes of the new process start a new stream
    with its own header (checked: one `RIFF` per process in the output).
    Old encoder.md §3 defines `restart_offset` for exactly that; `%ogg`
    reports it. Callers cannot split files or reset joiners there.

16. **Only `%ffmpeg` can finalise a header.** `output.file` lets `%ffmpeg`
    write the file itself (`pipe_output.ml:449`); `%wav` and `%avi` cannot
    seek back and ship placeholder sizes (0xEFFFFFFF / 0xFFFFFFFF) and no
    AVI index, forever. _Read only_ for the cause, _checked_ for the bytes.

17. **Channel default.** `%wav`, `%avi`, `%external` hard-code 2 channels
    (#933); `%ndi` uses `frame.audio.channels` at type computation.
    _Read only_.

18. **`%ndi` registers an encoder that always raises.** Other outputs
    accept `%ndi` at evaluation and fail at run time (checked with
    `output.file`: clock fails after 0 ticks, exit 1). Every other format
    unusable by an output is rejected at evaluation. Old encoder.md §1
    places "no encoder for this format" at evaluation.

19. **Old encoder.md §2 "stop drains and returns final bytes; stopped
    exactly once"**: `%wav`/`%avi` drop samples held by the rate converter
    (_read only_); `%external` `stop` raises `Finished` when the process is
    gone and on a second call (_read only_, `process_handler.ml:89–94`).

20. **Old encoder.md §2 zero-length pieces**: `%avi` turns a zero-length
    piece into 16 bytes of empty chunks; `%wav` emits the header on a
    zero-length first piece. _Read only_ (code paths §3 of each file).

## Gaps

21. **`%external` stdin write and stdout collection share one lock**
    (`external_encoder.ml:94,140`). A process that stops reading stdin
    until its stdout is drained deadlocks the clock thread and the
    scheduler task. _Checked, not reproduced_: `cat` and a 20× expanding
    Python filter ran 4 s at real time without stalling (14 183 440 bytes,
    all collected).

22. **`%external` `stop` waits on a condition once, with no predicate**
    (`external_encoder.ml:159`): a spurious wake-up returns early and loses
    the tail; a process that never exits blocks `stop` (and the caller's
    thread) forever. The `on_stop` signal is sent without the mutex.
    _Read only_.

23. **`%external` bytes from `encode` are not the encoding of the frame
    given**: they are whatever stdout produced since the last call. Nothing
    tells the caller (e.g. for bitrate accounting or timing). _Checked_
    (all bytes arrive, at a lag).

24. **The metadata given to `%external` never reaches the process**;
    `restart_on_metadata` only restarts it. _Read only_.

25. **`int_string`/`dword` clear bit 31** (`wav_aiff.ml:254`,
    `avi.ml:36`): sizes ≥ 2 GiB (`%wav(duration=…)` of ~3.4 h stereo
    16-bit at 44.1 kHz) are written modulo 2³¹ silently. _Read only_.

26. **No test covers any header byte** of `%wav` or `%avi`, nor any
    `%external` behaviour beyond "it starts". _Read only_ (git grep).

## To verify

27. mm's `Audio.S16LE/S24LE/S32LE/U8.of_audio` clip floats to [-1, 1]
    (claimed from memory in wav.md §3, avi.md §3).
28. `Audio_converter.Samplerate` (libsamplerate backend) holds samples
    across calls, which `stop` then drops (wav.md §9).
29. NDI SDK: `clock_video`/`clock_audio` make `send_video`/`send_audio`
    block to pace the stream (ndi.md §C7); FLTP `stride` is the channel
    stride in bytes.
30. AVI: positive `biHeight` for I420 means top-down (YUV formats ignore
    the sign) — assumed by `%avi` writing a positive height.
