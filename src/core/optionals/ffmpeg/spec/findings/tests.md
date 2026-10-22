# Findings: test suite

Findings about the tests of the FFmpeg layer, gathered while writing [tests.md](../tests.md). Paths are relative to the worktree root.

## Defects

### Encoder matrix passes on a single packet

What: every encoder template accepts a stream when `duration >= 10. or nb_read_packets > 0` (`tests/media/encoder_*.liq.in`, the `on_close` condition). A file with one packet per stream passes; a truncated or half-written file passes.
Why it matters: the 33-configuration matrix is the widest encoder coverage there is, and it pins presence, not length or continuity. The duration branch is dead weight.
Read only.

### Transparency test has no transparency

What: `logo.png` is produced with `-f mjpeg` (`tests/media/dune`, rule `logo.png`), so it is a JPEG with pix_fmt `yuvj444p` and no alpha plane. The overlay test (`test_ffmpeg_transparency_filter.liq`) then checks codec parameters only.
Why it matters: alpha handling in overlay and in the video converters is untested although the test name says otherwise.
Checked: ran the fixture command; `file` reports JPEG, the prober reports `mjpeg,yuvj444p`.

### Filter array-argument test cannot fail on the filter

What: `test_ffmpeg_filter_array_args.liq` applies `aformat(sample_fmts=["s32","fltp"])`, then encodes with aac into a wav container and asserts the probed `sample_fmt` is `s32` or `fltp`. The aac decoder always reports `fltp`.
Why it matters: the assertion holds whatever the filter does, including if the array argument is ignored.
Checked: the same pipeline with the ffmpeg CLI and `sample_fmts=s32` probes `aac,fltp`.

### Decoder tests assert values fixed by the output encoder

What: `ffmpeg_audio_decoder.liq`, `mono.liq`, `stereo.liq` assert channel count and 48000 Hz on a file written with `%wav(mono)` / `%wav(stereo)` under `samplerate=48000`; the encoder fixes both values. `mono.liq` and `stereo.liq` also set `settings.decoder.priorities.ffmpeg := -1` on mp3/flac inputs, so they do not exercise the FFmpeg decoder at all. The a/v decoder family runs on a mono input file and asserts `stereo`, which the internal default layout gives regardless.
Why it matters: decoder channel handling and resampling are effectively unasserted; three tests named as FFmpeg decoder tests test other code.
Read only.

### Bitstream filter test compares parameters the filters do not change

What: `ffmpeg_bitstream_filter.liq` applies `h264_mp4toannexb` and `aac_adtstoasc` and compares layout, rate, sample/pixel format and codec type with the input.
Why it matters: passes if both filters are no-ops or are never applied. Nothing checks the bitstream format.
Read only.

### Filter tests check codec names only

What: `ffmpeg_filter.liq`, `ffmpeg_filter_parse.liq` (whose graph contains `fps=fps=30`), `ffmpeg_add_text.liq`, `test_ffmpeg_filter_changing_rate.liq` assert only codec name, pixel format and audio format of the output.
Why it matters: the filter graph may do nothing and the tests pass; the 30 fps conversion and the changing-rate case pin nothing about rate or timestamps.
Read only.

### Tests that pass on the first event

What: these call "pass" on the first track start, or after a fixed 0.1 s, before any output is verified: `pcm_s16_decode.liq`, `pcm_f32_decode.liq`, `video-only.liq`, `GH2602.liq`, `unified-pcm-types.liq`, `srt_raw_pcm.liq`, `ffmpeg-copy-input-http.liq`. `subtitle_multi.liq`, `subtitle_copy_encode.liq`, `subtitle_reencode.liq` pass on any file close.
Why it matters: each proves only that the script typechecks and starts; a decoder or encoder that produces garbage, or nothing after the first frame, passes.
Read only.

### Side-data test does not pin the handler call count

What: `test_ffmpeg_side_data.liq` comments "Each decoder of the file asks once" but asserts `list.length(undecided()) > 0`.
Why it matters: repeated or per-frame calls of the undecided-rotation handler pass.
Read only.

### `test.raises` accepts a function that does not raise

What: `tests/test.liq:207`. The "was not raised" error is raised inside the same `try`, so it is caught: with `e=null` it is swallowed, and with `e=error.failure` its kind matches. Not used by FFmpeg tests today; used by language tests.
Why it matters: any test written with it can pass while testing nothing.
Checked: a script calling `test.raises(fun () -> 1)` and `test.raises(fun () -> 1, error.failure)` continues past both and exits 0.

### `ffmpeg_get_type_test` passes with no fixtures

What: `tests/core/ffmpeg_get_type_test.ml:209,214` prints `SKIP:` and exits 0 when the media files are missing.
Why it matters: combined with it being unwired (below), a run can report success having tested nothing.
Read only.

### `stream_decoder_test` asserts nothing

What: `tests/core/stream_decoder_test.ml` decodes and re-encodes to a file and never inspects the result; `Avutil.Error \`Invalid_data` is swallowed in the loop.
Why it matters: only an exception other than end of file fails it.
Read only.

### Rejection tests accept any error

What: `tests/language/encoders.liq` (`try … test.fail() catch _ do () end`) and `incorrect()` in `tests/language/type_errors.liq` treat any error as the expected rejection, including a parse error in the test string or a missing optional.
Why it matters: the reason for rejection (naming convention, non-literal channels) is not pinned.
Read only.

## Asymmetries

### Copy concatenation runs only with the relaxed compatibility check

What: `test_ffmpeg_copy_concat.liq` sets `settings.ffmpeg.content.copy.relaxed_compatibility_check := true` because the fixtures report different frame rates.
Why it matters: the only copy-concatenation timestamp test never runs the default (strict) check; what strict mode does with these inputs is untested.
Read only.

### `ffmpeg_quality` builds an encoder with two streams on one field

What: `tests/core/ffmpeg_quality.ml`, second case: both streams are keyed `Frame.Fields.audio`. Only one sets `b`.
Why it matters: whether the HLS bitrate sums several streams is not pinned, and the test input is a shape the parser does not produce.
Read only.

### Codec-parameter oracle is environment-dependent

What: `hls_global_headers.liq` pins `avc1.64001f`; `all_media_file_descriptions.json` pins muxer-chosen time bases (1/12800, 1/12288, 1/28224000, 1/14112000, …).
Why it matters: these move with the host's x264 and FFmpeg versions, not with Liquidsoap.
Read only.

## Gaps

### No packet-level timestamp assertion anywhere

What: no test reads packet PTS/DTS (no prober call with packet listing). All timestamp coverage is aggregate: stream duration (exact only in copy concat), cue times, segment-duration sums, signal level after a seek.
Why it matters: non-monotonic DTS, PTS < DTS, duplicate timestamps or a gap compensated elsewhere in the stream all pass every test. This is the area the user asked about.
Read only.

### Timestamp situations with no test

What: no test covers a packet with no PTS or DTS; non-monotonic timestamps inside one copied stream; B-frame streams copied across a boundary (fixtures are encoded with defaults, but nothing checks DTS order); cross-stream interleaving; timestamps across encoder restart, reconnect (`GH2850.liq` is skipped unconditionally), `insert_metadata` with a new track into an FFmpeg file, or `reopen_on_metadata`; continuity across an HLS resume (`test_ffmpeg_distributed_hls.liq` checks codec parameters only); the frame-rate-change filter test asserts nothing on timing.
Why it matters: these are the cases `timestamps.md` describes; none is pinned.
Read only.

### Unwired tests

What:

- `format_dump_test`, `ffmpeg_get_type_test`, `ffmpeg_stream_description_test` (and `ffprobe_process_test`) have rules only under their own alias (`tests/core/dune:102-183`), not under `citest`, `runtest` or `mediatest`.
- `tests/media/test_ffmpeg_drop_tracks.liq` and `test_lufs_true_peak.liq` have no rule.
- `test_ffmpeg_hls_copy_mp4` has a rule but is not in the `mediatest` alias (`tests/media/dune`, final alias).
- `tests/failures/test_ctype.liq` has no rule.
  Why it matters: CI runs `@citest`, `@doctest`, `@mediatest` only (`.github/workflows/tests.yml`), so none of these run. `%X.drop` tracks, format serialization of FFmpeg contents, per-track decode/copy selection and the stream-description snapshot are therefore not covered in CI.
  Checked: grep over every `dune`/`dune.inc` and the workflow files finds no other reference to these aliases or files.

### Skips reported as success

What: `run_test` maps exit 123 to success with a warning. `GH2850.liq` and `hls_discontinuity.liq` call `test.skip()` unconditionally; three HLS tests skip on Debian 10; `GH3944.liq` skips when `loudnorm` is absent; any test using the assets directory skips when it is empty.
Why it matters: a CI run is green with these tests never executed; `hls_discontinuity.liq` holds the only exact segment-duration and discontinuity expectations.
Read only.

### Surface with no test

What: no test calls `file.metadata.ffmpeg`, `request.duration.ffmpeg`, `enable_external_ffmpeg_decoder`, `ffmpeg.filter.audio_video.output`, `ffmpeg.raw.decode.video`, `ffmpeg.raw.decode.audio_video`, `ffmpeg.raw.encode.video`, `video.add_text.ffmpeg.raw`, or the `track.ffmpeg.*` encode/decode operators directly. No test sets `settings.ffmpeg.autorotate.*`, `settings.ffmpeg.format_change.*`, `settings.ffmpeg.scaling_algorithm`, `settings.ffmpeg.scaling_threads` or `settings.ffmpeg.log.*`. Hardware acceleration is only typechecked. The FFmpeg samplerate converter runs only on hosts without libsamplerate (most tests force `native`).
Why it matters: these paths are specified but unpinned.
Read only (checked the operator list with `liquidsoap --list-functions-md` and grepped `tests/`).

### Error paths

What: the only FFmpeg error asserted is ENOENT on input open (kind `ffmpeg`). No test covers decoder errors mid-stream, encoder errors, an unknown codec or format, invalid options, or incompatible copy parameters at blit time.
Why it matters: failure semantics in every subsystem file are untested.
Read only.

## To verify

### `--auto-promote` in CI and diff rules

What: `.github/scripts/test-posix.sh` runs `dune build … --auto-promote`. The type snapshots and `format_dump_test` are `diff` rules.
Why it matters: if a promoted diff did not fail the build, snapshot regressions would be silently accepted. Dune normally still reports the failed diff; not confirmed here.
Read only.

### Fixture start offsets in copy concat

What: the offsets 41.17 s / 25.2 s / 8.17 s come from `setpts=N+1235` etc. at 30 fps.
Why it matters: tests.md states them as the provoked condition.
Checked for the first file shape: an equivalent 2 s file probes `start_time` 41.166016.
