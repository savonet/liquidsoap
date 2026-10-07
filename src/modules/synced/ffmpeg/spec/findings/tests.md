# Findings: test suite

No test executable is built in this tree (`_build/.../ffmpeg/test/` does
not exist), so no test was run. Every finding is **read only** unless it
says **checked**.

## Defects

### The unhandled-packet test does not check subtitle packets, and can skip its video check

`test/test_unhandled_packet.ml:1-4`, `:64-70`. The header says the test
checks "that video and subtitle packets are delivered via the
on_unhandled_packet callback". The subtitle counter is printed and never
compared. The video check is guarded by `List.length video_streams > 0`,
so an input whose video stream is not detected passes. A binding that
drops every unhandled subtitle packet passes. **confirmed (second read)**:
`unhandled_subtitle` is incremented at `:34` and read only by the `printf`
at `:61`; the fixture command does give the file a subtitle stream
(`gen/gen_test.ml:119-120`), so the check was available.

### The unhandled-packet test bypasses the "asserted nothing" guard

`test/test_unhandled_packet.ml:64-73` against `test/test_assert.ml:1-3`.
The helper exists so "a test that checked nothing must not be able to
report success". This test never calls it and ends by printing `PASS`. It
is in the same executables stanza as the ten that do
(`gen/gen_test.ml:11-25`). **confirmed (second read)**: the file contains
no reference to `Test_assert`; its two checks are bare `exit 1` branches.

### "Shape and content for every output vector kind" has one kind with neither

`test/test_swresample.ml:1-4` against `:73-77`. The planar-frame output is
converted, ignored, and followed by `check "DblPlanarFrame converted"
true`. The check is a constant. It counts towards the "checked" total. A
wrong plane count or garbage content in planar frames passes. The
half-rate section (`:103-132`) also omits the planar frame. **confirmed
(second read)**: `:76-77` is `ignore pframe; Test_assert.check "…" true`,
so the step only shows that the conversion returned without raising.

### The interrupt check never runs on the reference CI and reports PASSED

`examples/interrupt.ml:2`; run at `gen/gen_test.ml:78`. With
`GITHUB_ACTIONS` set the program exits 0 before doing anything. The runner
prints `PASSED: interrupt` (`test/test_runner.ml:36`). The reason for the
skip is not in the code, the CHANGES file, or the history of the file (one
import commit). **confirmed (second read)**: `:2` is a top-level `exit 0`
ahead of everything else, `git log` shows one commit, and `CHANGES` mentions
only the addition of the callback.

### The hardware-encode steps pass under every non-crashing outcome

`examples/hw_encode.ml:36-39`, `:153-193`; run twice at
`gen/gen_test.ml:76-77`. Encoder missing: `exit 0`. Any exception in the
device or frame path: caught, printed as "Optional test failed", status 0.
No configuration found: a message, status 0. Both steps print `PASSED`.
On the reference CI (software FFmpeg build, `.github/actions/build-ffmpeg`)
the only code reached is the failed lookup. An output file is opened
before the lookup and never closed on that path (`:32`). **confirmed
(second read)** for the code paths; a crash is the only failing outcome.
That the CI build lacks `h264_nvenc` was not established: the configure
line in `.github/actions/build-ffmpeg/action.yml:57-60` does not mention it
and FFmpeg enables it automatically when the nv-codec headers are present.

### Report labels disagree with the unit requested

`test/test_info.ml:79-80`, `:117-118`. Video duration is labelled
"duration (ns)" and subtitle duration "duration (us)"; both are requested
in `` `Millisecond``. Output only; nothing compares it. A rewrite that
copies the labels inherits the error. **confirmed (second read)**: both
calls pass `~format:`Millisecond`.

## Asymmetries

### Three steps run outside the runner

`gen/gen_test.ml:196-198`, `:102-122`, `:126-137`, `:140-149`, `:153-190`.
Seven of the 45 steps are run by the rule directly: the subtitle remux, the
normaliser, `diff`, and the four `ffmpeg` commands that build fixtures. The
other 38 go through the runner. Their failure still fails the rule, with no
`FAILED:` line or group marker. **narrowed**: seven steps, not three;
counted with `grep -c '(run'` (45) against the runner lines (38).

### Normaliser reads in binary mode and writes in text mode

`test/normalize_line_endings.ml:2` (`open_in_bin`) against `:7`
(`open_out`). On a platform whose text mode translates line endings the
write puts back the carriage returns the program just removed. The `diff`
that follows (`gen/gen_test.ml:198`) then depends on how the fixture was
checked out. `.gitattributes` in the bindings sets nothing for the
fixture. **confirmed (second read)**: `.gitattributes` holds one line,
`CHANGES merge=union`.

### Examples assert option reporting; the dedicated test covers other entry points

`examples/encode_stream.ml:75-80` asserts unused-option reporting for
`open_output_stream` and `new_audio_stream` with `assert`.
`test/test_options.ml` covers `open_input`, `open_output` and
`Audio.create_encoder` through the counted helper. The two groups do not
overlap and only one is counted. Other option-taking entry points
(`open_input_stream`, `open_output_format`, `new_video_stream`,
`new_subtitle_stream`, `Video.create_encoder`, `BitstreamFilter.init`,
per-stream `opts` of the `configure_*_stream` callbacks) have neither,
while the test's header says "Every entry point must report the same
unused keys" (`test/test_options.ml:1`). **confirmed (second read)** for
the cited lines: `test_options.ml` calls only `Av.open_input`,
`Av.open_output` and `Avcodec.Audio.create_encoder`. The list of other
option-taking entry points was not rechecked.

### Errors swallowed in examples used as tests

`examples/demuxing_decoding.ml:72` prints a read error and goes on to
print "Demuxing succeeded" with status 0.
`examples/subtitle_remux.ml:111-118` prints write and read errors and
continues; only the later `diff` can notice. `examples/decode_stream.ml:108`
retries on invalid data without bound. Sibling programs let the same
errors escape. **confirmed (second read)** for the three cited places; the
sibling comparison was not rechecked.

## Gaps

### With one library missing the suite does not exist, and only one invocation says so

`gen/gen_test.ml:10`, `gen/dune:5-10`, `detect/dune:10-31`,
`detect/all_available.ml`. The generator emits nothing unless all seven
libraries are available. This includes `avdevice` and `avfilter`, which no
`test_*` program links. `detect` also reports a library unavailable when
its name is in `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS` (`detect/detect.ml:28-31,
76`), when `pkg-config` is absent, or when the version floor is not met.

**checked**: in a scratch project with the same layout (a `test/dune`
holding `(dynamic_include ../gen/x.inc)` and an empty generated include),
dune 3.23.1 gives:

- `dune build @ffmpeg_citest` → error "Alias "ffmpeg_citest" specified on
  the command line is empty", exit 1;
- `dune build @runtest` → exit 0;
- `dune build @all` → exit 0.

So the direct invocation fails loudly. Any wrapper that reaches the tests
through `@runtest`, `@all`, a default build, or an alias that depends on
`ffmpeg_citest` only when present, reports success having run zero tests.
The alias is not attached to `runtest` in any case
(`gen/gen_test.ml:28-30`).

**checked again (verification)**: an independent scratch project of the
same shape under dune 3.23.1 printed `Error: Alias "ffmpeg_citest"
specified on the command line is empty.` with exit 1, and exit 0 for
`@runtest`, `@all` and `@default`. The bindings' own CI calls
`dune build @ffmpeg_citest` directly (`.github/workflows/ci.yml:42-43`), so
that path is loud; what the preceding shared action runs was not read.

### Nothing counts the steps that ran

`gen/gen_test.ml:67-198`, `test/test_runner.ml:30-42`. Success is "the
rule's action exited 0". There is no total of steps run or passed. A
generated rule with fewer `run` lines, or with a step whose program exits
0 early (the two skips above), is indistinguishable from a full run.
**read only**

### The enclosing repository never runs this suite

No file outside the bindings refers to `ffmpeg_citest` (grep over `dune`,
`*.inc`, `*.yml`, `*.ml`, `*.sh`, `Makefile`, `dune-project`). The
enclosing repository's CI targets are `@citest`, `@doctest`, `@mediatest`
(`.github/workflows/tests.yml:31`). The rule is also scoped
`(package ffmpeg)` (`gen/gen_test.ml:30`), which hides it from a
`-p <other package>` build. Changes made to the bindings in the enclosing
repository are tested only when the bindings' own workflow runs.
**checked** (the grep); the consequence is **read only**.

### One rule, strict sequence: the first failure hides the rest

`gen/gen_test.ml:67-68` (`progn`). A failure at step 1 leaves 44 steps
unrun and unreported (**narrowed**: the action has 45 steps). Read-side tests (17-32) cannot run when a write-side
example (11-16) fails, so one encoder regression masks every
demuxer result. **read only**

### No time limit anywhere

`test/test_runner.ml:25` waits without bound. The interrupt check
(`examples/interrupt.ml:29-35`) opens a listening socket that nothing
connects to: with a broken interrupt callback it hangs instead of
failing. The same holds for a read loop that never reaches end of file
(`test/test_drain.ml:17-23` and the others loop until `` `Eof``).
**read only**

### The rule's result does not depend on the FFmpeg libraries

`gen/gen_test.ml:31-66`. The declared dependencies are the executables and
the fixture. The shared FFmpeg libraries the executables load, the
codecs enabled in them, and `GITHUB_ACTIONS` are not dependencies. After
an FFmpeg upgrade that keeps compiler and linker flags unchanged, the
executables are not rebuilt and dune treats the alias as up to date.
**read only**; stated from knowledge of dune's rule memoisation, see To
verify.

### Files written by the steps are undeclared and persist

`gen/gen_test.ml:76-198`. `A4.flac`, `out.mkv` and the rest are written
into the build tree as side effects. A step that stops producing its file
without failing leaves the previous run's copy for later steps to read.
The hardware-encode steps open `nvenc.mp4` and the options test opens
`test_options_out.mkv` in the same way. **read only**

### Why the normaliser exists is not recorded

`test/normalize_line_endings.ml`, `gen/gen_test.ml:196-198`. No comment,
commit message or CHANGES entry says where carriage returns come from.
The fixture has none and has multi-line cues. See To verify for the
likely source. **read only**

### Assertions too loose to catch the regression their comment names

- `test/test_info.ml:19-28`: "The getters must reach the AVClass object
  rather than the OCaml block wrapping it." The checks are
  `probesize > 0` and `max_delay >= -1`. A getter reading the wrong memory
  passes for any positive or large value. Comparing with a value just set
  through the option dictionary would be exact; the test does not do it.
- `test/test_seek.ml:38-40`: only a lower bound (`19 <= pos`). A seek that
  lands at the end of the file, or a read that returns the last frame,
  passes.
- `test/test_resample.ml:111-114`, `:162`: "more than zero bytes/samples".
  The raw files written (`:33`, `:38`, `:146`) are compared with nothing.
  Wrong sample values, wrong channel counts and wrong rates pass.
- `test/test_subtitle_read.ml:64`: "more than zero"; the fixture has five
  cues.
- `test/test_codec.ml:14-17`: the round trip compares names of ids. An id
  table that maps two ids to one name, or a constant `string_of_id`,
  passes.
- `test/test_swscale.ml`: plane sizes only; no pixel value.
  **read only**

### The options test states that it does not test what its last case looks like

`test/test_options.ml:4-7`, `:49-72`. The comment says the 20000-key case
"proves nothing about rooting": both a small and a large result pass with
the root removed. The case stays as a round-trip check. A rewrite must not
read it as a GC-safety test; none exists in the suite. **read only**

### Regression tests whose trigger depends on FFmpeg internals

- `test/test_input_streams_grow.ml` needs the MPEG-PS demuxer not to find
  four late streams during probing (`gen/gen_test.ml:151-152`). If a
  future demuxer finds them at open, the assertion
  `streams_at_open < streams_at_end` fails although the binding is
  correct; the property protected (close does not walk past the stream
  contexts) is then untested.
- `test/test_drain.ml` expects 625, which follows from the `color` source
  defaulting to 25 fps for `d=25` (`gen/gen_test.ml:132`); the rate is not
  stated in the command.
- `test/test_seek.ml` relies on `-bf 2` and on ten reads being "enough
  frames for the decoder to have buffered some" (`:26`).
  **read only**

### Signal deaths are reported with an unconventional code

`test/test_runner.ml:27`. `128 + n` uses the OCaml runtime's signal
number, which is negative for the signals it knows (segmentation fault
included). The result is non-zero, so the step fails, but the printed code
is not the shell's `128 + signo`. **checked**: `ocaml -e 'print_int
Sys.sigsegv'` prints `-10`, so a segmentation fault is reported as 118.

### Example warnings are not errors

`examples/dune:1-4` turns off warnings-as-errors for the examples in the
dev profile. The examples are 27 of the 45 steps. **read only**

## API gaps

### No way to observe a skipped or optional step

The harness contract is exit status only (`test/test_runner.ml:36-42`).
A step that cannot run on this host has no status to return other than 0
or failure. This is a gap in the harness interface, not in the bindings'
`.mli`.

Nothing else: the suite adds no evidence of a gap in the public API beyond
what the coverage map shows as untested.

## To verify

- libavcodec's SubRip encoder writes `\r\n` for a line break inside a cue,
  which would be the source of the carriage returns the normaliser
  removes. From memory of `srtenc.c`.
- The MPEG-PS demuxer creates streams on first sight of their packets and
  `avformat_find_stream_info` stops probing before 6 s with default
  `probesize`/`analyzeduration`. Asserted by the comment at
  `gen/gen_test.ml:151-152`; not run.
- The `color` lavfi source defaults to 25 fps. From memory.
- Every FFmpeg encoder sets `AV_CODEC_CAP_DR1`. Asserted by the comment at
  `test/test_codec.ml:8-10`; from memory it holds for the native AAC
  encoder in the versions on CI, not checked for all.
- The FLAC demuxer accepts a stream of raw FLAC frames with no `fLaC`
  header, which is what the stand-alone encoder example writes to
  `A4.flac`. From memory.
- dune does not re-run an alias action whose declared dependencies are
  unchanged, and does not track shared libraries loaded at run time. From
  knowledge of dune; not run here.
- What `savonet/build-and-test-ocaml-module` runs before the explicit
  `dune build @ffmpeg_citest` step (`.github/workflows/ci.yml:36-43`). Not
  read; it lives outside this repository.
