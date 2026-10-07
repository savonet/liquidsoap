# Findings: build, detection, code generation

"Checked" entries were confirmed against a real build of the generated
files (FFmpeg `N-126774-g6242bd002d`, libavutil 61.7.100, OCaml 5.5.0) or
by the command named. Paths are relative to the ffmpeg root; generated
files are under `_build/default/src/modules/synced/ffmpeg/`.

## Defects

### `Val_CodecID` returns the range marker instead of the first codec of a range

`gen_code/gen_code.ml:429-430`; consumer `avcodec/avcodec_stubs.c:1518`.
The `codec_id` block scans the whole enum with no exclusion, so
`AV_CODEC_ID_FIRST_AUDIO`, `FIRST_SUBTITLE` and `FIRST_UNKNOWN` become rows
placed just before `PCM_S16LE`, `DVD_SUBTITLE` and `TTF`, which have the
same C value. `Val_CodecID` returns the first matching row, so those three
real codecs are reported as `` `First_audio ``, `` `First_subtitle ``,
`` `First_unknown ``. The per-range tables do not have the problem because
their start line is consumed. **Checked**: generated
`avcodec/codec_id_stubs.h` rows 942-943 and 1194-1195; `codec_id.ml` lines
1405-1406, 1629-1630, 1657-1658.
**Checked again, reach narrowed (verification).** Regenerating the header by
hand gives the same 1102 rows. `Val_CodecID` has one call site, the
bitstream-filter codec list (`avcodec_stubs.c:1518`). In FFmpeg n7.1.5 and
n8.1.3 the only filter listing one of the three shadowed codecs is
`pcm_rechunk` (`libavcodec/bsf/pcm_rechunk.c:217`, `AV_CODEC_ID_PCM_S16LE`),
so `` `First_audio `` is returned today; no filter lists `DVD_SUBTITLE` or
`TTF`, so the other two markers are latent. `Val_AudioCodecID`,
`Val_VideoCodecID`, `Val_SubtitleCodecID`, `Val_UnknownCodecID` and the
codec iterator (`avcodec_stubs.c:1436-1448`, per-range tables) contain no
marker row and are unaffected. The OCaml-to-C direction is harmless:
`` `First_audio `` converts to the value of `PCM_S16LE`.

### The `hw_device_type` stop pattern can never match

`gen_code/gen_code.ml:505`. The stop pattern is
`[ \t]*AV_HWDEVICE_TYPE_NONE ` with a trailing space; the enum line is
`AV_HWDEVICE_TYPE_NONE,`. The scan runs to the end of the preprocessed
input. The table is correct today only because no later line starts with
`AV_HWDEVICE_TYPE_`. Had the pattern matched, the table would be empty
(`NONE` is the first member). **Checked**: generated table has all 15
members; `grep AV_HWDEVICE_TYPE_NONE hwcontext.h`.
**Checked again (verification)**: the generator run by hand gives 15 rows
with and without a working preprocessor; `hwcontext.h` at n7.1.5, n8.1.3 and
n9.0.2 has `NONE,` and no line starting with the pattern (the three
`AV_HWDEVICE_TYPE_NONE ` occurrences are inside `*` comment lines, which
the anchored match rejects).

### Include-path discovery drops every directory whose path contains a capital `I`

`gen_code/config/discover.ml:30-37`. A `-I` flag is recognised by
splitting the token on the character `I` and requiring exactly two pieces.
`-I/opt/Include` or `-I/home/Ivan/ffmpeg/include` yields three pieces and
is ignored without a diagnostic. When every `-I` flag of a package is
dropped this way, the package falls back to
`pkg-config --variable=includedir` (`discover.ml:50-54`), which usually
names the same directory, so the loss is real only when another flag of the
same package survives or `includedir` is unset or different. A path
containing a space contributes its first fragment as a bogus directory
(`-I/my dir/include` gives `/my`); `-isystem /usr/Include` contributes the fragment after its capital `I`. The generator still receives the library's own `-I` flags on its
command line, which masks the loss in the normal case.
**Narrowed; checked**: a copy of `split_flags` run on these inputs printed
`[]` for `-I/opt/Include`, `["/usr/include"]` for
`-I/home/Ivan/ffmpeg/include -I/usr/include`, `["/my"]`, and the fragment after the capital `I` of `Include`.

### The promoted summary file is empty

`dune:1-19`. The availability lines are `echo`ed outside
`with-stdout-to`, so they go to the build output; the file `ffmpeg.config`
that is promoted to the source tree receives `(echo "")`. A reader of the
file learns nothing. **Confirmed (second read)**: no `with-stdout-to`
encloses the seven availability `echo`s, the only redirected action is
`(echo "")`, and no other rule writes the file (it is in `.gitignore:6`).
Still not built.

### README does not match the tree

`README.md:14,82`. It lists a `decode_audio` example that does not exist
and links examples on a `master` branch while CI and the mirror use
`main`. It states FFmpeg 5.1 as the minimum; CI builds only 7.1 and 8.0.1
(`.github/workflows/ci.yml:22-26`), so the stated minimum is untested.
**Confirmed (second read)**: `examples/` has no `decode_audio.ml`; the CI
matrix is `n8.0.1` plus one `n7.1` job and both workflows trigger on `main`.

## Asymmetries

### `SWR_DITHER_NONE` is missing where `AV_CODEC_ID_NONE` is re-injected

`gen_code/gen_code.ml:607-609` versus `:416-430`. Both blocks start on the
line of their `NONE` member, which the scan consumes. The codec id blocks
add `NONE` back as an extra; the dither block does not, so
`Swresample_options.dither_type` has no constructor for
`SWR_DITHER_NONE`. **Checked**: generated `swresample_options.ml` has
three dither constructors. **Confirmed (second read)**:
`swresample.h:148-154` puts `SWR_DITHER_NONE = 0` on the line the start
pattern consumes, and the C table (`swresample_options_stubs.h:6-12`) has
three rows.

### `_NB` as a stop marker cuts members declared after it

`gen_code/gen_code.ml:472,484`. `AVColorPrimaries` and
`AVColorTransferCharacteristic` declare extension members after `_NB`
(`AVCOL_PRI_V_GAMUT`, `AVCOL_TRC_V_LOG`, at 256) in the FFmpeg version
built against. They are not in the tables, so `Val_ColorPrimaries` /
`Val_ColorTrc` raise `Failure` for a frame carrying them, while every
other colour value converts. **Checked**: `pixfmt.h` of the installed
FFmpeg and the generated `.ml` files.
**Worse than reported (verification)**: the same conversions serve
`color_primaries_from_name` and `color_trc_from_name`
(`avutil/avutil_stubs.c:692,711`), which return an option for an unknown
name; FFmpeg's lookup also walks the extension name table
(`libavutil/pixdesc.c`, n8.1.3), so the name of an extension member raises
`Failure` instead of returning a value. The members exist at n8.1.3 and
n9.0.2 and not at n7.1.5.

### Preprocessed and non-preprocessed tables treat guarded members differently

`gen_code/gen_code.ml:534,554,564,574,584,594`. Enum tables go through
`cc -E`, so members under a false `#if` disappear. Flag tables are read
raw, so a `#define` under a false `#if` still becomes a table row and then
fails to compile as an undeclared identifier. No such guard exists in the
flag headers of the FFmpeg built against. **Read only.**

### Context and pkg-config handling is implemented twice

`detect/detect.ml:33-42,57-70` and `gen_code/config/discover.ml:16-25,58-70`
duplicate the per-context variable rule. They then diverge: detection
honours `LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS` and uses dune-configurator's
`Pkg_config`; discovery ignores the exclusion list, queries all seven
packages, reads `PKG_CONFIG` itself and falls back to
`/usr/local/include`, `/usr/include`. The headers a table is scanned from
and the headers the stub is compiled against are therefore chosen by two
different procedures. **Read only.**

### Some `.ml` generation rules depend on the matching header, others do not

`avutil/dune:65,87`, `avcodec/dune:52,74,96,118` list `<table>_stubs.h` as
a dependency of `<table>.ml`; the other eleven `.ml` rules do not. The
generator never reads that header. **Read only.**

### Fallback header lists do not match the includes

`avutil/dune:31-50` lists `media_types_stubs.h`, which `avutil_stubs.c`
does not include; `avcodec/dune:28-37` omits it although
`avcodec/avcodec_stubs.c:18` includes it. The rules are inert, so nothing
breaks. **Checked** for inertness: `dune rules …/avutil_stubs.o` shows the
header dependencies coming from the directory, not from this rule.

### Ten variant constants are generated and never used

`gen_code/gen_code.ml:260-356`. `PVV_Again`, `PVV_Buffer`, `PVV_Failure`,
`PVV_Forced`, `PVV_Frame`, `PVV_Link`, `PVV_Nb`, `PVV_Ok`, `PVV_Packet`,
`PVV_Sink` appear in no stub. **Checked**: `grep -o 'PVV_…'` over all
`*_stubs.c`/`*_stubs.h` compared with the generated header.

## Gaps

### A failed preprocessor run silently degrades to scanning the raw header

`gen_code/gen_code.ml:220-231`. Any exception, including the assertion on
the exit status, falls back to reading the header as text: conditional
sections are not resolved, includes are not followed (so
`hw_config_method`, which relies on `avcodec.h` pulling in `codec.h`,
loses every row), comments are scanned. The generator
reports nothing; the shell's or compiler's own standard error passes
through. **Narrowed; checked**: the built generator run with
`/nonexistent/cc` and with `false` as compiler exits 0 (the first prints
only the shell's "No such file or directory", the second nothing).
`hw_config_method` becomes an empty table (`_TAB_LEN 0`, against 4 rows):
its members live in `codec.h`. `subtitle_type` is unchanged, because
`enum AVSubtitleType` is defined in `avcodec.h` itself at n7.1.5, n8.1.3 and
n9.0.2. `codec_id`, `pixel_format` and `hw_device_type` are byte-identical
with this FFmpeg. A wrong include directory triggers the same path: with
`-I<empty dir>` the compiler fails on `libavutil/avconfig.h`, the raw
`pixfmt.h` is scanned, exit 0.

### A missing header or start line produces an empty result and exit 0

`gen_code/gen_code.ml:119-121,202-204`. Header not found: a warning on
standard error and an empty output file. Start pattern not found: the
block is skipped with no message. The failure surfaces later as an
unrelated OCaml or C compile error (unbound module member, undeclared
function). **Checked** for the missing header: the generator rebuilt with
`Config.paths = ["/nonexistent/include"]` and run for `pixel_format` in `h`
and `ml` modes printed the warning, wrote two 0-byte files and exited 0.
An unknown generator name does fail (`Failure`, exit 2). The missing start
line is **confirmed (second read)**: the `if` at `:121` has no `else`.

### Generated headers define external functions and have no include guard

`gen_code/gen_code.ml:148-198,235-238`. Each header may be included by one
translation unit in the whole program. This is respected by hand and
stated nowhere. `media_types_stubs.h` is installed for downstream C code
(`avutil/dune:25-28`) while the avcodec library already contains its
definitions: a downstream stub that includes it and links statically with
`ffmpeg-avcodec` gets duplicate `MediaTypes_val`, `Val_MediaTypes`.
**Read only.**

### Table symbols are an unwritten interface

`avcodec/avcodec_stubs.c:896-905,934-943,1090-1093,1437-1448`,
`avutil/avutil_stubs.c:753`. Stubs index `…_TAB[i][0|1]` and `…_TAB_LEN`
directly; the names are derived from prefix and OCaml type name
(`gen_code.ml:122-123`). A rewrite that keeps only the three functions
breaks these call sites. **Read only.**

### Alias members share a C value; the reverse lookup picks by table position

`gen_code/gen_code.ml:180-198`. `Smptest428_1`, `Smptest2084`, the four
`AV_CH_LAYOUT_*` alias defines and the codec id markers all duplicate a C
value. Which constructor the C-to-OCaml direction returns depends only on
header order, and for codec ids on the extras being emitted first. Nothing
states the intended winner. **Checked** in generated tables.

### The public variant types depend on the FFmpeg headers present at build time

`gen_code/gen_code.ml:99-105`. No allow-list, rename table or minimum set
exists: the constructor set of `Pixel_format.t`, `Codec_id.*` and the rest
is whatever the installed headers declare. OCaml code matching on a
constructor compiles or not depending on the FFmpeg version. The error
text "Do you need to recompile the ffmpeg binding?" is the only
acknowledgement. **Read only.**

### Availability is per library but libraries depend on each other

`<lib>/dune` `enabled_if`, `detect/dune`. Only the umbrella is `(optional)`.
If `avutil` is unavailable (for example excluded through
`LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS`) while `avcodec` is available, `avcodec`
is enabled with a dependency that does not exist. Nothing derives a
library's availability from its dependencies'. **Read only.**

### FFmpeg headers are not build dependencies of the tables

`avutil/dune`, `avcodec/dune`, `swresample/dune`: each generator rule
depends only on `../detect/<lib>_c_flags`. Upgrading FFmpeg in place under
the same prefix leaves stale tables until a clean. The detection rule's
`(deps (universe))` reruns detection but its output is unchanged, so
nothing downstream reruns. **Read only.**

### C flags are pasted unquoted into a shell command

`gen_code/gen_code.ml:223-225`. Only the header path is quoted. A flag
with a space or shell metacharacter (an include directory with a space)
breaks the preprocessor command, which then triggers the silent fallback
above. The `.sexp` flag files are likewise written with no quoting
(`detect/detect.ml:6-11`). **Read only.**

### `VALUE_NOT_FOUND` is an in-band value

`gen_code/gen_code.ml:238`. `0xFFFFFFF` is returned through the table's C
type; for the `uint64_t` mask tables it is a representable mask. No table
member has that value today. **Read only.**

### `@runtest` runs nothing

`gen/gen_test.ml:29`, `opam/*.opam`. The opam build command includes
`"@runtest" {with-test}`, but every test hangs off the `ffmpeg_citest`
alias. `opam install --with-test` exercises no test. **Confirmed (second
read)**: no `dune` file in the tree has a `test` stanza or a `runtest`
alias, and the generated rule names only `ffmpeg_citest`.

### Stale submodule declaration

`.gitmodules` declares an `m4` submodule over the `git://` protocol; no
`m4` directory exists and nothing references it. **Checked**: `ls`.

## API gaps

### Range markers are public codec identifiers

`Codec_id.codec_id` (re-exported as `Avcodec.id`) contains
`` `First_audio ``, `` `First_subtitle ``, `` `First_unknown ``, which name
no codec, and they shadow three real codecs in the C-to-OCaml direction
(see Defects). **Checked.**

### No constructor for "no dithering"

`Swresample.options` includes `dither_type`, which has no value for
`SWR_DITHER_NONE` and none for the noise-shaping dithers. An explicit
"none" cannot be expressed; it is only reachable as the default.
**Checked** in the generated type.

### Enum types are not fixed by the interface

The `.mli` files alias generated types (`type t = Pixel_format.t`, …), so
the frozen surface does not actually pin the constructor sets. A rewrite
has to decide whether "same API" means "same generation rule" or "same
constructors as some reference FFmpeg version". **Confirmed (second
read)**: `avutil/avutil.mli:206-261` and `avcodec/avcodec.mli:144,268,391,
445,458` are plain aliases of the generated types, with no constructor list.

## To verify

- Settled: the minimum versions of `detect/dune` fall between the 5.0 and 5.1
  releases, and 5.1 is the first release that satisfies them. **checked**
  against the version headers at each release tag
  ([compatibility.md](../compatibility.md) §4).
- dune-configurator behaviour assumed in the spec: `Pkg_config.get`
  honours `PKG_CONFIG`; `query_expr_err` runs an existence test on `expr`
  and then `--cflags` / `--libs` on `package` passed as one argument
  holding several module names; on macOS it may extend `PKG_CONFIG_PATH`
  with Homebrew paths. Only the Linux result was observed.
- `%{cc}` expanding to the compiler plus the OCaml configuration's C flags
  as one string: inferred from the quoting in the rules; the generator's
  actual command line was not captured.
- dune fallback-mode semantics (rule ignored when the target exists in the
  source tree) are from dune's documentation; only the consequence (header
  dependencies come from the directory) was observed with `dune rules`.
- The variant hash rule was checked by recomputing six generated
  constants; it was not checked on a 32-bit target.
- Behaviour on Windows (the link-flag filter, `default.windows` context
  variables) and under cross-compilation was read, not run.
- The `ffmpeg.config` summary output was not built.
- `Str` treating `\}` as a literal brace: consistent with the observed
  `subtitle_type` and `filter_type` tables, not checked in isolation.
