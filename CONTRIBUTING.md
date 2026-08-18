# Contributing to Liquidsoap

This guide helps you navigate the Liquidsoap codebase. Whether you want to fix a bug, add a new operator, or hack on the type system, this document will point you in the right direction.

## Optional Dependencies

Liquidsoap has many optional features that are enabled when their dependencies are available. To see what's currently enabled in your build:

```bash
liquidsoap --build-config
```

This shows all optional dependencies and their status. To install optional dependencies:

```bash
# Install specific optional packages
opam install ffmpeg lame vorbis opus flac
```

Common optional packages:

- **ffmpeg** - FFmpeg integration (decoding, encoding, filters, HLS)
- **lame**, **vorbis**, **opus**, **flac** - Audio codec support
- **sdl** - Video output and visualization
- **srt** - SRT streaming protocol
- **alsa**, **pulseaudio**, **portaudio** - Audio I/O backends

## Build System

Liquidsoap uses [dune](https://dune.build/) as its build system. The project is organized into several packages defined in `dune-project`:

- **liquidsoap**: The main application with all streaming functionality
- **liquidsoap-lang**: The core language library (parser, type system, evaluator)
- **liquidsoap-js**: JavaScript bindings via js_of_ocaml

### Building

```bash
# Install dependencies
opam install . --deps-only

# Build everything
dune build

# Run tests
dune build @citest @mediatest

# Interactive mode (REPL) - useful for testing expressions
dune exec -- liquidsoap --interactive
# Or with line editing support:
ledit dune exec -- liquidsoap --interactive
```

The top-level `liquidsoap` script provides a convenient way to run liquidsoap as if it were fully installed:

```bash
# From the root of the codebase:
./liquidsoap --help
./liquidsoap script.liq

# Equivalent to running an installed liquidsoap
```

This script handles all the necessary environment setup, making it easier to test changes without a full installation.

### Project Structure

```
liquidsoap/
├── src/
│   ├── lang/           # Language implementation (liquidsoap-lang package)
│   │   ├── prelude/    # Leaf utilities: positions, string escaping, hashing
│   │   ├── types/      # The type system, depends on prelude alone
│   │   ├── data/       # JSON, documentation, method maps, runtime errors
│   │   ├── ast/        # Parsed and runtime term representations
│   │   ├── values/     # Runtime values, global environment
│   │   ├── cache/      # On-disk caches: the store, its maintenance, term cache
│   │   ├── parser/     # Lexer, grammar, token-level preprocessor
│   │   ├── reducer/    # Desugaring: parsed term -> runtime term
│   │   ├── runtime/    # Type checking, evaluation, script loading, builtins
│   │   ├── console/    # REPL/console interface
│   │   ├── stdlib/     # Thin wrapper around OCaml's stdlib for specific functions
│   │   └── tooling/    # Language tooling (LSP, formatting)
│   ├── core/           # Streaming engine (liquidsoap package)
│   │   ├── utils/      # Leaf utilities: logging, threads, processes, paths
│   │   ├── stream/     # Frames, content kinds and formats, generators
│   │   ├── media/      # Container formats, converters, decoder framework
│   │   ├── encoder/    # Encoders and their language bindings
│   │   ├── clock/      # Clocks
│   │   ├── source/     # The source class, tracks, source values
│   │   ├── request/    # URI resolution, request pool, playlist parsing
│   │   ├── runtime/    # The Lang API operators register themselves with
│   │   ├── net/        # HTTP and the harbor server
│   │   ├── protocols/  # Request protocols (annotate:, mpd:)
│   │   ├── decoders/   # Concrete decoders and metadata resolvers
│   │   ├── sources/    # Input sources
│   │   ├── outputs/    # Outputs
│   │   ├── operators/  # Operators, synth, video, visualisation
│   │   ├── builtins/   # Built-in functions exposed to scripts
│   │   └── optionals/  # Optional features (ffmpeg, alsa, etc.)
│   ├── libs/           # Standard library written in Liquidsoap
│   ├── modules/        # Vendored OCaml libraries
│   └── bin/            # Executable entry points
├── tests/              # Test suites
├── doc/                # Documentation
└── scripts/            # Build and utility scripts
```

## Core Modules

### Language (`src/lang/`)

The language implementation is split into layered dune libraries, each of which
may only depend on the ones above it in this table. The layering is enforced by
the build: `liquidsoap-lang.types` cannot reach a value or a term even by
accident.

Each library has a top-level module -- `src/lang/<dir>/liquidsoap_lang_<dir>.mli`
-- listing exactly the modules it exports. Everything else in the directory is
private to the library, so `Term_reducer` is reachable but the six
`term_reducer_*` modules behind it are not. Consumers get the exported modules
unqualified through `-open`, which is why the code reads `Term.` and `Type.`
rather than `Liquidsoap_lang_ast.Term.`. When you add a module, it stays
internal until you add it to that `.mli`.

| Library                   | Directory           | Purpose                                                                                                                     |
| ------------------------- | ------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `liquidsoap-lang.prelude` | `src/lang/prelude/` | Leaf utilities everything needs: source positions, string escaping and quoting, the hash used for caching.                  |
| `liquidsoap-lang.types`   | `src/lang/types/`   | **The type system**: representation, constraints, custom types, unification, and the printable form used in error messages. |
| `liquidsoap-lang.data`    | `src/lang/data/`    | Self-contained services: JSON, documentation, method maps, the runtime error type, build configuration.                     |
| `liquidsoap-lang.ast`     | `src/lang/ast/`     | The term representations. See `src/lang/ast/README.md` for the parsed / runtime term phases.                                |
| `liquidsoap-lang.values`  | `src/lang/values/`  | Runtime values and the global environment they are registered in.                                                           |
| `liquidsoap-lang.cache`   | `src/lang/cache/`   | The on-disk caches: the marshalling store, its maintenance, and the typechecking cache keyed on parsed terms.               |
| `liquidsoap-lang.parser`  | `src/lang/parser/`  | Lexer, Menhir grammar, and the token-level preprocessor (string interpolation, `%include`, `%ifdef`).                       |
| `liquidsoap-lang.reducer` | `src/lang/reducer/` | Desugaring: parsed term to runtime term. One module per kind of desugaring.                                                 |
| `liquidsoap-lang`         | `src/lang/runtime/` | Type checking, evaluation, script loading, the `Lang` API and the core builtins.                                            |

The files you are most likely to want:

| File                                 | Purpose                                                                                                          |
| ------------------------------------ | ---------------------------------------------------------------------------------------------------------------- |
| `parser/lexer.ml`                    | Tokenizer. Handles string interpolation, comments, and all lexical analysis.                                     |
| `parser/parser.mly`                  | Menhir grammar defining the syntax. Start here to understand how scripts are parsed.                             |
| `ast/parsed_term.ml`                 | The AST the parser produces, before desugaring. This is what the formatter and the LSP consume.                  |
| `ast/term.ml`, `ast/runtime_term.ml` | The runtime term, i.e. what actually gets typechecked and evaluated.                                             |
| `reducer/term_reducer.ml`            | Dispatcher for desugaring; the `term_reducer_*` modules next to it do the work.                                  |
| `types/type.ml`                      | Type representation: `Type.t` for all types in the language.                                                     |
| `types/typing.ml`                    | **Type inference engine**. Hindley-Milner style inference with subtyping, plus generalization and instantiation. |
| `types/repr.ml`                      | Type pretty-printing, and the rendering of type errors.                                                          |
| `runtime/typechecking.ml`            | Higher-level type checking that wraps `typing.ml`; the full program pass.                                        |
| `runtime/evaluation.ml`              | **Expression evaluator**. Executes typed terms to produce values.                                                |
| `values/value.ml`                    | Runtime value representation. All Liquidsoap values are `Value.t`.                                               |
| `values/environment.ml`              | Variable binding environments, for both type checking and evaluation.                                            |
| `cache/term_cache.ml`                | The typechecking cache: keyed on the parsed term plus the builtin environment.                                   |
| `runtime/runtime.ml`                 | Script loading and execution: `%include`, caching, and the main evaluation loop.                                 |
| `data/doc.ml`                        | Documentation extraction; generates operator documentation from type signatures and annotations.                 |
| `runtime/builtins_*.ml`              | Core built-in functions (lists, strings, math, ...) that are part of the language itself.                        |

Changes to any of these show up in `tests/snapshots/`, which records the parsed
term, its hash, the desugared term, the inferred type and the resulting value
for a corpus of scripts. See `tests/snapshots/README.md`.

### Streaming Engine (`src/core/`)

The streaming engine is where audio/video processing happens. Like `src/lang/`,
it is a stack of layered dune libraries, each one directory, each of which may
only depend on the ones above it in this table. The layering is enforced by the
build: `liquidsoap_core_utils` cannot reach a frame, and nothing below
`runtime` can register an operator.

Each library has a `src/core/<dir>/liquidsoap_core_<dir>.mli` listing exactly
what it exports; everything else in the directory is private to it. Consumers
get the exported modules unqualified through `-open`, which is why the code
reads `Frame.` and `Lang.` rather than `Liquidsoap_core_stream.Frame.`.

Operators register themselves through top-level side effects, and the modules
that do so are referenced by nothing, so every library keeps `-linkall`.

| Library                     | Directory             | Purpose                                                                                                            |
| --------------------------- | --------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `liquidsoap_core_utils`     | `src/core/utils/`     | Leaf utilities: logging, threads, processes, sockets, time, string handling, build-time paths, the config tree.    |
| `liquidsoap_core_stream`    | `src/core/stream/`    | **The media data model**: content kinds and formats, frames, generators, and the liquidsoap types describing them. |
| `liquidsoap_core_media`     | `src/core/media/`     | Container formats (AVI, WAV), sample-rate and channel-layout converters, and the `Decoder` plug registry.          |
| `liquidsoap_core_encoder`   | `src/core/encoder/`   | The encoder framework, one module per format, and the bindings behind `%mp3(...)` and friends.                     |
| `liquidsoap_core_clock`     | `src/core/clock/`     | **Timing and synchronization**. Clocks tick, and each tick asks every animated source for one frame.               |
| `liquidsoap_core_source`    | `src/core/source/`    | **The heart of Liquidsoap**: the `source` class every operator inherits from, tracks, and source values.           |
| `liquidsoap_core_request`   | `src/core/request/`   | URI resolution, the request pool, playlist parsing.                                                                |
| `liquidsoap_core_runtime`   | `src/core/runtime/`   | Core's `Lang`: everything an operator needs to register itself, plus the hooks `liquidsoap-lang` calls back into.  |
| `liquidsoap_core_net`       | `src/core/net/`       | HTTP and the harbor server that `input.harbor` and `output.harbor` are built on.                                   |
| `liquidsoap_core_protocols` | `src/core/protocols/` | Request protocols: `annotate:` and `mpd:`.                                                                         |
| `liquidsoap_core_decoders`  | `src/core/decoders/`  | Concrete decoders and metadata resolvers. Above `request/` because they register into its resolver plugs.          |
| `liquidsoap_core_sources`   | `src/core/sources/`   | Input sources: harbor input, dynamic requests, noise, test patterns.                                               |
| `liquidsoap_core_outputs`   | `src/core/outputs/`   | The output framework and the outputs built on it: icecast, harbor, HLS, pipes.                                     |
| `liquidsoap_core_operators` | `src/core/operators/` | **All audio/video operators**, plus synth and visualisation. The top of the stack.                                 |

`liquidsoap_core` itself holds no code: `src/core/liquidsoap_core.mli`
re-exports, under short names, everything builtins and optional plugins are
meant to use — including the `liquidsoap-lang` modules core republishes, so it
is the only file that spells out a `Liquidsoap_lang_*` path. Plugins do
`(libraries liquidsoap_core)` plus `-open Liquidsoap_core` and never name a
layer.

The files you are most likely to want:

| File                     | Purpose                                                                                                                                                               |
| ------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `source/source.ml`       | The `source` class: frame generation, availability, the pull-based streaming model.                                                                                   |
| `clock/clock.ml`         | Clocks and synchronization. Critical for understanding real-time behaviour.                                                                                           |
| `stream/frame.ml`        | A frame: the chunk of audio, video and metadata that flows between operators.                                                                                         |
| `stream/content_base.ml` | The content registry. `content_base`, `frame_base` and `metadata_base` exist because `Content` and `Frame` genuinely need each other's types; do not merge them back. |
| `request/request.ml`     | URIs, resolvers and the request queue.                                                                                                                                |
| `source/track.ml`        | Individual audio/video/subtitle tracks within a source.                                                                                                               |

### Built-in Functions (`src/core/builtins/`)

Functions exposed to Liquidsoap scripts that need access to the streaming engine.

| File                   | Purpose                                                               |
| ---------------------- | --------------------------------------------------------------------- |
| `builtins_source.ml`   | Source-related functions: `source.tracks`, `source.on_metadata`, etc. |
| `builtins_track.ml`    | Track operators: `track.audio.amplify`, `track.video.add`, etc.       |
| `builtins_request.ml`  | Request handling: `request.create`, `request.resolve`, etc.           |
| `builtins_clock.ml`    | Clock management: `clock.assign_new`, `clock.unify`, etc.             |
| `builtins_settings.ml` | Configuration settings exposed to scripts.                            |
| `builtins_server.ml`   | Telnet/socket server for runtime control.                             |
| `builtins_harbor.ml`   | HTTP server (harbor) for input streams and web interfaces.            |

### Script Library (`src/libs/`)

The standard library written in Liquidsoap itself. These `.liq` files provide high-level functions built on top of the core operators.

| File           | Purpose                                                        |
| -------------- | -------------------------------------------------------------- |
| `audio.liq`    | Audio processing helpers: `amplify`, `clip`, `normalize`, etc. |
| `video.liq`    | Video processing: scaling, effects, text overlay.              |
| `source.liq`   | Source utilities: `fallback`, `switch`, `rotate`, etc.         |
| `playlist.liq` | Playlist handling: `playlist`, `playlist.safe`, etc.           |
| `fades.liq`    | Fade in/out and crossfade implementations.                     |
| `hls.liq`      | HLS output helpers.                                            |
| `icecast.liq`  | Icecast output helpers.                                        |
| `ffmpeg.liq`   | FFmpeg integration helpers.                                    |
| `request.liq`  | Request utilities and protocols.                               |

### Optional Features (`src/core/optionals/`)

Each subdirectory adds support for an optional dependency (ffmpeg, alsa, pulseaudio, etc.). These are compiled conditionally based on available libraries.

| Directory                            | Purpose                                                                                  |
| ------------------------------------ | ---------------------------------------------------------------------------------------- |
| `ffmpeg/`                            | FFmpeg integration: decoding, encoding, filters, HLS, RTMP. The largest optional module. |
| `alsa/`, `pulseaudio/`, `portaudio/` | Audio I/O backends.                                                                      |
| `sdl/`                               | SDL video output and visualization.                                                      |
| `srt/`                               | SRT streaming protocol.                                                                  |
| `ogg/`, `vorbis/`, `opus/`, `flac/`  | Audio codec support.                                                                     |
| `ladspa/`, `lilv/`                   | Audio plugin frameworks.                                                                 |

## Common Tasks

### "I want to hack on the type system"

Everything except the final pass lives in `src/lang/types/`, which depends on
nothing but `src/lang/prelude/`:

1. **`types/type.ml`** - Understand how types are represented
2. **`types/typing.ml`** - The main inference algorithm, including unification
3. **`types/repr.ml`** - How types and type errors are rendered
4. **`runtime/typechecking.ml`** - Full program type checking

Key concepts:

- Types use a unification-based inference similar to Hindley-Milner
- Subtyping is used extensively (e.g., `{foo: int, bar: string}` is a subtype of `{foo: int}`)
- Type variables are represented with mutable references for efficient unification
- See `src/lang/ast/README.md` for details on term representation phases (parsed → typed → runtime)

To test type system changes:

```bash
dune build @citest
```

### "I want to add a new audio operator"

1. **Create the operator** in `src/core/operators/my_operator.ml`:

```ocaml
open Source

class my_operator (source : source) param =
  object
    inherit operator ~name:"my_operator" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private generate_frame =
      let frame = source#get_frame in
      (* Process the frame here *)
      frame
  end

let _ =
  Lang.add_operator "my_operator"
    [
      ("param", Lang.float_t, Some (Lang.float 1.0), Some "My parameter");
      ("", Lang.source_t, None, None);
    ]
    ~return_t:Lang.source_t
    ~category:`Audio
    ~descr:"My new operator"
    (fun p ->
      let param = Lang.to_float (List.assoc "param" p) in
      let source = Lang.to_source (List.assoc "" p) in
      new my_operator source param)
```

2. **Add it to `src/core/operators/liquidsoap_core_operators.ml` and `.mli`**,
   which list what the library exports. Nothing outside the library can see a
   module that is missing from them.

3. **Add a high-level wrapper** in `src/libs/audio.liq` if appropriate:

```liquidsoap
# My new operator
# @category Source / Audio processing
# @param s Source to process
def my_operator(~param=1.0, s)
  my_operator(param=param, s)
end
```

4. **Add tests** in `tests/` directory

### "I want to contribute to the scripting library"

The script library lives in `src/libs/*.liq`. These are Liquidsoap scripts that ship with the application.

1. **Find the right file** - Functions are organized by domain (audio.liq, video.liq, playlist.liq, etc.)

2. **Follow the documentation format**:

```liquidsoap
# Brief description of what the function does.
# @category Category / Subcategory
# @param ~named_param Description of named parameter
# @param positional Description of positional parameter
def my_function(~named_param=default, positional)
  # Implementation
end
```

3. **Test interactively**:

```bash
dune exec -- liquidsoap -c 'my_function(...)'
```

4. **Run the test suite**:

```bash
dune build @citest @mediatest
```

### "I want to work on the documentation"

Pages live in `doc/content/*.md` and are published at
[liquidsoap.info](https://www.liquidsoap.info) by
[savonet/website](https://github.com/savonet/website). There is no rendered copy in this
repository: the markdown is the source, and the site is built from it.

Preview your changes against the real site:

```bash
dune build @doc/serve-website
```

That builds the reference, fetches the site, and serves it on
<http://localhost:3000>. Editing a page under `doc/content` re-syncs and reloads it in the
browser. Set `LIQUIDSOAP_WEBSITE` to a checkout of savonet/website to work on the site
itself at the same time.

A few things to know when writing:

- **Link pages as `.md`**: `[clocks](./clocks.md)`, not `clocks.html`. That resolves on
  GitHub, in an editor and on the website, and a typo shows up as a missing file rather
  than as a 404 nobody notices.
- **Code samples live in `doc/content/liq/`** and are included rather than pasted:

  ````markdown
  ```{.liquidsoap include="my-example.liq"}

  ```
  ````

  Every snippet is run through `liquidsoap --check` by `dune build @doctest`, so an
  example that stops compiling fails the build.

- **`reference.md`, `reference-extras.md`, `reference-deprecated.md`, `protocols.md` and
  `settings.md` are generated** by the binary from the `@category` and `@param` comments
  in `src/libs/*.liq` and the OCaml sources. Do not edit them; change the documentation
  comment instead.

### "I want to add an FFmpeg feature"

FFmpeg integration is in `src/core/optionals/ffmpeg/`. Key files:

| File                     | Purpose                               |
| ------------------------ | ------------------------------------- |
| `ffmpeg_decoder.ml`      | Decoding media files with FFmpeg      |
| `ffmpeg_encoder.ml`      | Encoding output with FFmpeg           |
| `ffmpeg_filter.ml`       | FFmpeg filter graph integration       |
| `ffmpeg_copy_content.ml` | Passthrough/remuxing without decoding |
| `lang_ffmpeg.ml`         | FFmpeg operators exposed to scripts   |

## Testing

Tests are split into two main categories:

```bash
# CI tests (language, typing, OCaml unit tests, most functionality)
dune build @citest

# Media tests (encoding, decoding, ffmpeg features)
dune build @mediatest

# Run a specific test by name (without extension)
dune build @my_test_name
```

Test locations:

- `tests/language/` - Language and typing tests (Liquidsoap scripts)
- `tests/media/` - Media encoding/decoding tests
- `tests/core/` - OCaml unit tests

## Code Style

- OCaml code is formatted with `ocamlformat` (config in `.ocamlformat`)
- Liquidsoap code should follow existing conventions in `src/libs/`
- Use descriptive names and add documentation comments

## Backporting and Rolling Releases

Liquidsoap maintains stable release branches using the following workflow:

### Branch and Tag Structure

- **Development branch**: `main`
- **Stable branches**: `v2.4.x-latest`, `v2.3.x-latest`, etc.
- **Rolling release tags**: `rolling-release-v2.5.x`, `rolling-release-v2.4.x`, etc.

Which branches publish a rolling release, and under which version, is configured in
[`.github/release-matrix.json`](.github/release-matrix.json). `main` is listed there as the
in-development line, so it rolls alongside the stable branches. That file also generates the
release table in `README.md`.

### Backporting PRs

To backport a PR from `main` to a stable branch:

1. Add a label like `backport:v2.4.x-latest` to the PR before merging
2. When the PR is merged, the backport workflow automatically:
   - Cherry-picks the changes to the target branch
   - Creates a new PR for the backport

### Rolling Release Tags

When CI passes on a branch listed in the release matrix (e.g., `main` or `v2.4.x-latest`), the corresponding rolling release tag (`rolling-release-v2.5.x`, `rolling-release-v2.4.x`) is automatically created/updated, its release assets are replaced, and Docker images are published under the same name. The tag always points to the latest CI-validated commit on that branch.

## Getting Help

- GitHub Issues: https://github.com/savonet/liquidsoap/issues
- Discussion: https://github.com/savonet/liquidsoap/discussions
- Discord: https://discord.gg/savonet
