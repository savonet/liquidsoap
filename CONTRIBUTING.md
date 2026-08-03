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
│   │   ├── base/       # Core streaming: sources, operators, clocks
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

### Streaming Engine (`src/core/base/`)

The streaming engine is where audio/video processing happens.

| File                      | Purpose                                                                                                                                                                            |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `source.ml`               | **The heart of Liquidsoap**. Defines the `source` class that all audio/video sources inherit from. Handles frame generation, availability, and the pull-based streaming model.     |
| `clock.ml`                | **Timing and synchronization**. Manages clocks that drive sources. Each clock ticks independently, pulling frames from its sources. Critical for understanding real-time behavior. |
| `frame.ml` (in `stream/`) | Frame representation. A frame is a chunk of multimedia data (audio samples, video frames, metadata) that flows between operators.                                                  |
| `request.ml`              | Media request handling. Manages URIs, resolvers, and the request queue for loading media files.                                                                                    |
| `track.ml`                | Track abstraction for handling individual audio/video/subtitle tracks within a source.                                                                                             |
| `operators/`              | **All audio/video operators**. Each file implements one or more operators (amplify, compress, crossfade, etc.).                                                                    |
| `sources/`                | Input sources (playlists, single files, requests, silence, etc.).                                                                                                                  |
| `outputs/`                | Output destinations (files, icecast, HLS, etc.).                                                                                                                                   |
| `decoder/`                | Media decoders. Interfaces with ffmpeg, mad, flac, etc. to decode audio/video files.                                                                                               |
| `encoder/`                | Media encoders. Interfaces with ffmpeg, lame, opus, etc. to encode streams.                                                                                                        |
| `conversions/`            | Format conversion between different audio/video representations.                                                                                                                   |
| `tools/`                  | Utility functions: JSON handling, HTTP, process management, etc.                                                                                                                   |

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

1. **Create the operator** in `src/core/base/operators/my_operator.ml`:

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

2. **Add to dune** in `src/core/base/dune` (it's auto-discovered, but check if needed)

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
- **Rolling release tags**: `rolling-release-v2.4.x`, `rolling-release-v2.3.x`, etc.

### Backporting PRs

To backport a PR from `main` to a stable branch:

1. Add a label like `backport:v2.4.x-latest` to the PR before merging
2. When the PR is merged, the backport workflow automatically:
   - Cherry-picks the changes to the target branch
   - Creates a new PR for the backport

### Rolling Release Tags

When CI passes on a stable branch (e.g., `v2.4.x-latest`), a corresponding rolling release tag (`rolling-release-v2.4.x`) is automatically created/updated. This tag always points to the latest CI-validated commit on that branch.

## Getting Help

- GitHub Issues: https://github.com/savonet/liquidsoap/issues
- Discussion: https://github.com/savonet/liquidsoap/discussions
- Discord: https://discord.gg/savonet
