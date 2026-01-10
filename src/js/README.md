# Liquidsoap JavaScript Playground

This directory contains the JavaScript build of Liquidsoap, used to power the online playground at https://www.liquidsoap.info/try/

## Building

### Basic build (requires js_of_ocaml)

```bash
dune build src/js
```

This builds:

- `interactive_js.bc.js` - The Liquidsoap interpreter compiled to JavaScript
- `index.html` - The playground HTML page
- `playground.bundle.js` - Bundled CodeMirror editor and dependencies (requires npm)

### With full playground support (requires npm)

If `npm` is available in your PATH, dune will automatically bundle all the playground dependencies (CodeMirror, syntax highlighting, etc.) into `playground.bundle.js`.

Without npm, a stub file is created that shows an error message in the browser console.

## Testing locally

Start a local webserver to test the playground:

```bash
dune build @src/js/serve-playground
```

This builds all necessary files and starts a Python HTTP server on port 8080. Open http://localhost:8080 in your browser.

## Files

| File                            | Description                              |
| ------------------------------- | ---------------------------------------- |
| `interactive_js.ml`             | OCaml source for the JS interpreter      |
| `index.html`                    | Playground HTML page                     |
| `runtime.js`                    | JavaScript runtime stubs                 |
| `stdlib_js.liq`                 | JavaScript-specific stdlib includes      |
| `.scripts/bundle-playground.sh` | Script to bundle playground dependencies |
| `.scripts/serve-playground.sh`  | Script to start local test server        |

## Architecture

```
┌─────────────────────────────────────┐
│  Browser                            │
├─────────────────────────────────────┤
│  index.html                         │
│    ├── playground.bundle.js         │
│    │   (CodeMirror + themes +       │
│    │    syntax highlighting)        │
│    └── interactive_js.bc.js         │
│        (Liquidsoap interpreter)     │
└─────────────────────────────────────┘
```

The playground consists of two main JavaScript components:

1. **playground.bundle.js** - Contains the CodeMirror 6 editor with:
   - Liquidsoap syntax highlighting (`codemirror-lang-liquidsoap`)
   - Code formatting (`liquidsoap-prettier`)
   - Multiple color themes

2. **interactive_js.bc.js** - The Liquidsoap interpreter compiled from OCaml using js_of_ocaml, including:
   - Parser and type checker
   - Evaluator
   - Standard library (embedded via virtual filesystem)

## CI/CD

The playground is built and deployed automatically via GitHub Actions:

1. `.github/workflows/js.yml` builds the JS artifacts
2. `.github/workflows/ci.yml` (`update_doc` job) deploys to https://www.liquidsoap.info/try/

## Troubleshooting

### "npm not available" error

Install Node.js/npm and ensure it's in your PATH, then rebuild:

```bash
dune clean
dune build src/js
```

### CodeMirror errors in browser

If you see errors like "multiple instances of @codemirror/state", make sure you're using the bundled `playground.bundle.js` rather than loading packages from CDN. The bundle ensures all CodeMirror dependencies use compatible versions.
