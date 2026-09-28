# Developer tools

A Liquidsoap script is a program. A typo in it can stop your radio, and the earlier you catch the typo, the better. Liquidsoap and its companion tools help you at every step: while you type, before you deploy, and in your continuous integration. This page presents them.

## Checking a script

You want to know that a script is valid before you put it on air. `liquidsoap --check` parses and typechecks a script, then exits before starting any output. You can run it anywhere, even on a machine without your music or your sound card:

```sh
liquidsoap --check radio.liq
```

When the script has an error, Liquidsoap tells you where it is and what went wrong, and exits with a non-zero code:

```
At radio.liq, line 2, char 41-47:
output.icecast(%mp3, mount="radio", port="8000", mksafe(music))

Error 5: this value has type
  string
but it should be a subtype of
  int
```

Here, the `port` of `output.icecast` is a number, so it should be written `port=8000`.

`--check` only typechecks the script. Some problems are detected when the script runs, for instance an output whose source is [fallible](./quick_start.md#that-source-is-fallible) or a file that cannot be decoded.

Liquidsoap also reports warnings: an unused variable, a source that is not connected to an output, or a value that is ignored. The script still runs when it has warnings:

```
At radio.liq, line 2, char 2-7:
  y = 3

Warning 4: Unused variable y
```

To treat warnings as errors, add `--strict`. The check then fails on the first warning, which is useful in your continuous integration:

```sh
liquidsoap --strict --check radio.liq
```

`--check` always typechecks the script, so it reports every warning. To fill the script cache ahead of time, use `--cache-only`. See [how scripts run](./script_lifecycle.md#caching) for the details.

## Finding documentation

Every operator, function and setting comes with its documentation. To read the documentation of an operator, with the type and the default value of each of its arguments, run:

```sh
liquidsoap -h output.icecast
```

The [help page](./help.md) explains how to search for operators, settings and server commands. The same documentation is available online in the [API reference](./reference.md).

## Trying things out

Sometimes you want to try a small piece of code: check what a function returns, or see how a string is formatted. Liquidsoap has an interactive mode for this. Start it with:

```sh
liquidsoap --interactive
```

Type an expression, end it with `;;`, and Liquidsoap shows its value and its type.

You can also try Liquidsoap in your browser, without installing anything, in the [playground](https://www.liquidsoap.info/try). The playground runs the Liquidsoap interpreter in your browser.

## Editor support

A good editor shows you errors as you type, completes the names of operators and shows their documentation. The Liquidsoap language server brings all of this to your editor.

### The language server

The language server speaks the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/), so it works with any editor that has an LSP client. It provides:

- **Errors and warnings** as you type: syntax errors, type errors and unused variables. The server runs Liquidsoap's own parser and typechecker, so it reports the same errors as `liquidsoap --check`, without running your script. An error inside a file you `%include` shows on the `%include` line.
- **Hover**: the documentation of a standard library function, or the type of the expression under the cursor.
- **Completion**: the names in scope, and the methods of a value after a `.`.
- **Signature help**: the arguments of a function while you type them.
- **Go to definition** for the names your script defines, including in files you `%include`.
- **Document outline**: the definitions of your script.
- **Formatting** with [liquidsoap-prettier](#formatting).

A script you are editing is usually not valid at every keystroke. The server tries to replace the invalid parts of the script with a universal placeholder, so the rest of the script keeps its errors, types and completions.

The server needs [Node.js](https://nodejs.org) 22 or later. Install it from npm:

```sh
npm install -g liquidsoap-language-server
```

This installs the `liquidsoap-language-server` command. Your editor starts it as `liquidsoap-language-server --stdio`.

When `liquidsoap` is installed, the server asks it for its standard library, so the server knows exactly the operators you have, LV2 and LADSPA plugins included. The server reads it once per Liquidsoap version and keeps it under `~/.cache/liquidsoap-language-server/`. Set the `LIQUIDSOAP` environment variable to the path of another `liquidsoap` binary to use that one. When no `liquidsoap` is found, the server uses the standard library it ships with.

### Visual Studio Code

Install the [Liquidsoap extension](https://marketplace.visualstudio.com/items?itemName=savonet.vscode-liquidsoap) from the Marketplace. The extension ships the language server, so you only need Node.js. The extension adds syntax highlighting, errors in the editor and in the Problems panel, and the usual commands: F12 goes to a definition, Ctrl+Space completes, and Shift+Alt+F formats the script.

The extension has two settings:

- `liquidsoap.languageServer.enabled`: check scripts with the language server. With this setting off, the extension only highlights and formats.
- `liquidsoap.path`: the `liquidsoap` binary to take the standard library from.

In the web version of Visual Studio Code, the extension provides highlighting and formatting only, since the language server needs Node.js.

### Neovim

Neovim 0.11 and later configure language servers natively. Add this to your `init.lua`:

```lua
vim.filetype.add({ extension = { liq = "liquidsoap" } })

vim.lsp.config("liquidsoap", {
  cmd = { "liquidsoap-language-server", "--stdio" },
  filetypes = { "liquidsoap" },
  root_markers = { ".git" },
})
vim.lsp.enable("liquidsoap")
```

Open a `.liq` file, and `:checkhealth vim.lsp` lists the `liquidsoap` client. With Neovim's default mappings, `]d` and `[d` go to the next and previous error, `K` shows the documentation under the cursor, `CTRL-]` goes to a definition, `gO` lists the definitions of the script, and `:lua vim.lsp.buf.format()` formats it.

For syntax highlighting, install the `liquidsoap` parser of [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) with `:TSInstall liquidsoap`.

### Helix

Add this to `~/.config/helix/languages.toml`:

```toml
[language-server.liquidsoap]
command = "liquidsoap-language-server"
args = ["--stdio"]

[[language]]
name = "liquidsoap"
scope = "source.liquidsoap"
file-types = ["liq"]
comment-token = "#"
indent = { tab-width = 2, unit = "  " }
language-servers = ["liquidsoap"]
```

`hx --health liquidsoap` checks that Helix finds the server. `space k` shows the documentation, `g d` goes to a definition, `space s` lists the definitions of the script, and `:format` formats it.

### Emacs

Liquidsoap has an Emacs mode, `liquidsoap-mode`. Install it with opam:

```sh
opam install liquidsoap-mode
```

You can also copy [`scripts/liquidsoap-mode.el`](https://github.com/savonet/liquidsoap/blob/main/scripts/liquidsoap-mode.el) from the Liquidsoap repository. Emacs 29 and later come with the Eglot LSP client. Add this to your init file:

```elisp
;; Where opam installs the mode; use your own directory if you copied it.
(add-to-list 'load-path
             (expand-file-name "share/emacs/site-lisp"
                               (string-trim (shell-command-to-string "opam var prefix"))))
(require 'liquidsoap-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(liquidsoap-mode . ("liquidsoap-language-server" "--stdio"))))
(add-hook 'liquidsoap-mode-hook #'eglot-ensure)
```

Errors show with Flymake, `M-.` goes to a definition, `C-M-i` completes, and `M-x eglot-format-buffer` formats the script.

### Other editors

Any LSP client can start the language server. Configure your client to run the following command for files with the `.liq` extension:

```sh
liquidsoap-language-server --stdio
```

If you build your own tools, Liquidsoap grammars are also available for [tree-sitter](https://github.com/savonet/tree-sitter-liquidsoap) and for the [CodeMirror](https://github.com/savonet/codemirror-lang-liquidsoap) web editor.

## Formatting

A consistent layout makes a script easier to read and makes changes easier to review. [liquidsoap-prettier](https://github.com/savonet/liquidsoap-prettier) formats Liquidsoap scripts, based on the [Prettier](https://prettier.io/) code formatter. The language server and the Visual Studio Code extension use it when you format a script.

Install it from npm:

```sh
npm install -g liquidsoap-prettier
```

To format files in place:

```sh
liquidsoap-prettier -w radio.liq "scripts/**/*.liq"
```

To check that files are formatted, for instance in your continuous integration, use `-c`. The command exits with code `0` when all files are formatted, and `2` otherwise:

```sh
liquidsoap-prettier -c "scripts/**/*.liq"
```

If your project already uses Prettier, add liquidsoap-prettier as a plugin. Install both packages:

```sh
npm install -D prettier liquidsoap-prettier
```

Then add the plugin to your `.prettierrc`:

```json
{
  "plugins": ["liquidsoap-prettier"]
}
```

The language server also reads the `.prettierrc` next to your script.

## Continuous integration

When your scripts live in a git repository, you can check every change before it reaches your server. A typical check formats and typechecks all scripts:

```sh
liquidsoap-prettier -c "**/*.liq"
liquidsoap --strict --check radio.liq
```

To format scripts on every commit, use the [pre-commit](https://pre-commit.com/) hook. Add this to your `.pre-commit-config.yaml`, with the tag you want in `rev`:

```yaml
- repo: https://github.com/savonet/pre-commit-liquidsoap
  rev: ""
  hooks:
    - id: liquidsoap-prettier
```

If you deploy with Docker, you can also typecheck your script while building the image, which fills the cache and makes the container start faster. See [caching in production](./script_lifecycle.md#caching-in-production-and-docker-images).
