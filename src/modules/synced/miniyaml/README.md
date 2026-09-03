# MiniYaml — A simple YAML parser in pure OCaml

> [!WARNING]
> This repository is read-only. All changes must be made in
> [savonet/liquidsoap](https://github.com/savonet/liquidsoap) under
> `src/modules/synced/miniyaml/` and will be mirrored here automatically.

MiniYaml reads and writes the subset of YAML that people actually hand-write in
configuration files: block mappings and sequences, inline flow collections, quoted
scalars and comments.

It is written in pure OCaml and has **no dependencies** — not even a lexer or parser generator. The whole library is a single module of about 650 lines.

## Installation

```sh
opam install miniyaml
```

The opam package and the dune library are named `miniyaml`, the module is `Yaml`:

```
(executable
 (name my_program)
 (libraries miniyaml))
```

## Usage

```ocaml
let () =
  match
    Yaml.of_string
      {|
name: miniyaml
tags: [yaml, parser]
server:
  host: localhost
  port: 8080
|}
  with
  | Ok v -> print_string (Yaml.to_string v)
  | Error e -> prerr_endline e
```

which prints

```yaml
name: miniyaml
tags:
  - yaml
  - parser
server:
  host: localhost
  port: 8080
```

Values are built and inspected directly:

```ocaml
Yaml.to_string (Assoc [ "ports", List [ Float 80.; Float 443. ] ])
(* "ports:\n  - 80\n  - 443\n" *)
```

## API

```ocaml
type t =
  | Null
  | Bool of bool
  | Float of float          (** any number, integers included *)
  | String of string
  | List of t list
  | Assoc of (string * t) list

val of_string : string -> (t, string) result
val to_string : t -> string
```

`Assoc` entries keep the order in which they occur, and duplicate keys are preserved.
Errors are reported with the line at which they were detected, for instance
`line 3: unexpected content`.

Printing and parsing are inverse to each other: `of_string (to_string v) = Ok v` holds
for **every** value `v`. Strings that would otherwise be read back as something else are
quoted on output, so `String "true"`, `String "1.5"` and `String ""` survive the trip.

## Supported subset

- block mappings (`key: value`) and sequences (`- item`), nested by indentation,
  including sequences written at the indentation of their key and entries written
  compactly after a dash (`- name: alice` with further entries below);
- flow collections, `[1, 2]` and `{a: b}`, nested and mixed with block style;
- plain, single-quoted (`''` escapes a quote) and double-quoted scalars, the latter with
  the usual escapes (`\n`, `\t`, `\uXXXX`, ...);
- comments, and the `---` / `...` document markers;
- the plain scalars `~`, `null`, `true`, `false`, `.inf`, `.nan` and decimal numbers.

The following are _not_ supported, and are rejected with an explicit error rather than misparsed: block scalars (`|` and `>`), anchors and aliases (`&`, `*`), tags (`!`), directives (`%`), complex mapping keys (`?`), multi-document streams, and tabulations used for indentation. Flow collections have to fit on a single line.

## Building

```sh
dune build
dune runtest
```

## Tool disclosure

Most of the project was generated with Claude code (Opus 5).

## License

MiniYaml is distributed under the GNU General Public License version 3, see
[LICENSE](LICENSE).
