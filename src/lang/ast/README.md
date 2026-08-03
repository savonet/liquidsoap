# Term Representation Phases

Terms go through three representations as they flow through the compiler:

## 1. Parsed Terms (`parsed_term.ml`)

The full AST produced by the parser. Contains all syntactic details needed for tooling:

```ocaml
type t = {
  term : ast;
  pos : Pos.t;              (* Source location *)
  mutable comments : ...;   (* Attached comments *)
}
```

Includes all syntax: `_if`, `_while`, `_for`, `_let`, pattern matching, method invocations, etc. Used by:

- **Formatter** - to preserve comments and layout
- **LSP** - for hover, completion, go-to-definition
- **Error messages** - accurate source positions

## 2. Typed Terms (`term_base.ml`)

After type checking, terms carry their inferred types:

```ocaml
type 'a term = {
  t : Type.t;           (* The inferred type *)
  term : 'a;            (* The AST node *)
  methods : 'a term Methods.t;
  flags : Flags.flags;
}
```

Syntactic sugar has been desugared at this stage. For example, `if cond then a else b` becomes a function application. The `t` field contains the type computed by the type checker.

## 3. Runtime Terms (`runtime_term.ml`)

Minimal AST for evaluation:

```ocaml
type 'a runtime_ast =
  [ `Int of int | `Float of float | `String of string | `Bool of bool
  | `Let of 'a let_t | `List of 'a list | `App of 'a * (string * 'a) list
  | `Fun of ('a, Type.t) func | `Invoke of 'a invoke | ... ]
```

Only contains constructs needed by the evaluator. No syntactic sugar, no comments, no formatting information. This is what `evaluation.ml` executes.

## Flow Summary

```
Source Code
    | (lexer.ml, parser.mly)
    v
Parsed Term  <-- used by formatter, LSP
    | (typechecking.ml)
    v
Typed Term   <-- type errors reported here
    | (already desugared during type checking)
    v
Runtime Term <-- executed by evaluation.ml
    |
    v
Value        <-- runtime result (value.ml)
```
