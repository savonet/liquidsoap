(** The front end: lexer, grammar and the token-level preprocessor that handles
    string interpolation, %include and %ifdef. *)

(** The generated grammar. *)
module Parser = Parser

(** Term constructors used by the grammar's actions. *)
module Parser_helper = Parser_helper

(** The tokenizer. *)
module Lexer = Lexer

(** Token-level preprocessing: string interpolation and conditionals. *)
module Preprocessor = Preprocessor

(** Parsed-term level preprocessing: %include expansion. *)
module Term_preprocessor = Term_preprocessor
