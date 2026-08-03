(** The term representations, see README.md: the parsed term the parser produces
    and the runtime term everything downstream works on. *)

(** The AST skeleton shared by both representations. *)
module Runtime_term = Runtime_term

(** The runtime term: what gets typechecked and evaluated. *)
module Term = Term

(** The term the parser produces, before desugaring. This is what the formatter
    and the LSP consume. *)
module Parsed_term = Parsed_term

(** Strip the parts of a term that need not be cached. *)
module Term_trim = Term_trim
