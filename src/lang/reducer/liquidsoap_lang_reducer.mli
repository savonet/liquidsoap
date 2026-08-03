(** Desugaring: parsed term to runtime term.

    The reducers for each construct live in the term_reducer_* modules, which
    are internal: everything goes through {!Term_reducer}. *)

(** Reduce a parsed term to a runtime term. *)
module Term_reducer = Term_reducer
