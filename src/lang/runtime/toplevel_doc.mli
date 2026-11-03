(** The documentation of a toplevel definition: [Evaluation.toplevel_add] calls
    [fill] on the documentation parsed from its comment, which carries the
    descriptions, and the value's type and value supply the types, default
    values and methods. *)

(** The fields of a record argument of type [t], listed once one of them has a
    description: from [@field] in [documented], else from the type. *)
val fields :
  generalized:Type.var list ->
  (string * Doc.Value.meth) list ->
  Type.t ->
  (string * Doc.Value.meth) list

val fill :
  generalized:Type.var list -> Type.t -> Value.t -> Doc.Value.t -> Doc.Value.t
