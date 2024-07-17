
(** A tiny language for describing time intervals. *)

type expr

val from_string : string -> expr
val to_string : expr -> string
val satisfied : expr -> bool
val satisfied_with_offset : expr -> int -> bool
val never : unit -> expr
val always : unit -> expr

(** When typing, one must be able to compute the disjunction of two expressions,
  * and check that two expressions do not intersect.
  *
  * In order to do that, we reduce every [expr] to a normal form,
  * of the type [cexpr], and work then on it. *)

type cexpr

val compile : expr -> cexpr
val conjunction : cexpr -> cexpr -> cexpr
val disjunction : cexpr -> cexpr -> cexpr
val intersect : cexpr -> cexpr -> bool
val cexpr_to_string : cexpr -> string
val compiled_never : unit -> cexpr
val compiled_always : unit -> cexpr
