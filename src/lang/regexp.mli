(* Operational module: *)

type flag = [ `i | `g | `s | `m ]
type sub = { matches : string option list; groups : (string * string) list }

module type T = sig
  type t

  val regexp : ?flags:flag list -> string -> t
  val split : t -> string -> string list
  val exec : t -> string -> sub
  val test : t -> string -> bool
  val substitute : t -> subst:(string -> string) -> string -> string
end

include T

(* Implementation, filled by language user: *)

type regexp =
  < split : string -> string list
  ; exec : string -> sub
  ; test : string -> bool
  ; substitute : subst:(string -> string) -> string -> string >

val regexp_ref : (?flags:flag list -> string -> regexp) ref
