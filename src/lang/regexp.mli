module type T = sig
  type t
  type sub

  val regexp : string -> t
  val regexp_or : string list -> t
  val split : pat:string -> string -> string list
  val exec : ?pat:string -> ?rex:t -> string -> sub
  val get_substring : sub -> int -> string

  val substitute :
    ?pat:string -> ?rex:t -> subst:(string -> string) -> string -> string
end

include T

val regexp_ref : (module T) ref
