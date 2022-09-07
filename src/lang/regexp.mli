type sub = ..
type t = ..
type flag = [ `i | `g | `s | `m ]

module type T = sig
  type t

  val regexp : ?flags:flag list -> string -> t
  val regexp_or : ?flags:flag list -> string list -> t
  val split : ?pat:string -> ?rex:t -> string -> string list
  val exec : ?pat:string -> ?rex:t -> string -> sub
  val test : ?pat:string -> ?rex:t -> string -> bool
  val num_of_subs : sub -> int
  val get_substring : sub -> int -> string

  val substitute :
    ?pat:string -> ?rex:t -> subst:(string -> string) -> string -> string

  val substitute_first :
    ?pat:string -> ?rex:t -> subst:(string -> string) -> string -> string
end

module type Regexp_t = T with type t := t

val regexp_ref : (module Regexp_t) ref

include T with type t := t
