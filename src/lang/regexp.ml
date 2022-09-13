type t = ..
type flag = [ `i | `g | `s | `m ]
type sub = { matches : string option list; groups : (string * string) list }
type _sub = sub

module type T = sig
  type t

  val regexp : ?flags:flag list -> string -> t
  val regexp_or : ?flags:flag list -> string list -> t
  val split : ?pat:string -> ?rex:t -> string -> string list
  val exec : ?pat:string -> ?rex:t -> string -> sub
  val test : ?pat:string -> ?rex:t -> string -> bool

  val substitute :
    ?pat:string -> ?rex:t -> subst:(string -> string) -> string -> string

  val substitute_first :
    ?pat:string -> ?rex:t -> subst:(string -> string) -> string -> string
end

module DummyRegexp = struct
  let regexp ?flags:_ _ = failwith "Not implemented"
  let regexp_or ?flags:_ _ = failwith "Not implemented"
  let split ?pat:_ ?rex:_ _ = failwith "Not implemented"
  let exec ?pat:_ ?rex:_ _ = failwith "Not implemented"
  let test ?pat:_ ?rex:_ _ = failwith "Not implemented"
  let substitute ?pat:_ ?rex:_ ~subst:_ _ = failwith "Not implemented"
  let substitute_first ?pat:_ ?rex:_ ~subst:_ _ = failwith "Not implemented"
end

module type Regexp_t = T with type t := t

let regexp_ref = ref (module DummyRegexp : Regexp_t)

let regexp ?flags s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.regexp ?flags s

let regexp_or ?flags l =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.regexp_or ?flags l

let split ?pat ?rex s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.split ?pat ?rex s

let exec ?pat ?rex s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.exec ?pat ?rex s

let test ?pat ?rex s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.test ?pat ?rex s

let substitute ?pat ?rex ~subst s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.substitute ?pat ?rex ~subst s

let substitute_first ?pat ?rex ~subst s =
  let module Regexp = (val !regexp_ref : Regexp_t) in
  Regexp.substitute_first ?pat ?rex ~subst s
