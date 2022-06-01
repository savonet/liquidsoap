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

module DummyRegexp = struct
  type t
  type sub

  let regexp _ = failwith "Not implemented"
  let regexp_or _ = failwith "Not implemented"
  let split ~pat:_ _ = failwith "Not implemented"
  let exec ?pat:_ ?rex:_ _ = failwith "Not implemented"
  let get_substring _ _ = failwith "Not implemented"
  let substitute ?pat:_ ?rex:_ ~subst:_ _ = failwith "Not implemented"
end

let regexp_ref = ref (module DummyRegexp : T)

type t
type sub

let regexp s =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.regexp s)

let regexp_or l =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.regexp_or l)

let split ~pat s =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.split ~pat s)

let exec ?pat ?rex s =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.exec ?pat ?rex:(Obj.magic rex) s)

let get_substring sub pos =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.get_substring (Obj.magic sub) pos)

let substitute ?pat ?rex ~subst s =
  let module Regexp = (val !regexp_ref : T) in
  Obj.magic (Regexp.substitute ?pat ?rex:(Obj.magic rex) ~subst s)
