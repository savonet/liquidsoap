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

type t =
  < split : string -> string list
  ; exec : string -> sub
  ; test : string -> bool
  ; substitute : subst:(string -> string) -> string -> string >

type regexp = t

let dummy_regexp ?flags:_ _ =
  object
    method split _ = assert false
    method exec _ = assert false
    method test _ = assert false
    method substitute ~subst:_ _ = assert false
  end

let regexp_ref = ref dummy_regexp

let regexp ?flags s =
  let fn = !regexp_ref in
  fn ?flags s

let split rex = rex#split
let exec rex = rex#exec
let test rex = rex#test
let substitute rex = rex#substitute
