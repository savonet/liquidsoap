type state

external create : unit -> state = "liquidsoap_xxh3_create"

external unsafe_update : state -> string -> int -> int -> unit
  = "liquidsoap_xxh3_update"

external digest : state -> int64 = "liquidsoap_xxh3_digest"

let update state s offset length =
  if offset < 0 || length < 0 || offset > String.length s - length then
    invalid_arg "Xxh.update";
  unsafe_update state s offset length
