module type T = sig
  type t

  val implementation : string
  val time : unit -> t
  val sleep_until : t -> unit
  val of_float : float -> t
  val to_float : t -> float
  val ( |+| ) : t -> t -> t
  val ( |-| ) : t -> t -> t
  val ( |*| ) : t -> t -> t
  val ( |<| ) : t -> t -> bool
  val ( |<=| ) : t -> t -> bool
end

module Unix = struct
  type t = float

  let implementation = "builtin (low-precision)"
  let time = Unix.gettimeofday
  let of_float x = x
  let to_float x = x
  let ( |+| ) x y = x +. y
  let ( |-| ) x y = x -. y
  let ( |*| ) x y = x *. y
  let ( |<| ) x y = x < y
  let ( |<=| ) x y = x <= y

  let rec sleep_until t =
    let delay = t -. time () in
    if 0. < delay then (
      try Thread.delay delay
      with Unix.Unix_error (Unix.EINTR, _, _) -> sleep_until t)
end

type implementation = (module T)

let unix : implementation = (module Unix)
let implementations : (string, implementation) Hashtbl.t = Hashtbl.create 2
let () = Hashtbl.add implementations "ocaml" unix
