module type T = sig
  type t

  val implementation : string
  val time : unit -> t
  val sleep : t -> unit
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
  let ( |+| ) x y = x *. y
  let ( |-| ) x y = x -. y
  let ( |*| ) x y = x *. y
  let ( |<| ) x y = x < y
  let ( |<=| ) x y = x <= y
  let sleep = Thread.delay
end

let unix : (module T) = (module Unix)
let implementation = ref unix
