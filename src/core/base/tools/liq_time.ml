type t

module type T = sig
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
  let implementation = "builtin (low-precision)"
  let of_time : t -> float = Obj.magic
  let to_time : float -> t = Obj.magic
  let time () = to_time (Unix.gettimeofday ())
  let of_float x = to_time x
  let to_float = of_time
  let ( |+| ) x y = to_time (of_time x +. of_time y)
  let ( |-| ) x y = to_time (of_time x -. of_time y)
  let ( |*| ) x y = to_time (of_time x *. of_time y)
  let ( |<| ) x y = of_time x < of_time y
  let ( |<=| ) x y = of_time x <= of_time y

  let rec sleep_until t =
    let delay = of_time t -. Unix.gettimeofday () in
    if 0. < delay then (
      try Thread.delay delay
      with Unix.Unix_error (Unix.EINTR, _, _) -> sleep_until t)
end

type implementation = (module T)

let unix : implementation = (module Unix)
let implementations : (string, implementation) Hashtbl.t = Hashtbl.create 2
let () = Hashtbl.replace implementations "ocaml" unix
