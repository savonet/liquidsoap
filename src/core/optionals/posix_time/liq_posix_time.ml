open Posix_time2
open Posix_time2.Timespec

module Sys_time = struct
  let implementation = "native (high-precision)"
  let of_time : Liq_time.t -> Timespec.t = Obj.magic
  let to_time : Timespec.t -> Liq_time.t = Obj.magic
  let time () = to_time (clock_gettime `Monotonic)

  let of_float d =
    let tv_sec = Int64.of_float d in
    let tv_nsec = Int64.of_float ((d -. floor d) *. 1_000_000_000.) in
    to_time (Timespec.create tv_sec tv_nsec)

  let to_float t =
    let { tv_sec; tv_nsec } = of_time t in
    Int64.to_float tv_sec +. (Int64.to_float tv_nsec /. 1_000_000_000.)

  let normalize ~tv_sec ~tv_nsec =
    let tv_sec = Int64.add tv_sec (Int64.div tv_nsec 1_000_000_000L) in
    let tv_nsec = Int64.rem tv_nsec 1_000_000_000L in
    to_time (Timespec.create tv_sec tv_nsec)

  let apply fn x y =
    let x = of_time x in
    let y = of_time y in
    normalize ~tv_sec:(fn x.tv_sec y.tv_sec) ~tv_nsec:(fn x.tv_nsec y.tv_nsec)

  let ( |+| ) = apply Int64.add
  let ( |-| ) = apply Int64.sub

  let ( |*| ) x y =
    let x = of_time x in
    let y = of_time y in
    normalize
      ~tv_sec:(Int64.mul x.tv_sec y.tv_sec)
      ~tv_nsec:
        (Int64.add
           (Int64.add
              (Int64.mul x.tv_sec y.tv_nsec)
              (Int64.mul x.tv_nsec y.tv_sec))
           (Int64.div (Int64.mul x.tv_nsec y.tv_nsec) 1_000_000_000L))

  let ( |<| ) x y =
    let x = of_time x in
    let y = of_time y in
    if Int64.equal x.tv_sec y.tv_sec then x.tv_nsec < y.tv_nsec
    else x.tv_sec < y.tv_sec

  let ( |<=| ) x y =
    let x = of_time x in
    let y = of_time y in
    if Int64.equal x.tv_sec y.tv_sec then x.tv_nsec <= y.tv_nsec
    else x.tv_sec <= y.tv_sec

  let rec sleep_until t =
    if t |<=| time () then ()
    else (
      try
        clock_nanosleep
          ~clock:(if Sys.os_type = "Unix" then `Monotonic else `Realtime)
          ~absolute:true (of_time t)
      with
        | Unix.Unix_error (Unix.EINTR, _, _) -> sleep_until t
        | Unix.Unix_error (Unix.EINVAL, _, _) -> ())
end

let of_time : (module Liq_time.T) = (module Sys_time)
let () = Hashtbl.replace Liq_time.implementations "posix" of_time
