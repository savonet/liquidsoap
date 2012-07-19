include Audio.S32

let sample_max = 1 lsr 15
let fsample_max = float sample_max

type buffer = t

let length b = String.length b / 4

(* TODO *)
let make = create

let copy = String.copy

let gain_sample k b o = gain k b o 1

let rms b o l =
  let ms = ms b o l in
  sqrt (float ms /. fsample_max)

let of_u8 buf o l ?(resample=1.) out off =
  if resample = 1. then
    (
      of_u8 buf o l out off;
      l
    )
  else
    failwith "TODO: implement of_u8"

let of_s16le buf o l ?(resample=1.) out off =
  if resample = 1. then
    (
      of_s16le buf o l out off;
      l
    )
  else
    failwith "TODO: implement of_s16le"
