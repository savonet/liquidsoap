include Audio.Mono

type t = buffer

let length = duration

let make = create

include Analyze

let sub = Array.sub

let gain = amplify

let gain_sample x b i =
  b.(i) <- b.(i) *. x

let to_s16le b o l = Audio.S16LE.make b o l

let of_u8 b o l c off = ignore (Audio.U8.to_audio b o l c off)

let of_s16le b o l c off = ignore (Audio.S16LE.to_audio b o l c off)
