include Audio.Mono
(* include Audio.S32 *)

type t = buffer

let length = duration

let make = create

include Analyze

let sub = Array.sub

let to_s16le b =
  Audio.S16LE.make b 0 (duration b.(0))

let of_u8 = Audio.U8.convert_to_audio

let of_s16le = Audio.S16LE.convert_to_audio
