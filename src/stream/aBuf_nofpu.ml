let sample_bytes = 4
let sample_max = 1 lsr 15
let fsample_max = float sample_max

type buffer = string
type t = buffer

let length b = Bytes.length b / sample_bytes

let create n = Bytes.create (n*sample_bytes)

let make n = Bytes.make (n*sample_bytes) '\000'

external clear : t -> int -> int -> unit = "caml_nofpu_clear"

let blit s soff d doff len = String.blit s (soff*sample_bytes) d (doff*sample_bytes) (len*sample_bytes)

let copy = Bytes.copy

external gain : float -> t -> int -> int -> unit = "caml_nofpu_gain"

let gain_sample k b o = gain k b o 1

external add : t -> int -> t -> int -> int -> unit = "caml_nofpu_add"

let sub _ = failwith "TODO"

external ms : t -> int -> int -> int = "caml_nofpu_ms"

let rms b o l =
  let ms = ms b o l in
  sqrt (float ms /. fsample_max)

let of_u8 _ = failwith "TODO"
(* external of_u8 : string -> int -> int -> t -> int -> unit = "caml_nofpu_of_u8" *)

let of_s16le _ = failwith "TODO"
(* external of_s16le : string -> int -> int -> t -> int -> unit = "caml_nofpu_of_s16le" *)

external to_s16le : t array -> int -> int -> string = "caml_nofpu_to_s16le"

let resample _ = failwith "TODO"
