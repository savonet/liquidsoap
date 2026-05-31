(** Audio input and output using the OSS sound devices. *)

open Mm_audio

(** Create a writer on an OSS sound device. *)
class writer : ?device:string -> int -> int -> Audio.IO.Writer.t

class reader : ?device:string -> int -> int -> Audio.IO.Reader.t
