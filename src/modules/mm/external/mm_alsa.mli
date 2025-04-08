open Mm_audio

val rw :
  int ->
  int ->
  ?device:string ->
  ?playback:bool ->
  ?capture:bool ->
  ?blocking:bool ->
  ?buffer_size:int ->
  ?periods:int ->
  unit ->
  < version : string
  ; delay : int
  ; prepare : unit
  ; wait : int -> bool
  ; recover : exn -> unit
  ; read : Audio.t -> int -> int -> int
  ; write : Audio.t -> int -> int -> int
  ; close : unit >
