module type Aacplus_t =
  sig
    type t
    exception Invalid_data
    exception Invalid_config
    val create : channels:int -> samplerate:int -> bitrate:int -> unit -> t
    val frame_size : t -> int
    val encode : t -> float array array -> string
  end
type handler = { mutable encoder : (module Aacplus_t) option; }
val handler : handler
