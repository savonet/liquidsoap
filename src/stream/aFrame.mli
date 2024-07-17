(** This module is a wrapper around Frame,
  * which turns all units to frames instead of ticks. *)

(** {2 Types} *)

type t = Frame.t

(** {2 Basic manipulation} *)

(** Duration in seconds. *)
val duration : t -> float

(** {2 Breaks}
  * Breaks are track limits.
  * Everything below is in samples. *)

val size       : t -> int
val position   : t -> int
val breaks     : t -> int list
val add_break  : t -> int -> unit
val set_breaks : t -> int list -> unit

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Reset breaks. *)
val clear : t -> unit

(** Reset breaks and metadata, but leaves the last metadata at position -1. *)
val advance : t -> unit

(** {2 Metadatas handling} *)

exception No_metadata

type metadata = (string,string) Hashtbl.t

val free_metadata    : t -> int -> unit
val set_metadata     : t -> int -> metadata -> unit
val get_metadata     : t -> int -> metadata option
val free_all_metadata: t -> unit
val get_all_metadata : t -> (int*metadata) list
val set_all_metadata : t -> (int*metadata) list -> unit

(** {2 Chunks} *)

exception No_chunk
val get_chunk : t -> t -> unit

(** {2 Helpers} *)

(** Direct access to a track, assuming that it's PCM float. *)
val get_float_pcm : t -> float array array

val get_s16le_length : t -> int

val to_s16le : t -> string

(** Fill a frame from a (float) generator. *)
val fill_frame : Float_pcm.Generator.t -> Frame.t -> unit

(** Fill a frame from a (raw) generator. *)
val fill_frame_from_raw : Float_pcm.Generator_from_raw.t -> Frame.t -> unit

val blankify : t -> int -> int -> unit

val multiply : t -> int -> int -> float -> unit

val add : t -> int -> t -> int -> int -> unit

val substract : t -> int -> t -> int -> int -> unit

val rms : t -> int -> int -> float array
