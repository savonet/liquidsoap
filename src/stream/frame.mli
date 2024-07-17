(** {2 Types} *)

type t

type float_pcm = float array

type float_pcm_t = float (* samplefreq *)

type track_t =
  | Float_pcm_t of float_pcm_t
  | Midi_t of Midi.header

type track =
  | Float_pcm of (float_pcm_t * float_pcm)
  | Midi of (Midi.header * Midi.track ref)

(** {2 Basic manipulation} *)

(** [create type freq length] where [freq] is the number of ticks in a second
  * and [length] is in ticks. *)
val create : track_t array -> freq:int -> length:int -> t
val kind   : t -> track_t array

val get_tracks : t -> track array
val add_track : t -> track -> unit

(** Duration in seconds. *)
val duration : t -> float

(** {2 Breaks}
  * Breaks are track limits.
  * Everything below is in "size units". *)

val size       : t -> int
val position   : t -> int
val breaks     : t -> int list
val add_break  : t -> int -> unit
val set_breaks : t -> int list -> unit

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Reset breaks. *)
val clear : t -> unit

(** {2 Metadatas handling} *)

exception No_metadata

type metadata = (string,string) Hashtbl.t

val free_metadata    : t -> unit
val set_metadata     : t -> int -> metadata -> unit
val get_metadata     : t -> int -> metadata option
val get_all_metadata : t -> (int*metadata) list
val set_all_metadata : t -> (int*metadata) list -> unit

(** {2 Chunks} *)

exception No_chunk
val get_chunk : t -> t -> unit
