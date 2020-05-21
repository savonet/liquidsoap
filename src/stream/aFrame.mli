(** Specialization of Frame for content kinds restricted to audio,
  * with a fixed number of channels.
  * All units are in audio samples instead of master clock ticks. *)

(** {2 Types} *)

type t = Frame.t

val channels_of_kind : Frame.content_kind -> int

(** {2 Basic manipulation} *)

(** Duration in seconds. *)
val duration : unit -> float

(** {2 Breaks}
  * Breaks are track limits.
  * Everything below is in samples. *)

(** Size of an audio frame. *)
val size : unit -> int

(** Current position in frame. *)
val position : t -> int

(** Breaks in frame. *)
val breaks : t -> int list

(** Add a break. *)
val add_break : t -> int -> unit

(** Change all the breaks. *)
val set_breaks : t -> int list -> unit

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Reset breaks. *)
val clear : t -> unit

(** Reset breaks and metadata, but leaves the last metadata at position -1. *)
val advance : t -> unit

(** {2 Metadatas handling} *)

exception No_metadata

type metadata = (string, string) Hashtbl.t

val free_metadata : t -> int -> unit
val set_metadata : t -> int -> metadata -> unit
val get_metadata : t -> int -> metadata option
val free_all_metadata : t -> unit
val get_all_metadata : t -> (int * metadata) list
val set_all_metadata : t -> (int * metadata) list -> unit

(** {2 Helpers} *)

(** Get audio contents *)
val content : t -> Frame.audio_t array

(** Same as [content] with [offset=0], converted to s16le. *)
val to_s16le : t -> string

(** {2 Sound processing} *)

(** [blankify frame off len] blanks the frame at offset [off] for length [len] (in samples). *)
val blankify : t -> int -> int -> unit

(** [multiply frame off len x] multiplies the audio data of the frame from
    offset [off] during length [len] by coefficent [x]. *)
val multiply : t -> int -> int -> float -> unit

(** Add two portions of frames of same length. *)
val add : t -> int -> t -> int -> int -> unit

(** RMS (root mean square) of a portion of a frame. *)
val rms : t -> int -> int -> float array
