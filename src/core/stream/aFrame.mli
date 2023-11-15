(** Specialization of Frame for content kinds restricted to audio,
  * with a fixed number of channels.
  * All units are in audio samples instead of main clock ticks. *)

(** {2 Types} *)

type t = Frame.t

(** {2 Basic manipulation} *)

(** Duration in seconds. *)
val duration : unit -> float

(** {2 Track marks}
  * Track marks are track limits.
  * Everything below is in samples. *)

(** Size of an audio frame. *)
val size : unit -> int

(** Current position in frame. *)
val position : t -> int

(** Track marks in frame. *)
val track_marks : t -> int list

(** Is it partially filled ? *)
val is_partial : t -> bool

(** {2 Metadatas handling} *)

exception No_metadata

type metadata = Frame.metadata

val get_metadata : t -> int -> metadata option
val get_all_metadata : t -> (int * metadata) list

(** {2 Helpers} *)

(** Get audio contents. Raises [Not_found] if frame has no audio data. *)
val content : t -> Content.data

(** Get PCM content. Raises [Content.Invalid]
  * if frame content is not pcm. *)
val pcm : t -> Content.Audio.data

(** Same as [content] with [offset=0], converted to s16le. *)
val to_s16le : t -> string

(** {2 Sound processing} *)

(** [blankify frame off len] blanks the frame at offset [off] for length [len] (in samples). *)
val blankify : t -> int -> int -> t

(** [multiply frame off len x] multiplies the audio data of the frame from
    offset [off] during length [len] by coefficient [x]. *)
val multiply : t -> int -> int -> float -> t

(** Add two portions of frames of same length. *)
val add : t -> int -> t -> int -> int -> t

(** RMS (root mean square) of a portion of a frame. *)
val rms : t -> int -> int -> float array
