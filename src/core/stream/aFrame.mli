(** Specialization of Frame for content kinds restricted to audio, with a fixed
    number of channels. All units are in audio samples instead of main clock
    ticks. *)

(** {2 Types} *)

type t = Frame.t

(** {2 Basic manipulation} *)

(** Duration in seconds. *)
val duration : unit -> float

(** {2 Track marks}
    Track marks are track limits. Everything below is in samples. *)

(** Size of an audio frame. *)
val size : unit -> int

(** Current position in frame. *)
val position : t -> int

(** {2 Helpers} *)

(** Get PCM content. Raises [Content.Invalid] if frame content is not pcm. *)
val pcm : t -> Content.Audio.data

(** Get content in S16LE format. Raises [Content.Invalid] if frame content is
    not pcm. *)
val s16le : t -> string

(** {2 Sound processing} *)

(** RMS (root mean square) of a portion of a frame. *)
val rms : t -> int -> int -> float array
