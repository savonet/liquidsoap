(** These values describe the frames used in sources.
  *
  * Important note: time should be counted in ticks which is an abstract time
  * value and not in samples, conversion can be done with the help of
  * [ticks_per_sample] in particular.
  *
  * Currently all sources are expected to manipulate the same frame type.
  * In the future it is possible that some sources handle mono audio,
  * while other handle stereo, or even more that audio.
  * This shouldn't be too painful in terms of design since most sources don't
  * manipulate directly the contents, while those who do it are seen as
  * manipulating the float_pcm tracks of the frame without touching the others.
  * That's not so fair since add() or fade() should do it on video too...
  *
  * These values can be modified during initialization
  * but should remain fixed after that. *)

open Dtools

val conf : Conf.ut
val conf_samplerate : int Conf.t
val conf_size : int Conf.t
val conf_channels : int Conf.t

(** Number of samples in a second. *)
val samples_per_second : unit -> int

(** Number of samples in a frame. *)
val samples_per_frame : unit -> int

(** Number of audio channels in a frame. *)
val channels : unit -> int

(** Helper function to create an audio frame with the right parameters. *)
val create_frame : unit -> Frame.t

(** Number of ticks in a second. *)
val ticks_per_second : unit -> int

(** Number of ticks in a frame. *)
val ticks_per_frame : unit -> int

(** Number of ticks in a sample. *)
val ticks_per_sample : unit -> int

(** Duration of a frame in seconds. *)
val seconds_per_frame : unit -> float

val samples_of_seconds : float -> int
val ticks_of_seconds   : float -> int
val seconds_of_samples : int -> float
val seconds_of_ticks   : int -> float
val ticks_of_samples : int -> int
val samples_of_ticks : int -> int
