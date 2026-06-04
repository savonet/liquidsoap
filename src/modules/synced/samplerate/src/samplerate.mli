(** Bindings for libsamplerate library, which is dedicated to changing the
    sampling rate of audio data. All offsets and sizes are given in number of
    samples {i per channel}.

    @author Samuel Mimram *)

(** Kind of converter. *)
type converter =
  | Conv_sinc_best_quality
      (** This is a bandlimited interpolator derived from the mathematical sinc
          function and this is the highest quality sinc based converter,
          providing a worst case Signal-to-Noise Ratio (SNR) of 97 decibels (dB)
          at a bandwidth of 97%. All three Conv_sinc_* converters are based on
          the techniques of Julius O. Smith although this code was developed
          independently. *)
  | Conv_sinc_medium_quality
      (** This is another bandlimited interpolator much like the previous one.
          It has an SNR of 97dB and a bandwidth of 90%. The speed of the
          conversion is much faster than the previous one. *)
  | Conv_fastest
      (** This is the fastest bandlimited interpolator and has an SNR of 97dB
          and a bandwidth of 80%. *)
  | Conv_zero_order_hold
      (** A Zero Order Hold converter (interpolated value is equal to the last
          value). The quality is poor but the conversion speed is blindlingly
          fast. *)
  | Conv_linear
      (** A linear converter. Again the quality is poor, but the conversion
          speed is blindingly fast. *)

(** Name of a converter. *)
val get_conv_name : converter -> string

(** Description of a converter. *)
val get_conv_descr : converter -> string

(** {2 Simple API} *)

(** [convert converter channels ratio inbuf offset length] converts audio data
    with given number of channels with the given ratio (output samplerate /
    input samplerate) reading from given buffer starting at given offset, the
    given number of audio samples (per channel). *)
val convert :
  converter -> int -> float -> float array -> int -> int -> float array

(** {2 Full API} *)

(** Internal state for a converter. *)
type t

(** Create a converter of given kind with given number of channels. *)
val create : converter -> int -> t

(** Convert audio data with given state, at given ratio, reading from given
    buffer at given offset the given number of samples, writing in output buffer
    starting at offset the given number of samples. Returns the number of
    samples (per channel) used from input buffer and produced in output buffer.
*)
val process :
  t ->
  float ->
  float array ->
  int ->
  int ->
  float array ->
  int ->
  int ->
  int * int

(** Similar to [process] but takes bigarrays to store audio data. *)
val process_ba :
  t ->
  float ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int * int

(** Similar to [process] but allocates a new buffer instead of writing in a
    specified output buffer. *)
val process_alloc : t -> float -> float array -> int -> int -> float array

(** Reset the state of the encoder. *)
val reset : t -> unit
