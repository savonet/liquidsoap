(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

open Mm_base

(** Operations on audio data. *)

(** At given sample rate, number of samples in given amount of time. *)
val samples_of_seconds : int -> float -> int

(** At given sample rate, duration of given number of samples. *)
val seconds_of_samples : int -> int -> float

(** Convert decibels to linear coefficient. *)
val lin_of_dB : float -> float

(** Convert linear coefficient to decibels. *)
val dB_of_lin : float -> float

(** Operations on samples. *)
module Sample : sig
  (** A sample. *)
  type t = float

  (** Clip a sample (ie ensure that it is between [-1.] and [1.]. *)
  val clip : t -> t

  (** An IIR filter with given b coefficients. *)
  val fir : float array -> t -> t

  (** An IIR filter with given a and b coefficients. *)
  val iir : float array -> float array -> t -> t
end

(** Operations on notes. *)
module Note : sig
  type t = int

  val a4 : int
  val c5 : int
  val c0 : int
  val create : int -> int -> t
  val freq : t -> float
  val of_freq : float -> t
  val name : t -> int
  val octave : t -> int

  (** Returns note number and octave. *)
  val modulo : t -> int * int

  val to_string : t -> string
  val of_string : string -> t
end

(** Operations on mono buffers (with only one channel). *)
module Mono : sig
  (** A mono buffer. *)
  type t = float array

  type buffer = t

  val create : int -> t
  val make : int -> float -> t
  val sub : t -> int -> int -> t
  val blit : t -> int -> t -> int -> int -> unit
  val copy : t -> int -> int -> t

  val copy_to_ba :
    t ->
    int ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit

  val copy_from_ba :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    t ->
    int ->
    int ->
    unit

  val of_ba :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t -> t

  val to_ba :
    t ->
    int ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

  val copy_to_int16_ba :
    t ->
    int ->
    int ->
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit

  val copy_from_int16_ba :
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    t ->
    int ->
    int ->
    unit

  val of_int16_ba :
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t -> t

  val to_int16_ba :
    t ->
    int ->
    int ->
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** Length in samples. *)
  val length : t -> int

  val append : t -> int -> int -> t -> int -> int -> t

  (** Clear a portion of a buffer (fill it with zeroes). *)
  val clear : t -> int -> int -> unit

  val amplify : float -> t -> int -> int -> unit
  val resample : ?mode:[ `Nearest | `Linear ] -> float -> t -> int -> int -> t
  val clip : t -> int -> int -> unit
  val noise : t -> int -> int -> unit
  val squares : t -> int -> int -> float

  (** Samplewise add two buffers, storing the result in the first one. *)
  val add : t -> int -> t -> int -> int -> unit

  (** Samplewise multiply two buffers of the same length, storing the result in
      the first one. *)
  val mult : t -> int -> t -> int -> int -> unit

  module Ringbuffer_ext : Ringbuffer.R with type buffer = t
  module Ringbuffer : Ringbuffer.R with type buffer = t

  (** Buffers of variable size. These are particularly useful for temporary
      buffers. *)
  module Buffer_ext : sig
    type t

    val create : int -> t
    val length : t -> int
    val prepare : t -> int -> buffer
  end

  (** Functions for analyzing audio data. *)
  module Analyze : sig
    (** Compute the RMS power of a portion of a buffer. *)
    val rms : t -> int -> int -> float

    (** Simple implementation of the FFT algorithm. For fastest implementations
        optimized libraries such as fftw are recommended. *)
    module FFT : sig
      (** Internal data for computing FFT. *)
      type t

      (** Initialize FFT for an analysis of [2^n] samples. *)
      val init : int -> t

      (** Length of the FFT buffer analysis in samples. *)
      val length : t -> int

      (** [complex_create buf off len] create a array of complex numbers by
          copying data from [buf] (the imaginary part is null). *)
      val complex_create : buffer -> int -> int -> Complex.t array

      (** Perform an FFT analysis. *)
      val fft : t -> Complex.t array -> unit

      (** Frequency associated to the [k]-th coefficient of an FFT. *)
      val band_freq : int -> t -> int -> float

      (** Windowing functions. These can be used to on complex buffers in order
          to improve the quality of the FFT, see
          http://en.wikipedia.org/wiki/Windowing_functions. *)
      module Window : sig
        val cosine : Complex.t array -> unit
        val hann : Complex.t array -> unit
        val hamming : Complex.t array -> unit
        val lanczos : Complex.t array -> unit
        val triangular : Complex.t array -> unit
        val bartlett_hann : Complex.t array -> unit
        val blackman : ?alpha:float -> Complex.t array -> unit
        val nuttall : Complex.t array -> unit
        val blackman_harris : Complex.t array -> unit
        val blackman_nuttall : Complex.t array -> unit
      end

      val notes :
        int ->
        t ->
        ?note_min:int ->
        ?note_max:int ->
        ?volume_min:float ->
        ?filter_harmonics:bool ->
        buffer ->
        (Note.t * float) list

      val loudest_note : (Note.t * float) list -> (Note.t * float) option
    end
  end

  module Effect : sig
    (** A compander following the mu-law (see
        http://en.wikipedia.org/wiki/Mu-law).*)
    val compand_mu_law : float -> t -> int -> int -> unit

    class type t = object
      method process : buffer -> int -> int -> unit
    end

    class amplify : float -> t
    class clip : float -> t

    class biquad_filter :
      int ->
      [ `Band_pass
      | `High_pass
      | `Low_pass
      | `Notch
      | `All_pass
      | `Peaking
      | `Low_shelf
      | `High_shelf ] ->
      ?gain:float ->
      float ->
      float ->
      t

    (** ADSR (Attack/Decay/Sustain/Release) envelopes. *)
    module ADSR : sig
      (** An ADSR envelope. *)
      type t

      (** Create an envelope with specified Attack/Decay/Sustain/Release times
          in seconds (excepting sustain which is an amplification coefficient
          between [0.] and [1.]). Negative sustain means that that notes should
          be released just after decay. *)
      val make : int -> float * float * float * float -> t

      (** Current state in the ADSR envelope. *)
      type state

      (** Initial state for processing. *)
      val init : unit -> state

      val release : state -> state
      val dead : state -> bool
      val process : t -> state -> buffer -> int -> int -> state
    end
  end

  (** Sound generators. *)
  module Generator : sig
    (** A sound generator. *)
    class type t = object
      method set_volume : float -> unit
      method set_frequency : float -> unit

      (** Fill a buffer with generated sound. *)
      method fill : buffer -> int -> int -> unit

      (** Same as [fill] but adds the sound to the buffer. *)
      method fill_add : buffer -> int -> int -> unit

      (** Release the generator (used for generator with envelopes). *)
      method release : unit

      (** Is the generator still producing sound? This should become false soon
          after release has been triggered. *)
      method dead : bool
    end

    (** Generate a sine waveform. *)
    class sine : int -> ?volume:float -> ?phase:float -> float -> t

    (** Generate a square waveform. *)
    class square : int -> ?volume:float -> ?phase:float -> float -> t

    (** Generate a saw waveform. *)
    class saw : int -> ?volume:float -> ?phase:float -> float -> t

    (** Generate a triangle waveform. *)
    class triangle : int -> ?volume:float -> ?phase:float -> float -> t

    class white_noise : ?volume:float -> int -> t
    class chain : t -> Effect.t -> t
    class add : t -> t -> t
    class mult : t -> t -> t

    (** Apply an ADSR envlope on a generator. *)
    class adsr : Effect.ADSR.t -> t -> t
  end
end

(** An audio buffer. *)
type t = float array array

type buffer = t

(** [create chans len] creates a buffer with [chans] channels and [len] samples
    as duration. *)
val create : int -> int -> t

val make : int -> int -> float -> t

(** Length in samples. *)
val length : t -> int

(** Create a buffer with the same number of channels and duration as the given
    buffer. *)
val create_same : t -> t

(** Clear the buffer (sets all the samples to zero). *)
val clear : t -> int -> int -> unit

(** Copy the given buffer. *)
val copy : t -> int -> int -> t

val append : t -> int -> int -> t -> int -> int -> t
val channels : t -> int

(** Convert a buffer to a mono buffer by computing the mean of all channels. *)
val to_mono : t -> int -> int -> Mono.t

(** Convert a mono buffer into a buffer. Notice that the original mono buffer is
    not copied an might thus be modified afterwards. *)
val of_mono : Mono.t -> t

val interleave : t -> int -> int -> Mono.t
val deinterleave : int -> Mono.t -> int -> int -> t

module U8 : sig
  val size : int -> int -> int
  val of_audio : t -> int -> Bytes.t -> int -> int -> unit
  val to_audio : string -> int -> t -> int -> int -> unit
end

module S16LE : sig
  val size : int -> int -> int
  val length : int -> int -> int
  val of_audio : t -> int -> Bytes.t -> int -> int -> unit
  val make : t -> int -> int -> string
  val to_audio : string -> int -> t -> int -> int -> unit
end

module S16BE : sig
  val size : int -> int -> int
  val length : int -> int -> int
  val of_audio : t -> int -> Bytes.t -> int -> int -> unit
  val make : t -> int -> int -> string
  val to_audio : string -> int -> t -> int -> int -> unit
end

module S24LE : sig
  val size : int -> int -> int
  val of_audio : t -> int -> Bytes.t -> int -> int -> unit
  val to_audio : string -> int -> t -> int -> int -> unit
end

module S32LE : sig
  val size : int -> int -> int
  val of_audio : t -> int -> Bytes.t -> int -> int -> unit
  val to_audio : string -> int -> t -> int -> int -> unit
end

module FLTP : sig
  val of_audio :
    src:t ->
    src_offset:int ->
    dst:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    dst_offset:int ->
    len:int ->
    stride:int ->
    unit

  val to_audio :
    src:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    src_offset:int ->
    dst:t ->
    dst_offset:int ->
    len:int ->
    stride:int ->
    unit
end

val resample : ?mode:[ `Nearest | `Linear ] -> float -> t -> int -> int -> t
val blit : t -> int -> t -> int -> int -> unit
val sub : t -> int -> int -> t
val clip : t -> int -> int -> unit
val noise : t -> int -> int -> unit
val squares : t -> int -> int -> float

val copy_to_ba :
  t ->
  int ->
  int ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
  unit

val copy_from_ba :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
  t ->
  int ->
  int ->
  unit

val of_ba :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array -> t

val to_ba :
  t ->
  int ->
  int ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

val copy_to_int16_ba :
  t ->
  int ->
  int ->
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
  unit

val copy_from_int16_ba :
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
  t ->
  int ->
  int ->
  unit

val of_int16_ba :
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
  t

val to_int16_ba :
  t ->
  int ->
  int ->
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t array

(** Amplify a portion of the buffer by a given coefficient. *)
val amplify : float -> t -> int -> int -> unit

(** Pan a stereo buffer from left to right (the buffer should have exactly two
    channels!). The coefficient should be between [-1.] and [1.]. *)
val pan : float -> t -> int -> int -> unit

(** Add two buffers of the same length, storing the result in the first one. *)
val add : t -> int -> t -> int -> int -> unit

(** Add to the first buffer, the second buffer multiplied by a coefficient. *)
val add_coeff : t -> int -> float -> t -> int -> int -> unit

(** Buffers of variable size. These are particularly useful for temporary
    buffers. *)
module Buffer_ext : sig
  type t

  (** Create an extensible buffer of given channels and initial size in samples.
  *)
  val create : int -> int -> t

  (** Make sure that the buffer can hold at least a given number of samples. *)
  val prepare : t -> ?channels:int -> int -> buffer
end

(** Circular ringbuffers. *)
module Ringbuffer : sig
  (** A ringbuffer. *)
  type t

  (** Create a ringbuffer of given number of channels and size (in samples). *)
  val create : int -> int -> t

  (** Number of channels of the ringbuffr. *)
  val channels : t -> int

  (** Number of samples available for reading. *)
  val read_space : t -> int

  (** Number of samples available for writing. *)
  val write_space : t -> int

  (** Advance the read pointer.*)
  val read_advance : t -> int -> unit

  (** Advance the write pointer. *)
  val write_advance : t -> int -> unit

  (** Fill in a buffer without changing read pointer. *)
  val peek : t -> buffer -> unit

  (** Fill in a buffer and advance read pointer. *)
  val read : t -> buffer -> unit

  (** Write a buffer into the ringbuffer. *)
  val write : t -> buffer -> unit

  val transmit : t -> (buffer -> int) -> int
end

(** Extensible ringbuffers.*)
module Ringbuffer_ext : sig
  type t

  val create : int -> int -> t
  val channels : t -> int
  val read_space : t -> int
  val write_space : t -> int
  val read_advance : t -> int -> unit
  val write_advance : t -> int -> unit
  val peek : t -> buffer -> unit
  val read : t -> buffer -> unit
  val write : t -> buffer -> unit
  val transmit : t -> (buffer -> int) -> int
end

module Analyze : sig
  val rms : t -> int -> int -> float array

  module ReplayGain : sig
    type t

    exception Not_supported

    (** Create internal state for computing ReplayGain. Raises [Not_supported]
        if the samplerate is not supported. *)
    val create : channels:int -> samplerate:int -> t

    (** Process a buffer. *)
    val process : t -> buffer -> int -> int -> unit

    (** Peak of processed samples. *)
    val peak : t -> float

    (** Replaygain for processed samples. *)
    val gain : t -> float
  end
end

(** Audio effects. *)
module Effect : sig
  (** A possibly stateful audio effect. *)
  class type t = object
    (** Apply the effect on a buffer. *)
    method process : buffer -> int -> int -> unit
  end

  class chain : t -> t -> t
  class of_mono : int -> (unit -> Mono.Effect.t) -> t

  class type delay_t = object
    inherit t
    method set_delay : float -> unit
    method set_feedback : float -> unit
  end

  (** [delay chans samplerate d once feedback] creates a delay operator for
      buffer with [chans] channels at [samplerate] samplerate with [d] as delay
      in seconds and [feedback] as feedback. If [once] is set to [true] only one
      echo will be heard (no feedback). *)
  val delay :
    int -> int -> float -> ?once:bool -> ?ping_pong:bool -> float -> delay_t

  (** Hardknee compressor with RMS look-ahead envelope calculation and
      adjustable attack/decay. Given parameters are [attack] and [release] in
      seconds, [ratio] n means n:1 compression, [threshold] and [knee] in dB,
      and [rms_window] in second is the duration for RMS acquisition. [gain] is
      an additional pre-gain. *)
  class compress :
    ?attack:float ->
    ?release:float ->
    ?threshold:float ->
    ?ratio:float ->
    ?knee:float ->
    ?rms_window:float ->
    ?gain:float ->
    int ->
    int ->
  object
    inherit t
    method set_attack : float -> unit
    method set_gain : float -> unit
    method set_knee : float -> unit
    method set_ratio : float -> unit
    method set_release : float -> unit
    method set_threshold : float -> unit
    method reset : unit
  end

  (** A biquadratic filter. [gain] in dB is only used by peaking, low and high
      shelves. *)
  class biquad_filter :
    int ->
    int ->
    [ `Band_pass
    | `High_pass
    | `Low_pass
    | `Notch
    | `All_pass
    | `Peaking
    | `Low_shelf
    | `High_shelf ] ->
    ?gain:float ->
    float ->
    float ->
    t

  val auto_gain_control :
    int ->
    int ->
    ?rms_target:float ->
    ?rms_window:float ->
    ?kup:float ->
    ?kdown:float ->
    ?rms_threshold:float ->
    ?volume_init:float ->
    ?volume_min:float ->
    ?volume_max:float ->
    unit ->
    t
end

(** Sound generators. *)
module Generator : sig
  val white_noise : t -> int -> int -> unit

  class type t = object
    method set_volume : float -> unit
    method set_frequency : float -> unit
    method fill : buffer -> int -> int -> unit
    method fill_add : buffer -> int -> int -> unit
    method release : unit
    method dead : bool
  end

  class of_mono : Mono.Generator.t -> t
  class chain : t -> Effect.t -> t
end

(** Operation for reading and writing audio data from files, streams or devices.
*)
module IO : sig
  (** The file is not valid. *)
  exception Invalid_file

  (** The operation is not valid on the file/device. *)
  exception Invalid_operation

  (** Trying to read past the end of the stream. *)
  exception End_of_stream

  module Reader : sig
    class type t = object
      (** Number of channels. *)
      method channels : int

      (** Sample rate in samples per second. *)
      method sample_rate : int

      (** Length in samples. *)
      method length : int

      (** Duration in seconds. *)
      method duration : float

      (** Seek to a given sample. *)
      method seek : int -> unit

      (** Close the file. This method should only be called once. The members of
          the object should not be accessed anymore after this method has been
          called. *)
      method close : unit

      method read : buffer -> int -> int -> int
    end

    (** Create a reader object from a wav file. *)
    class of_wav_file : string -> t
  end

  module Writer : sig
    class type t = object
      method write : buffer -> int -> int -> unit
      method close : unit
    end

    (** Create a writer to a file in WAV format with given number of channels,
        sample rate and file name.*)
    class to_wav_file : int -> int -> string -> t
  end

  module RW : sig
    class type t = object
      method read : buffer -> int -> int -> unit
      method write : buffer -> int -> int -> unit
      method close : unit
    end

    class virtual bufferized :
      int ->
      min_duration:int ->
      fill_duration:int ->
      max_duration:int ->
      drop_duration:int ->
    object
      method virtual io_read : buffer -> unit
      method virtual io_write : buffer -> unit
      method read : buffer -> unit
      method write : buffer -> unit
    end
  end
end
