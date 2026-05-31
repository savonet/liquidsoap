(** This module perform audio resampling, rematrixing and sample format
    conversion operations. *)

open Avutil
open Swresample_options

val version : version

(**/**)

type vector_kind = Str | P_Str | Fa | P_Fa | Ba | P_Ba | Frm

(**/**)

(** Audio data modules for Swresample module input and output parameterization.
*)
module type AudioData = sig
  type t

  val vk : vector_kind
  val sf : Avutil.Sample_format.t
end

type options = [ dither_type | engine | filter_type ]
type ('i, 'o) ctx

(** Functor building an implementation of the swresample structure with
    parameterized input an output audio data types *)
module Make (I : AudioData) (O : AudioData) : sig
  type t = (I.t, O.t) ctx

  (** [Swresample.create in_cl ~in_sample_format:in_sf in_sr out_cl
       ~out_sample_format:out_sf out_sr] create a Swresample.t with [in_cl]
      channel layout, [in_sf] sample format and [in_sr] sample rate as input
      format and [out_cl] channel layout, [out_sf] sample format and [out_sr]
      sample rate as output format. If a sample format parameter is not
      provided, the sample format defined by the associated AudioData module is
      used.

      Raise Error "Swresample input/output sample format undefined" if a sample
      format parameter is not provided and the associated AudioData module does
      not define a sample format as is the case for Bytes and Frame. *)
  val create :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  (** [Swresample.from_codec in_ac out_cl ~out_sample_format:out_sf out_sr] do
      the same as {!Swresample.create} with the [in_ac] audio codec properties
      as input format. *)
  val from_codec :
    ?options:options list ->
    audio Avcodec.params ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  (** [Swresample.to_codec in_cl ~in_sample_format:in_sf in_sr out_ac] do the
      same as {!Swresample.create} with the [out_ac] audio codec properties as
      output format. *)
  val to_codec :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    audio Avcodec.params ->
    t

  (** [Swresample.from_codec_to_codec in_ac out_ac] do the same as
      {!Swresample.create} with the [in_ac] audio codec properties as input
      format and the [out_ac] audio codec properties as output format. *)
  val from_codec_to_codec :
    ?options:options list -> audio Avcodec.params -> audio Avcodec.params -> t

  (** [Swresample.convert rsp iad] resample and convert the [iad] input audio
      data to the output audio data according to the [rsp] resampler context
      format.

      Raise Error if the conversion failed. *)
  val convert : ?offset:int -> ?length:int -> t -> I.t -> O.t

  (** [Swresample.convert rpsp] flushes the last remaining data. *)
  val flush : t -> O.t
end

(** Byte string with undefined sample format for interleaved channels. The
    sample format must be passed to the create function. *)
module Bytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Unsigned 8 bit sample format byte string for interleaved channels. *)
module U8Bytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format byte string for interleaved channels. *)
module S16Bytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format byte string for interleaved channels. *)
module S32Bytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format byte string for interleaved channels. *)
module FltBytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format byte string for interleaved channels. *)
module DblBytes : sig
  type t = bytes

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Unsigned 8 bit sample format byte string for planar channels. *)
module U8PlanarBytes : sig
  type t = bytes array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format byte string for planar channels. *)
module S16PlanarBytes : sig
  type t = bytes array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format byte string for planar channels. *)
module S32PlanarBytes : sig
  type t = bytes array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format byte string for planar channels. *)
module FltPlanarBytes : sig
  type t = bytes array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format byte string for planar channels. *)
module DblPlanarBytes : sig
  type t = bytes array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format array for interleaved channels. *)
module FloatArray : sig
  type t = float array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format array for planar channels. *)
module PlanarFloatArray : sig
  type t = float array array

  val vk : vector_kind
  val sf : Sample_format.t
end

type u8ba =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type s16ba =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Unsigned 8 bit sample format bigarray for interleaved channels. *)
module U8BigArray : sig
  type t = u8ba

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format bigarray for interleaved channels. *)
module S16BigArray : sig
  type t = s16ba

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format bigarray for interleaved channels. *)
module S32BigArray : sig
  type t = s32ba

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format bigarray for interleaved channels. *)
module FltBigArray : sig
  type t = f32ba

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format bigarray for interleaved channels. *)
module DblBigArray : sig
  type t = f64ba

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Unsigned 8 bit sample format bigarray for planar channels. *)
module U8PlanarBigArray : sig
  type t = u8ba array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format bigarray for planar channels. *)
module S16PlanarBigArray : sig
  type t = s16ba array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format bigarray for planar channels. *)
module S32PlanarBigArray : sig
  type t = s32ba array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format bigarray for planar channels. *)
module FltPlanarBigArray : sig
  type t = f32ba array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format bigarray for planar channels. *)
module DblPlanarBigArray : sig
  type t = f64ba array

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Audio frame with undefined sample format. The sample format must be passed
    to the create function. *)
module Frame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Unsigned 8 bit sample format audio frame for interleaved channels. *)
module U8Frame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format audio frame for interleaved channels. *)
module S16Frame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format audio frame for interleaved channels. *)
module S32Frame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format audio frame for interleaved channels. *)
module FltFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format audio frame for interleaved channels. *)
module DblFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Unsigned 8 bit sample format audio frame for planar channels. *)
module U8PlanarFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 16 bit sample format audio frame for planar channels. *)
module S16PlanarFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Signed 32 bit sample format audio frame for planar channels. *)
module S32PlanarFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 32 bit sample format audio frame for planar channels. *)
module FltPlanarFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end

(** Float 64 bit sample format audio frame for planar channels. *)
module DblPlanarFrame : sig
  type t = audio frame

  val vk : vector_kind
  val sf : Sample_format.t
end
