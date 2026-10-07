(** Bindings to libswresample. The interface is the one of spec/swresample.md
    §4. *)

open Avutil

val version : version

type 'a kind

module type AudioData = sig
  type t

  val kind : t kind
end

type dither_type = Swresample_options.dither_type
type engine = Swresample_options.engine
type filter_type = Swresample_options.filter_type
type options = [ dither_type | engine | filter_type ]
type ('i, 'o) ctx

module Make (I : AudioData) (O : AudioData) : sig
  type t = (I.t, O.t) ctx

  val create :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  val from_codec :
    ?options:options list ->
    audio Avcodec.params ->
    Channel_layout.t ->
    ?out_sample_format:Sample_format.t ->
    int ->
    t

  val to_codec :
    ?options:options list ->
    Channel_layout.t ->
    ?in_sample_format:Sample_format.t ->
    int ->
    audio Avcodec.params ->
    t

  val from_codec_to_codec :
    ?options:options list -> audio Avcodec.params -> audio Avcodec.params -> t

  val convert : ?offset:int -> ?length:int -> t -> I.t -> O.t
  val flush : t -> O.t
end

type u8ba =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type s16ba =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

type s32ba = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64ba = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

module Bytes : AudioData with type t = bytes
module FloatArray : AudioData with type t = float array
module PlanarFloatArray : AudioData with type t = float array array
module Frame : AudioData with type t = audio frame
module U8Bytes : AudioData with type t = bytes
module S16Bytes : AudioData with type t = bytes
module S32Bytes : AudioData with type t = bytes
module FltBytes : AudioData with type t = bytes
module DblBytes : AudioData with type t = bytes
module U8PlanarBytes : AudioData with type t = bytes array
module S16PlanarBytes : AudioData with type t = bytes array
module S32PlanarBytes : AudioData with type t = bytes array
module FltPlanarBytes : AudioData with type t = bytes array
module DblPlanarBytes : AudioData with type t = bytes array
module U8BigArray : AudioData with type t = u8ba
module S16BigArray : AudioData with type t = s16ba
module S32BigArray : AudioData with type t = s32ba
module FltBigArray : AudioData with type t = f32ba
module DblBigArray : AudioData with type t = f64ba
module U8PlanarBigArray : AudioData with type t = u8ba array
module S16PlanarBigArray : AudioData with type t = s16ba array
module S32PlanarBigArray : AudioData with type t = s32ba array
module FltPlanarBigArray : AudioData with type t = f32ba array
module DblPlanarBigArray : AudioData with type t = f64ba array
module U8Frame : AudioData with type t = audio frame
module S16Frame : AudioData with type t = audio frame
module S32Frame : AudioData with type t = audio frame
module FltFrame : AudioData with type t = audio frame
module DblFrame : AudioData with type t = audio frame
module U8PlanarFrame : AudioData with type t = audio frame
module S16PlanarFrame : AudioData with type t = audio frame
module S32PlanarFrame : AudioData with type t = audio frame
module FltPlanarFrame : AudioData with type t = audio frame
module DblPlanarFrame : AudioData with type t = audio frame
