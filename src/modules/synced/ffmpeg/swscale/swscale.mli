(** This module perform image scaling and color space/pixel format conversion
    operations. *)

open Avutil

val version : version
val configuration : string
val license : string

type pixel_format = Avutil.Pixel_format.t
type flag = Fast_bilinear | Bilinear | Bicubic | Print_info
type t

(** [Swscale.create flags in_w in_h in_pf out_w out_h out_pf] create a Swscale.t
    scale context with the [flags] flag list defining the conversion type, the
    [in_w] width, [in_h] height and [in_pf] pixel format for input format and
    [out_w] width, [out_h] height and [out_pf] pixel format for output format.
*)
val create :
  flag list -> int -> int -> pixel_format -> int -> int -> pixel_format -> t

type planes = (data * int) array

(** [Swscale.scale ctx in_planes y h out_planes off] scale the [h] rows of
    [in_planes] starting from the row [y] to the [off] row of the [out_planes]
    output. *)
val scale : t -> planes -> int -> int -> planes -> int -> unit

(**/**)

type vector_kind = PackedBa | Ba | Frm | Str

(**/**)

(** Video data modules for Swscale module input and output parameterization. *)
module type VideoData = sig
  type t

  val vk : vector_kind
end

type ('i, 'o) ctx

(** Functor building an implementation of the swscale structure with
    parameterized input an output video data types *)
module Make (I : VideoData) (O : VideoData) : sig
  type t = (I.t, O.t) ctx

  (** [Swscale.create flags in_w in_h in_pf out_w out_h out_pf] do the same as
      {!Swscale.create}. *)
  val create :
    flag list -> int -> int -> pixel_format -> int -> int -> pixel_format -> t

  (*
  val from_codec : flag list -> video Avcodec.t -> int -> int -> pixel_format -> t
  (** [Swscale.from_codec flags in_vc out_w out_h out_pf] do the same as {!Swresample.create} with the [in_vc] video codec properties as input format. *)

  val to_codec : flag list -> int -> int -> pixel_format -> video Avcodec.t -> t
  (** [Swscale.to_codec flags in_w in_h in_pf out_vc] do the same as {!Swresample.create} with the [out_vc] video codec properties as output format. *)

  val from_codec_to_codec : flag list -> video Avcodec.t -> video Avcodec.t -> t
  (** [Swscale.from_codec_to_codec flags in_vc out_vc] do the same as {!Swresample.create} with the [in_vc] video codec properties as input format and the [out_vc] video codec properties as output format. *)
*)

  (** [Swscale.convert ctx ivd] scale and convert the [ivd] input video data to
      the output video data according to the [ctx] scaler context format.

      Raise Error if the conversion failed. *)
  val convert : t -> I.t -> O.t
end

(** Unsigned 8 bit bigarray split by planes. *)
module BigArray : sig
  type t = planes

  val vk : vector_kind
end

(** Unsigned 8 bit bigarrays split by planes with separate linesize array. *)
module PackedBigArray : sig
  type t = data array * int array

  val vk : vector_kind
end

(** Video frame. *)
module Frame : sig
  type t = video frame

  val vk : vector_kind
end

(** Bytes array. *)
module Bytes : sig
  type t = (string * int) array

  val vk : vector_kind
end
