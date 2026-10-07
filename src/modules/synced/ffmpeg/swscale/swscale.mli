(** Bindings to libswscale. The interface is the one of spec/swscale.md §4. *)

open Avutil

val version : version

type pixel_format = Avutil.Pixel_format.t
type flag = Fast_bilinear | Bilinear | Bicubic | Print_info
type t

val create :
  flag list -> int -> int -> pixel_format -> int -> int -> pixel_format -> t

type planes = (data * int) array

val scale : t -> planes -> int -> int -> planes -> int -> unit

type 'a kind

module type VideoData = sig
  type t

  val kind : t kind
end

type ('i, 'o) ctx

module Make (I : VideoData) (O : VideoData) : sig
  type t = (I.t, O.t) ctx

  val create :
    ?threads:int ->
    flag list ->
    int ->
    int ->
    pixel_format ->
    int ->
    int ->
    pixel_format ->
    t

  val convert : t -> I.t -> O.t
end

module BigArray : VideoData with type t = planes
module PackedBigArray : VideoData with type t = data array * int array
module Frame : VideoData with type t = video frame
module Bytes : VideoData with type t = (string * int) array
