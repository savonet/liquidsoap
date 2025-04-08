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

(** Operations on video data. *)

open Mm_audio
open Mm_image
module YUV420 = Image.YUV420

(** Images of videos. *)
module Image : sig
  type t = Image.YUV420.t

  val create : int -> int -> t
  val of_RGB24_string : string -> int -> t

  (** Convert to format usable by [Graphics.make_image]. *)
  val to_int_image : t -> int array array

  val copy : t -> t
  val width : t -> int
  val height : t -> int
  val dimensions : t -> int * int

  (** Size in bytes. *)
  val size : t -> int

  val blank : t -> unit
  val has_alpha : t -> bool
  val fill_alpha : t -> int -> unit
  val scale : t -> t -> unit
  val randomize : t -> unit

  (** [blit_all src dst] blits an entire image. *)
  val blit : t -> t -> unit

  val get_pixel_rgba : t -> int -> int -> int * int * int * int
  val set_pixel_rgba : t -> int -> int -> int * int * int * int -> unit

  (** Add the fist image to the second. *)
  val add : t -> ?x:int -> ?y:int -> t -> unit

  module Effect : sig
    val greyscale : t -> unit
    val sepia : t -> unit
    val invert : t -> unit
    val lomo : t -> unit

    module Alpha : sig
      val scale : t -> float -> unit
      val disk : t -> int -> int -> int -> unit
    end
  end
end

(** A video buffer. *)
type t = Image.t array

type buffer = t

(** Create a buffer with a given number of frames of given size. *)
val make : int -> int -> int -> t

(** Video with a single image. *)
val single : Image.t -> t

val blit : t -> int -> t -> int -> int -> unit

(** Create a fresh copy of a buffer. *)
val copy : t -> t

(** Length in images. *)
val length : t -> int

(** Size in bytes. *)
val size : t -> int

(** Obtain the i-th image of a video. *)
val get : t -> int -> Image.t

val set : t -> int -> Image.t -> unit
val iter : (Image.t -> unit) -> t -> int -> int -> unit
val blank : t -> int -> int -> unit
val randomize : t -> int -> int -> unit

(** Videos with canvas images. *)
module MakeCanvas (_ : Mm_image.Image.CanvasImage) : sig
  module Image : module type of Mm_image.Image.Canvas (Image)

  (** An image. *)
  type image = Image.t

  (** A video. *)
  type t = image array

  (** Create a video with given length and dimensions. *)
  val make : int -> int * int -> t

  (** Make a copy of the video (images themselves are not copied since they are
      supposed to be immutable). *)
  val copy : t -> t

  (** Create a video with one canvas image. *)
  val single : Image.t -> t

  (** Create a video with one image. *)
  val single_image : Mm_image.Image.YUV420.t -> t

  (** Length of the video (in images). *)
  val length : t -> int

  (** Estimated size of the video (in bytes). *)
  val size : t -> int

  (** Get the nth image of the video. *)
  val get : t -> int -> image

  (** Set the nth image of the video. *)
  val set : t -> int -> image -> unit

  (** Apply a function on the nth image of the video. *)
  val map_image : (image -> image) -> t -> int -> unit

  (** Render the nth image of the video. *)
  val render : ?transparent:bool -> t -> int -> Mm_image.Image.YUV420.t

  (** Change the contents of the nth image of the video (like [set] but takes an
      image instead of a canvas as argument). *)
  val put : t -> int -> Mm_image.Image.YUV420.t -> unit

  (** Blank the video starting at offset with given length. *)
  val blank : t -> int -> int -> unit

  (** Copy the images of one video to the other. *)
  val blit : t -> int -> t -> int -> int -> unit

  (** Map a function to the images of a video (starting at given offset, for
      given length). *)
  val map : (image -> image) -> t -> int -> int -> unit

  (** Iterate a function on the rendering of the images of the video. *)
  val iter : (Mm_image.Image.YUV420.t -> unit) -> t -> int -> int -> unit
end

module Canvas : module type of MakeCanvas (Image)

(* module Ringbuffer_ext : Ringbuffer.R with type elt = frame *)

(* module Ringbuffer : Ringbuffer.R with type elt = frame *)

(** Operations on frame rates. *)
module FPS : sig
  type t = float

  (** Convert a frame rate to a fraction. *)
  val to_frac : t -> int * int
end

(** Operation on files in AVI format. *)
module AVI : sig
  (** Writing AVI files. *)
  module Writer : sig
    (** Generate a header for the AVI file. *)
    val header :
      ?format:[> `YUV420 ] ->
      width:int ->
      height:int ->
      framerate:int ->
      ?channels:int ->
      ?samplerate:int ->
      ?vendor:string ->
      unit ->
      string

    (** Operations on chunks, which are blocks of (audio / video) data. *)
    module Chunk : sig
      val audio_s16le : Audio.t -> string
      val video_yuv420 : YUV420.t -> string
    end
  end
end

module IO : sig
  exception Invalid_file

  module Reader : sig
    class type t = object
      method width : int
      method height : int

      (** Number of frames per second. *)
      method frame_rate : FPS.t
      (* method set_target_size : int -> int -> unit *)

      (** Read a given number of frames. *)
      method read : buffer -> int -> int -> int

      method close : unit
    end
  end

  module Writer : sig
    class type t = object
      method write : buffer -> int -> int -> unit
      method close : unit
    end

    class to_avi_file : string -> FPS.t -> int -> int -> t
  end
end
