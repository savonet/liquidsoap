(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2009 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Video data manipulation *)

(** Raised when reading from a buffer of a file which is not of the expected
  * format *)
exception Invalid_format of string

(** {2 Types} *)

(** An RGBA frame. *)
type data =
       (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t =
  {
    (** Order matters for C callbacks !! *)
    data   : data;
    width  : int;
    height : int;
    stride : int
  }

(** An RGBA color. *)
type color = int * int * int * int

(** {2 Utility functions} *)

(** Extract the components of a color in 0xRRGGBB format. *)
val rgb_of_int : int -> int * int * int

(** {2 Basic manipulation} *)

(** Create a frame of a given width and height (in pixels). *)
val create : ?stride:int -> int -> int -> t

(** Copy a frame. *)
val copy : t -> t

val copy_channel : t array -> t array

(** Get the value of a pixel. *)
val get_pixel : t -> int -> int -> color

(** Set the value of a pixel. *)
val set_pixel : t -> int -> int -> color -> unit

(** [blit src dst] blits [src] into [dst] with a translation of [(x, y)]
  * and a scaling of [(w,h)].
  * Frames don't have to be of the same size. The [blank] parameter should
  * be set to [false] if what's outside [src] in [dst] does not need to be
  * blanked. *)
val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

(** Faster implementation of blit without translation or scaling, when both
  * images are of the same dimension. *)
val blit_fast : t -> t -> unit

(** [add dst src] adds the frame [src] on top of the frame [dst] in [dst] (see
  * [blit] for the meaning of other parameters. *)
val add : ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

val add_fast : t -> t -> unit

(** {2 Conversions} *)

(** Read a buffer in RGB format of given width (in pixels). *)
val of_linear_rgb : string -> int -> t

(** YUV data in [(y,y_stride),(u,v,uv_stride))] format. *)
type yuv = (data *int ) * (data * data * int)

val of_YUV420 : yuv -> t -> unit

(** Fill a frame from a YUV420 buffer of a given width and height. *)
val of_YUV420_create : yuv -> int -> int -> t

(** Create a YUV frame with [width height] size. *)
val create_yuv : int -> int -> yuv

(** Blank a YUV frame. *)
val blank_yuv : yuv -> unit

(** Fill a yuv buffer with a RGB frame. *)
val to_YUV420 : t -> yuv -> unit

(** Convert a frame to BMP format. *)
val to_bmp : t -> string

(** Save frame in a bitmap file. *)
val save_bmp : t -> string -> unit

(** Read a PPM in a string.
  * [alpha] is an optional color meaning transparency. *)
val of_ppm : ?alpha:(int * int * int) -> string -> t

(** Same as [of_ppm] but reads PPM from a file. *)
val read_ppm : ?alpha:(int * int * int) -> string -> t

(** Convert a frame to an array of [int] of format 0xRRGGBB. Useful for using
  * the [Graphics] module. *)
val to_int_image : t -> int array array

(** {2 Effetcs} *)

(** Blank a frame (i.e. set colors of pixels to black and alpha to zero). *)
val blank : t -> unit

(** Fill all the pixel of a frame with a given color. *)
val fill : t -> color -> unit

(** Give a random color to every pixel of the frame (alphas are preserved). *)
val randomize : t -> unit

(** [scale dst src] scales the frame [src] to the frame [dst]. *)
val scale : t -> t -> unit

(** Scale a frame to a given width and height. A new frame is returned. *)
val scale_to : t -> int -> int -> t

(** [proportional_scale dst src] scales the frame [src] to the frame [dst]. The
  * width / height ratio is preserved and black borders are added if necessary. *)
val proportional_scale : ?bilinear:bool -> t -> t -> unit

(** Same as [proportional_scale] but creates a new frame of given width and
  * height. *)
val proportional_scale_to : ?bilinear:bool -> t -> int -> int -> t

(** Convert the colors of a frame to greyscale. *)
val greyscale : t -> unit

(** Convert the colors of a frame to sepia. *)
val sepia : t -> unit

(** Invert the colors of a frame. *)
val invert : t -> unit

(** Rotate an image by a given angle (in radians). *)
val rotate : t -> float -> unit

(** Multiply opacity of the image by a coefficient (between 0 and 1). *)
val scale_opacity : t -> float -> unit

(** [circle_opacity x y r] draws a circle of radius [r] centered at [(x,y)] in
  * alpha channel. *)
val disk_opacity : t -> int -> int -> int -> unit

(** [affine ax ay bx by] scales the image by [(ax, ay)] and translates it of
  * [(ox, oy)]. *)
val affine : t -> float -> float -> int -> int -> unit

(** Translate the image. *)
val translate : t -> int -> int -> unit

(** [mask f m] sets the alpha mask of the frame [f] according to the frame [m]. *)
val mask : t -> t -> unit

(** Emulate the "Lomo effect". *)
val lomo : t -> unit

(** Set a color to be transparent. Last parameter is precision. *)
val color_to_alpha : t -> int * int * int -> int -> unit

(** Blur opacity. *)
val blur_alpha : t -> unit
