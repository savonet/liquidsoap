(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

 (* Video format converters *)

(** Plugin to add video-related configuration keys. *)
val video_converter_conf : Dtools.Conf.ut

(* From Gavl *)
type rgb_format = 
   | Rgb_24       (* 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
   | Bgr_24       (* 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
   | Rgb_32       (* 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
   | Bgr_32       (* 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
   | Rgba_32      (* 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)
type yuv_format =
   | Yuv_422    (* Planar YCbCr 4:2:2. Each component is an uint8_t *)
   | Yuv_444    (* Planar YCbCr 4:4:4. Each component is an uint8_t *)
   | Yuv_411    (* Planar YCbCr 4:1:1. Each component is an uint8_t *)
   | Yuv_410    (* Planar YCbCr 4:1:0. Each component is an uint8_t *)
   | Yuvj_420   (* Planar YCbCr 4:2:0. Each component is an uint8_t, 
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_422   (* Planar YCbCr 4:2:2. Each component is an uint8_t, 
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_444   (* Planar YCbCr 4:4:4. Each component is an uint8_t, 
                 * luma and chroma values are full range (0x00 .. 0xff) *)
type pixel_format = 
  | RGB of rgb_format
  | YUV of yuv_format

(** Data fields are unsigned 8 bit interger arrays for now. *)
type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 

(** A frame is, for now, either packed RGB
  * or planar YUV, both containing
  * unsigned 8 bits integers. *)
type rgb_frame =
  {
    rgb_format : rgb_format;
    data       : data;
    stride     : int
  }
type yuv_frame = 
  { 
    yuv_format : yuv_format;
    y          : data;
    y_stride   : int;
    u          : data;
    v          : data;
    uv_stride  : int
  }
type frame_data = 
  | Rgb of rgb_frame
  | Yuv of yuv_frame
type frame = 
  { 
    frame_data : frame_data;
    width      : int;
    height     : int
  }

(** Creates a frame with the data of an internal frame.
  * No copy is done. Don't forget to call [unlock_frame]
  * on the internal frame when processing is done. 
  * TODO: fix this horrible hack ! *)
val frame_of_internal_rgb : RGB.t -> frame 

(** Creates a frame from the data of an internal YUV frame. 
  * Parameters are: with, height, data. *)
val frame_of_internal_yuv : int -> int -> RGB.yuv -> frame

(** [~proportional src dst] performs the 
  * conversion from frame src to frame dst.
  * raises Not_found if no conversion routine
  * was found *) 
type converter = proportional:bool -> frame -> frame -> unit
(** A converter plugin is a name, a list of input formats, 
  * a list of output formats,
  * a fonction to create a converter. *)
type converter_plug = (pixel_format list)*(pixel_format list)*(unit->converter)

(** Plugin to register new converters. *)
val video_converters : converter_plug Plug.plug

(** [find_converter source destination] tries
  * to find a converter from source format 
  * to destination format. Proportional scale
  * is implicitely set via global configuration key 
  * for now. Returns a conversion function: frame -> frame -> unit. *)
val find_converter : pixel_format -> pixel_format -> (frame -> frame -> unit)

