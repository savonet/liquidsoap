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

module RGBA32 = ImageRGBA32
module YUV420 = ImageYUV420

exception Not_implemented

module Pixel = struct
  type rgb_format =
    | RGB24 (* 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
    | BGR24 (* 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
    | RGB32
      (* 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
    | BGR32
      (* 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
    | RGBA32

  (* 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)

  type yuv_format =
    | YUV422 (* Planar YCbCr 4:2:2. Each component is an uint8_t *)
    | YUV444 (* Planar YCbCr 4:4:4. Each component is an uint8_t *)
    | YUV411 (* Planar YCbCr 4:1:1. Each component is an uint8_t *)
    | YUV410 (* Planar YCbCr 4:1:0. Each component is an uint8_t *)
    | YUVJ420
      (* Planar YCbCr 4:2:0. Each component is an uint8_t,
       * luma and chroma values are full range (0x00 .. 0xff) *)
    | YUVJ422
      (* Planar YCbCr 4:2:2. Each component is an uint8_t,
       * luma and chroma values are full range (0x00 .. 0xff) *)
    | YUVJ444

  (* Planar YCbCr 4:4:4. Each component is an uint8_t, luma and
   * chroma values are full range (0x00 .. 0xff) *)

  type format = RGB of rgb_format | YUV of yuv_format

  let size = function
    | RGB x -> (
        match x with RGB24 | BGR24 -> 3 | RGB32 | BGR32 | RGBA32 -> 4)
    | YUV _ -> raise Not_implemented

  let string_of_format = function
    | RGB x -> (
        match x with
          | RGB24 -> "RGB24"
          | BGR24 -> "BGR24"
          | RGB32 -> "RGB32"
          | BGR32 -> "BGR32"
          | RGBA32 -> "RGBA32")
    | YUV x -> (
        match x with
          | YUV422 -> "YUV422"
          | YUV444 -> "YUV444"
          | YUV411 -> "YUV411"
          | YUV410 -> "YUV410"
          | YUVJ420 -> "YUVJ420"
          | YUVJ422 -> "YUVJ422"
          | YUVJ444 -> "YUVJ444")
end

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type rgb = { rgb_pixel : Pixel.rgb_format; rgb_data : data; rgb_stride : int }

type yuv = {
  yuv_pixel : Pixel.yuv_format;
  y : data;
  y_stride : int;
  u : data;
  v : data;
  uv_stride : int;
}

type t_data = RGB of rgb | YUV of yuv
type t = { data : t_data; width : int; height : int }

let rgb_data img =
  match img.data with
    | RGB rgb -> (rgb.rgb_data, rgb.rgb_stride)
    | _ -> assert false

let yuv_data img =
  match img.data with
    | YUV yuv -> ((yuv.y, yuv.y_stride), (yuv.u, yuv.v, yuv.uv_stride))
    | _ -> assert false

let width img = img.width
let height img = img.height

let pixel_format img =
  match img.data with
    | RGB rgb -> Pixel.RGB rgb.rgb_pixel
    | YUV yuv -> Pixel.YUV yuv.yuv_pixel

let make_rgb pix ?stride width height data =
  let stride =
    match stride with Some s -> s | None -> width * Pixel.size (Pixel.RGB pix)
  in
  let rgb_data = { rgb_pixel = pix; rgb_data = data; rgb_stride = stride } in
  { data = RGB rgb_data; width; height }

let of_RGBA32 img =
  let rgb_data =
    {
      rgb_pixel = Pixel.RGBA32;
      rgb_data = img.RGBA32.data;
      rgb_stride = img.RGBA32.stride;
    }
  in
  { data = RGB rgb_data; width = img.RGBA32.width; height = img.RGBA32.height }

let to_RGBA32 img =
  let rgb_data = match img.data with RGB d -> d | _ -> assert false in
  assert (rgb_data.rgb_pixel = Pixel.RGBA32);
  {
    RGBA32.data = rgb_data.rgb_data;
    width = img.width;
    height = img.height;
    stride = rgb_data.rgb_stride;
  }

let of_YUV420 img =
  let yuv_data =
    {
      yuv_pixel = Pixel.YUVJ420;
      y = img.YUV420.y;
      y_stride = img.YUV420.y_stride;
      u = img.YUV420.u;
      v = img.YUV420.v;
      uv_stride = img.YUV420.uv_stride;
    }
  in
  { data = YUV yuv_data; width = img.YUV420.width; height = img.YUV420.height }

let to_YUV420 img =
  let yuv = match img.data with YUV yuv -> yuv | _ -> assert false in
  assert (yuv.yuv_pixel = Pixel.YUVJ420);
  YUV420.make img.width img.height yuv.y yuv.y_stride yuv.u yuv.v yuv.uv_stride

external rgba32_to_bgr32 : data -> int -> data -> int -> int * int -> unit
  = "caml_RGBA32_to_BGR32"

external rgb24_to_rgba32 : data -> int -> data -> int -> int * int -> unit
  = "caml_RGB24_to_RGBA32"

external rgb32_to_rgba32 : data -> int -> data -> int -> int * int -> unit
  = "caml_RGB32_to_RGBA32"

let blank img =
  match img.data with
    | RGB rgb -> (
        match rgb.rgb_pixel with
          | Pixel.RGBA32 -> RGBA32.blank (to_RGBA32 img)
          | _ -> failwith "Not implemented")
    | YUV yuv -> (
        match yuv.yuv_pixel with
          | Pixel.YUVJ420 -> YUV420.blank (to_YUV420 img)
          | _ -> failwith "Not implemented")

let convert ?(proportional = true) ?scale_kind src dst =
  match (src.data, dst.data) with
    | RGB s, RGB d when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.RGBA32
      ->
        let src = to_RGBA32 src in
        let dst = to_RGBA32 dst in
        RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
    | YUV s, RGB d
      when s.yuv_pixel = Pixel.YUVJ420 && d.rgb_pixel = Pixel.RGBA32 ->
        let src = to_YUV420 src in
        let src = YUV420.to_RGBA32 src in
        let dst = to_RGBA32 dst in
        RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
    | RGB s, YUV d
      when s.rgb_pixel = Pixel.RGBA32 && d.yuv_pixel = Pixel.YUVJ420 ->
        let src = to_RGBA32 src in
        let src = YUV420.of_RGBA32 src in
        let dst = to_YUV420 dst in
        YUV420.scale ~proportional src dst
    | RGB s, RGB d when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.BGR32
      ->
        if src.width = dst.width && src.height = dst.height then
          rgba32_to_bgr32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
            (src.width, src.height)
        else raise Not_implemented
    | RGB s, RGB d when s.rgb_pixel = Pixel.RGB24 && d.rgb_pixel = Pixel.RGBA32
      ->
        if src.width = dst.width && src.height = dst.height then
          rgb24_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
            (src.width, src.height)
        else raise Not_implemented
    | RGB s, RGB d when s.rgb_pixel = Pixel.RGB32 && d.rgb_pixel = Pixel.RGBA32
      ->
        if src.width = dst.width && src.height = dst.height then
          rgb32_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
            (src.width, src.height)
        else raise Not_implemented
    | _ -> raise Not_implemented
