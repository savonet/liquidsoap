(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Mm
open Video_converter
module Img = Image.Generic
module P = Img.Pixel

let formats =
  [
    P.RGB P.RGB24;
    P.RGB P.BGR24;
    P.RGB P.RGB32;
    P.RGB P.BGR32;
    P.RGB P.RGBA32;
    P.YUV P.YUV422;
    P.YUV P.YUV444;
    P.YUV P.YUV411;
    P.YUV P.YUV410;
    P.YUV P.YUVJ420;
    P.YUV P.YUVJ422;
    P.YUV P.YUVJ444;
  ]

let format_of frame =
  let fmt = Img.pixel_format frame in
  match fmt with
    | P.RGB fmt -> (
        match fmt with
          | P.RGB24 -> `Rgb24
          | P.BGR24 -> `Bgr24
          | P.RGB32 -> `Rgba
          | P.BGR32 -> `Bgra
          | P.RGBA32 -> `Rgba)
    | P.YUV fmt -> (
        match fmt with
          | P.YUV422 -> `Yuv422p
          | P.YUV444 -> `Yuv444p
          | P.YUV411 -> `Yuv411p
          | P.YUV410 -> `Yuv410p
          | P.YUVJ420 -> Ffmpeg_utils.liq_frame_pixel_format ()
          | P.YUVJ422 -> `Yuvj422p
          | P.YUVJ444 -> `Yuvj444p)

let is_rgb = function P.RGB _ -> true | _ -> false

let create () =
  (* A [SwsContext] is not reentrant, so it belongs to the converter that uses
     it rather than to a table every converter shares. *)
  let context = ref None in
  let convert src dst =
    let src_f = format_of src in
    let dst_f = format_of dst in
    let src_w = Img.width src in
    let src_h = Img.height src in
    let dst_w = Img.width dst in
    let dst_h = Img.height dst in
    let formats = ((src_f, src_w, src_h), (dst_f, dst_w, dst_h)) in
    let conv =
      match !context with
        | Some (formats', conv) when formats' = formats -> conv
        | _ ->
            let conv =
              Swscale.create
                [Swscale.Bilinear; Swscale.Print_info]
                src_w src_h src_f dst_w dst_h dst_f
            in
            context := Some (formats, conv);
            conv
    in
    let data f =
      match Img.pixel_format f with
        | P.RGB _ ->
            let buf, stride = Img.rgb_data f in
            [| (buf, stride) |]
        | P.YUV _ ->
            let (y, sy), (u, v, s) = Img.yuv_data f in
            [| (y, sy); (u, s); (v, s) |]
    in
    let src_d = data src in
    let dst_d = data dst in
    Swscale.scale conv src_d 0 src_h dst_d 0
  in
  convert

let () =
  Plug.register video_converters "ffmpeg" ~doc:"FFmpeg image format converter."
    (formats, formats, create)
