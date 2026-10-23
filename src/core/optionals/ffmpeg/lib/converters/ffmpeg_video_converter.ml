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
module Img = Image.Generic
module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.BigArray)

let formats : (Img.Pixel.format * Avutil.Pixel_format.t) list =
  [
    (RGB RGB24, `Rgb24);
    (RGB BGR24, `Bgr24);
    (RGB RGB32, `Rgba);
    (RGB BGR32, `Bgra);
    (RGB RGBA32, `Rgba);
    (YUV YUV422, `Yuv422p);
    (YUV YUV444, `Yuv444p);
    (YUV YUV411, `Yuv411p);
    (YUV YUV410, `Yuv410p);
    (YUV YUVJ420, `Yuv420p);
    (YUV YUVJ422, `Yuvj422p);
    (YUV YUVJ444, `Yuvj444p);
  ]

let side img =
  (List.assoc (Img.pixel_format img) formats, Img.width img, Img.height img)

let planes img : Swscale.planes =
  match Img.pixel_format img with
    | RGB _ -> [| Img.rgb_data img |]
    | YUV _ ->
        let y, (u, v, stride) = Img.yuv_data img in
        [| y; (u, stride); (v, stride) |]

let create_scaler
    ((src_format, src_width, src_height), (dst_format, dst_width, dst_height)) =
  Scaler.create
    ~threads:(max 0 Settings.scaling_threads#get)
    [Pixel_formats.scaling_flag ()]
    src_width src_height src_format dst_width dst_height dst_format

(* The threaded scaler returns planes of the smallest line size; [dst] may
   have wider strides. *)
let blit_plane (data, line_size) (dst, dst_line_size) =
  for row = 0 to (Bigarray.Array1.dim data / line_size) - 1 do
    Bigarray.Array1.blit
      (Bigarray.Array1.sub data (row * line_size) line_size)
      (Bigarray.Array1.sub dst (row * dst_line_size) line_size)
  done

let create () =
  let context = ref None in
  fun src dst ->
    let key = (side src, side dst) in
    let scaler =
      match !context with
        | Some (k, scaler) when k = key -> scaler
        | _ ->
            let scaler = create_scaler key in
            context := Some (key, scaler);
            scaler
    in
    Array.iter2 blit_plane (Scaler.convert scaler (planes src)) (planes dst)

let () =
  let formats = List.map fst formats in
  Plug.register Video_converter.video_converters "ffmpeg"
    ~doc:"Convert image formats with FFmpeg." (formats, formats, create)
