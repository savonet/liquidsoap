(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Video_converter
module Img = Image.Generic
module P = Img.Pixel

let conf_gavl =
  Dtools.Conf.void
    ~p:(video_converter_conf#plug "gavl")
    "Gavl converter"
    ~comments:["Parameters for gavl converter."]

let conf_quality =
  Dtools.Conf.int ~p:(conf_gavl#plug "quality") "Conversion quality" ~d:2
    ~comments:["Quality setting for gavl video conversion. Range from 1 to 5"]

let scale_modes =
  [
    ("auto", Gavl.Video.Auto);
    ("nearest", Gavl.Video.Nearest);
    ("bilinear", Gavl.Video.Bilinear);
    ("quadratic", Gavl.Video.Quadratic);
    ("cubic_bspline", Gavl.Video.Cubic_bspline);
    ("cubic_mitchell", Gavl.Video.Cubic_mitchell);
    ("cubic_catmull", Gavl.Video.Cubic_catmull);
    ("scale_sinc_lanczos", Gavl.Video.Scale_sinc_lanczos);
  ]

let scale_args =
  String.concat ", "
    (List.map (fun (x, _) -> Printf.sprintf "\"%s\"" x) scale_modes)

exception Internal of Gavl.Video.scale_mode

let scale_mode_of_arg x =
  try
    let f (n, m) = if x = n then raise (Internal m) in
    List.iter f scale_modes;
    raise
      (Lang_errors.Invalid_value
         (Lang.string x, "gavl scale mode must be one of: " ^ scale_args))
  with Internal m -> m

let conf_scale_mode =
  Dtools.Conf.string
    ~p:(conf_gavl#plug "scale_mode")
    "Scale mode" ~d:"auto"
    ~comments:
      ( "Scale mode. Values must be one of: "
      :: List.map (fun (x, _) -> Printf.sprintf "\"%s\"" x) scale_modes )

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

let gavl_format_of x =
  match x with
    | P.RGB x -> (
        match x with
          | P.RGB24 -> Gavl.Video.Rgb_24
          | P.BGR24 -> Gavl.Video.Bgr_24
          | P.RGB32 -> Gavl.Video.Rgb_32
          | P.BGR32 -> Gavl.Video.Bgr_32
          | P.RGBA32 -> Gavl.Video.Rgba_32 )
    | P.YUV x -> (
        match x with
          | P.YUV422 -> Gavl.Video.Yuv_422_p
          | P.YUV444 -> Gavl.Video.Yuv_444_p
          | P.YUV411 -> Gavl.Video.Yuv_411_p
          | P.YUV410 -> Gavl.Video.Yuv_410_p
          | P.YUVJ420 -> Gavl.Video.Yuvj_420_p
          | P.YUVJ422 -> Gavl.Video.Yuvj_422_p
          | P.YUVJ444 -> Gavl.Video.Yuvj_444_p )

let video_format_of_frame f =
  let pf = gavl_format_of (Img.pixel_format f) in
  let w = Img.width f in
  let h = Img.height f in
  {
    Gavl.Video.frame_width = w;
    frame_height = h;
    image_width = w;
    image_height = h;
    pixel_width = 1;
    pixel_height = 1;
    pixelformat = pf;
    frame_duration = 0;
    timescale = 0;
    framerate_mode = Gavl.Video.Still;
    chroma_placement = Gavl.Video.Default;
    interlace_mode = Gavl.Video.No_interlace;
  }

let gavl_frame_of img =
  match Img.pixel_format img with
    | P.RGB _ ->
        let data, stride = Img.rgb_data img in
        {
          Gavl.Video.planes = [| (data, stride) |];
          timestamp = Int64.zero;
          duration = Int64.zero;
          frame_interlace_mode = Gavl.Video.No_interlace;
        }
    | P.YUV _ ->
        let (y, y_stride), (u, v, uv_stride) = Img.yuv_data img in
        {
          Gavl.Video.planes =
            [| (y, y_stride); (u, uv_stride); (v, uv_stride) |];
          timestamp = Int64.zero;
          duration = Int64.zero;
          frame_interlace_mode = Gavl.Video.No_interlace;
        }

module HT = struct
  type t = (bool * Gavl.Video.format * Gavl.Video.format) * Gavl.Video.t option

  let equal (fmt, _) (fmt', _) = fmt = fmt'
  let hash (fmt, _) = Hashtbl.hash fmt
end

module WH = struct
  include Weak.Make (HT)

  (* Number of converters to always keep in memory. *)
  let n = 2
  let keep = Array.make n None

  let add h fmt conv =
    let conv = (fmt, Some conv) in
    for i = 1 to n - 1 do
      keep.(i - 1) <- keep.(i)
    done;
    keep.(n - 1) <- Some conv;
    add h conv

  let find h fmt = Utils.get_some (snd (find h (fmt, None)))
end

(* Weak hashtable containing converters already created. *)
let converters = WH.create 5

let create () =
  let convert ~proportional src dst =
    let src_f = video_format_of_frame src in
    let dst_f = video_format_of_frame dst in
    (* Find or create a converter. *)
    let conv =
      try WH.find converters (proportional, src_f, dst_f)
      with Not_found ->
        let src_w = Img.width src in
        let src_h = Img.height src in
        let dst_w = Img.width dst in
        let dst_h = Img.height dst in
        let conv = Gavl.Video.create_converter src_f dst_f in
        if proportional then (
          let dst_rect =
            if dst_h * src_w < src_h * dst_w then (
              let ox = (dst_w - (src_w * dst_h / src_h)) / 2 in
              (ox, 0, dst_w - (2 * ox), dst_h) )
            else (
              let oy = (dst_h - (src_h * dst_w / src_w)) / 2 in
              (0, oy, dst_w, dst_h - (2 * oy)) )
          in
          Gavl.Video.set_rect conv (0., 0., float src_w, float src_h) dst_rect;
          Gavl.Video.reinit conv );
        WH.add converters (proportional, src_f, dst_f) conv;
        conv
    in
    (* We need to blank because we get garbage otherwise. *)
    if not (Img.width src = Img.width dst && Img.height src = Img.height dst)
    then Img.blank dst;

    (* Now we convert *)
    Gavl.Video.convert conv (gavl_frame_of src) (gavl_frame_of dst)
  in
  convert

let () = video_converters#register "gavl" (formats, formats, create)
