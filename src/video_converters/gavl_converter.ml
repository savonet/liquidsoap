(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Video_converter

let conf_gavl =
  Dtools.Conf.void ~p:(video_converter_conf#plug "gavl") "Gavl converter"
      ~comments:[
        "Parameters for gavl converter."
      ]

let conf_quality = 
  Dtools.Conf.int ~p:(conf_gavl#plug "quality") "Conversion quality" ~d:2
    ~comments:["Quality setting for gavl video conversion. Range from 1 to 5"]

let scale_modes = 
  [ ("auto",Gavl.Video.Auto); ("nearest",Gavl.Video.Nearest); 
    ("bilinear",Gavl.Video.Bilinear);("quadratic",Gavl.Video.Quadratic); 
    ("cubic_bspline",Gavl.Video.Cubic_bspline);
    ("cubic_mitchell",Gavl.Video.Cubic_mitchell);
    ("cubic_catmull",Gavl.Video.Cubic_catmull);
    ("scale_sinc_lanczos",Gavl.Video.Scale_sinc_lanczos) ]

let scale_args = 
  String.concat ", " 
    (List.map (fun (x,_) -> Printf.sprintf "\"%s\"" x) scale_modes)

exception Internal of Gavl.Video.scale_mode

let scale_mode_of_arg x = 
  try
    let f (n,m) = 
      if x = n then 
        raise (Internal m)
    in
    List.iter f scale_modes;
    raise ((Lang.Invalid_value
                    (Lang.string x, 
                     "gavl scale mode must be one of: " ^ scale_args)))
  with
    | Internal m -> m

let conf_scale_mode =
  Dtools.Conf.string ~p:(conf_gavl#plug "scale_mode") "Scale mode" ~d:"auto"
    ~comments:("Scale mode. Values must be one of: " :: 
                (List.map (fun (x,_) -> Printf.sprintf "\"%s\"" x) scale_modes))

let formats = [RGB Rgb_24; RGB Bgr_24; RGB Rgb_32;
               RGB Bgr_32; RGB Rgba_32; YUV Yuv_422;
               YUV Yuv_444; YUV Yuv_411; YUV Yuv_410;
               YUV Yuvj_420; YUV Yuvj_422; YUV Yuvj_444]

let gavl_format_of x = 
  match x with
    | RGB x -> 
       begin
         match x with
           | Rgb_24  -> Gavl.Video.Rgb_24
           | Bgr_24  -> Gavl.Video.Bgr_24
           | Rgb_32  -> Gavl.Video.Rgb_32
           | Bgr_32  -> Gavl.Video.Bgr_32
           | Rgba_32 -> Gavl.Video.Rgba_32
       end
    | YUV x -> 
       begin
        match x with
          | Yuv_422  -> Gavl.Video.Yuv_422_p
          | Yuv_444  -> Gavl.Video.Yuv_444_p
          | Yuv_411  -> Gavl.Video.Yuv_411_p
          | Yuv_410  -> Gavl.Video.Yuv_410_p
          | Yuvj_420 -> Gavl.Video.Yuvj_420_p
          | Yuvj_422 -> Gavl.Video.Yuvj_422_p
          | Yuvj_444 -> Gavl.Video.Yuvj_444_p
       end

let video_format_of_frame f = 
  let pf = 
    match f.frame_data with
      | Rgb x -> gavl_format_of (RGB x.rgb_format)
      | Yuv x -> gavl_format_of (YUV x.yuv_format)
  in
  let w = f.width in
  let h = f.height in
  {
  Gavl.Video.
    frame_width      = w;
    frame_height     = h;
    image_width      = w;
    image_height     = h;
    pixel_width      = 1;
    pixel_height     = 1;
    pixelformat      = pf;
    frame_duration   = 0;
    timescale        = 0;
    framerate_mode   = Gavl.Video.Still;
    chroma_placement = Gavl.Video.Default;
    interlace_mode   = Gavl.Video.No_interlace
  }

let gavl_frame_of x = 
  match x.frame_data with
    | Rgb x -> 
      { 
       Gavl.Video.
        planes = [|x.data,x.stride|];
        timestamp = Int64.zero;
        duration  = Int64.zero;
        frame_interlace_mode = Gavl.Video.No_interlace
      }
    | Yuv x -> 
      {
       Gavl.Video.
        planes = [|(x.y,x.y_stride);(x.u,x.uv_stride);(x.v,x.uv_stride)|];
        timestamp = Int64.zero;
        duration  = Int64.zero;
        frame_interlace_mode = Gavl.Video.No_interlace
      }

let create () =
  (* Instanciate with a default 
   * conversion type *)
  let w = Lazy.force Frame.video_width in
  let h = Lazy.force Frame.video_height in
  let yuv =
    {
    Gavl.Video.
      frame_width      = w;
      frame_height     = h;
      image_width      = w;
      image_height     = h;
      pixel_width      = 1;
      pixel_height     = 1;
      pixelformat      = Gavl.Video.Yuvj_420_p;
      frame_duration   = 0;
      timescale        = 0;
      framerate_mode   = Gavl.Video.Still;
      chroma_placement = Gavl.Video.Default;
      interlace_mode   = Gavl.Video.No_interlace
    }
  in
  let rgb =
    {
    Gavl.Video.
      frame_width      = w;
      frame_height     = h;
      image_width      = w;
      image_height     = h;
      pixel_width      = 1;
      pixel_height     = 1;
      pixelformat      = Gavl.Video.Rgba_32;
      frame_duration   = 0;
      timescale        = 0;
      framerate_mode   = Gavl.Video.Still;
      chroma_placement = Gavl.Video.Default;
      interlace_mode   = Gavl.Video.No_interlace
    }
  in
  let conv = Gavl.Video.create_converter yuv rgb in
  Gavl.Video.set_quality conv (conf_quality#get);
  Gavl.Video.set_scale_mode conv (scale_mode_of_arg conf_scale_mode#get);
  let was_p = ref false in
  let init_c = ref false in
  let reinit_c = ref false in
  let convert ~proportional src dst = 
    (* Check if we need to init/reinit the converter. *)
    let old_f,new_f = Gavl.Video.get_formats conv in
    let src_f = video_format_of_frame src in
    let dst_f = video_format_of_frame dst in
    let src_w = src.width in
    let src_h = src.height in
    let dst_w = dst.width in
    let dst_h = dst.height in
    if old_f <> src_f || new_f <> dst_f then
     begin
      init_c := true;
      was_p := false
     end;
    if proportional && not !was_p then
     begin
      let dst_rect = 
        if dst_h * src_w < src_h * dst_w then
          let ox = (dst_w - src_w * dst_h / src_h) / 2 in
          ox,0,dst_w - 2 * ox , dst_h
        else
          let oy = (dst_h - src_h * dst_w / src_w) / 2 in
          0,oy,dst_w,dst_h - 2 * oy
      in
      Gavl.Video.set_rect conv (0.,0.,float src_w,float src_h) dst_rect;
      reinit_c := true;
      was_p := true;
     end;
    if not proportional && !was_p then
     begin
       Gavl.Video.set_rect conv (0.,0.,float src_w,float src_h) (0,0,dst_w,dst_h);
       reinit_c := true;
       was_p := false
     end;
    if !init_c then
     begin
      Gavl.Video.init conv src_f dst_f;
      init_c := false;
      reinit_c := false
     end;
    if !reinit_c then
     begin
      Gavl.Video.reinit conv;
      reinit_c := false
     end;
    (* Now we convert *)
    Gavl.Video.convert conv (gavl_frame_of src)
                            (gavl_frame_of dst)
  in
  convert  

let () = video_converters#register "gavl" (formats,formats,create)
