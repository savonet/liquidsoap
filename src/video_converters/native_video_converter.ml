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


open Video_converter

type conveter = RGB.t ref

let formats = [RGB Rgba_32;YUV Yuvj_420]

let create () =
  let f = RGB.create (Fmt.video_width ())
                         (Fmt.video_height ())
  in
  let buf = ref f in 
  let convert ~proportional src dst =
    let need_scale = 
      src.width <> dst.width ||
      src.height <> dst.height 
    in
    let is_rgb f = 
      match f.frame_data with
        | Rgb x when x.rgb_format = Rgba_32 -> true
        | Yuv x when x.yuv_format = Yuvj_420 -> false
        | _ -> raise Not_found
    in
    let rgb_data f = 
      match f.frame_data with
        | Rgb x when x.rgb_format = Rgba_32 
            -> x.data
        | _ -> raise Not_found
    in
    let rgb_stride f = 
      match f.frame_data with
        | Rgb x when x.rgb_format = Rgba_32
            -> x.stride
        | _ -> raise Not_found
    in
    let yuv_data f = 
      match f.frame_data with
        | Yuv x when x.yuv_format = Yuvj_420
            -> (x.y,x.y_stride),(x.u,x.v,x.uv_stride)
        | _ -> raise Not_found
    in
    match is_rgb src,is_rgb dst, need_scale with
      | true,true,false
      | false,false,_ -> raise Not_found (* TODO *)
      | true,true,true ->
        let sf = {
                  RGB.
                   width  = src.width; 
                   height = src.height;
                   stride = rgb_stride src;
                   data   = rgb_data src 
                 }
        in
        let df = {
                  RGB. 
                   width  = dst.width;
                   height = dst.height;
                   stride = rgb_stride dst;
                   data   = rgb_data dst 
                 }
        in
        if proportional then
          RGB.proportional_scale df sf
        else
          RGB.scale df sf;
      | false,true,x -> 
          if x then
            begin
             if (!buf).RGB.width <> src.width ||
                (!buf).RGB.height <> src.height then
               begin
                 let frame = RGB.create src.width src.height in
                 buf := frame
               end;
             RGB.of_YUV420 (yuv_data src) !buf;
             let df = {
                       RGB.
                        width  = dst.width; 
                        height = dst.height;
                        stride = rgb_stride dst; 
                        data   = rgb_data dst 
                      }
             in
             if proportional then
               RGB.proportional_scale df !buf
             else
               RGB.scale df !buf
            end
          else
            let df = {
                      RGB.
                       width  = dst.width;
                       height = dst.height;
                       stride = rgb_stride dst;
                       data   = rgb_data dst
                     }
            in
            RGB.of_YUV420 (yuv_data src) df;
      | true,false,x -> 
          if x then
           begin
            if (!buf).RGB.width <> src.width ||
               (!buf).RGB.height <> src.height then
              begin
                let frame = RGB.create src.width src.height in
                buf := frame
              end;
            let sf = {
                      RGB. 
                       width  = dst.width; 
                       height = dst.height;
                       stride = rgb_stride src;
                       data   = rgb_data src 
                     }
            in
            if proportional then
              RGB.proportional_scale sf !buf
            else
              RGB.scale sf !buf;
            RGB.to_YUV420 !buf (yuv_data dst)
           end
          else
            let sf = {
                      RGB.
                       width  = dst.width;
                       height = dst.height;
                       stride = rgb_stride src;
                       data   = rgb_data src
                     }
            in
            RGB.to_YUV420 sf (yuv_data dst);
  in
  convert

let () = video_converters#register "native" (formats,formats,create)
