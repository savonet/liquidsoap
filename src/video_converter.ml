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

 (* Video format converters *)

let log = Dtools.Log.make ["video";"converter"]

(* TODO: is it the good place for this ? *)
let video_conf = 
  Dtools.Conf.void ~p:(Configure.conf#plug "video") "Video settings"
    ~comments:["Options related to video."] 

let video_converter_conf =
  Dtools.Conf.void ~p:(video_conf#plug "converter") "Video conversion"
    ~comments:["Options related to video conversion."]

let preferred_converter_conf = 
  Dtools.Conf.string ~p:(video_converter_conf#plug "preferred") ~d:"gavl"
  "Preferred video converter"

let proportional_scale_conf =
  Dtools.Conf.bool ~p:(video_converter_conf#plug "proportional_scale") ~d:true
  "Preferred proportional scale."

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

let string_of_pixel_format x = 
  match x with
    | RGB x ->
        begin 
          match x with
            | Rgb_24  -> "RGB24"
            | Bgr_24  -> "BGR32"
            | Rgb_32  -> "RGB32"
            | Bgr_32  -> "BGR32"
            | Rgba_32 -> "RGBA32"
        end
    | YUV x ->
        begin 
          match x with
            | Yuv_422  -> "YUV422"
            | Yuv_444  -> "YUV444"
            | Yuv_411  -> "YUV411"
            | Yuv_410  -> "YUV410"
            | Yuvj_420 -> "YUVJ420"
            | Yuvj_422 -> "YUVJ422"
            | Yuvj_444 -> "YUVJ444"
        end

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

let frame_of_internal_rgb f = 
  let frame_data = 
    { 
      rgb_format = Rgba_32;
      data = Image.RGBA8.data f;
      stride = Image.RGBA8.stride f;
    }
  in
  { 
    frame_data = Rgb frame_data;
    width =  Image.RGBA8.width f;
    height = Image.RGBA8.height f
  }


let frame_of_internal_yuv w h f =
  let ((y,y_stride),(u,v,uv_stride)) = Image.YUV420.internal f in
  let frame_data =
    {
      yuv_format = Yuvj_420;
      y = y;
      y_stride = y_stride;
      u = u;
      v = v;
      uv_stride = uv_stride
    }
  in
  {
    frame_data = Yuv frame_data;
    width =  w;
    height = h
  }

(** [~proportional src dst] performs the 
  * conversion from frame src to frame dst.
  * raises Not_found if no conversion routine
  * was found *) 
type converter = proportional:bool -> frame -> frame -> unit
(** A converter plugin is a name, a list of input formats, 
  * a list of output formats,
  * a fonction to create a converter. *)
type converter_plug = (pixel_format list)*(pixel_format list)*(unit->converter)

let video_converters : converter_plug Plug.plug =
    Plug.create ~doc:"Methods for converting video frames." "video converters"

exception Exit of converter

(** Only log preferred decoder availability once at start. *)
let () = 
  ignore(Dtools.Init.at_start
    (fun () ->
      let preferred = preferred_converter_conf#get in
      match video_converters#get preferred with
        | None ->
            log#f 4 "Couldn't find preferred video converter: %s." preferred
        | _ -> log#f 4 "Using preferred video converter: %s." preferred)) 


let find_converter src dst = 
  try
    begin
      let preferred = preferred_converter_conf#get in
      match video_converters#get preferred with
        | None -> ()
        | Some (sf,df,f) -> 
            if List.mem src sf && List.mem dst df then
              raise (Exit (f ()))
            else
              log#f 4 "Default video converter %s cannot do %s->%s."
                preferred
                (string_of_pixel_format src)
                (string_of_pixel_format dst)
    end;   
    List.iter
      (fun (name,(sf,df,f)) ->
           log#f 4 "Trying %s video converter..." name ;
           if List.mem src sf && List.mem dst df then
             raise (Exit (f ()))
           else ())
      video_converters#get_all;
    log#f 4 "Couldn't find a video converter from format %s to format %s." 
      (string_of_pixel_format src)
      (string_of_pixel_format dst);
    raise Not_found
  with
    | Exit x -> x ~proportional:proportional_scale_conf#get
