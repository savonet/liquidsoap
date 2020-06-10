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

(* Video format converters *)

let log = Log.make ["video"; "converter"]

(* TODO: is it the good place for this ? *)
let video_conf =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "video")
    "Video settings"
    ~comments:["Options related to video."]

let video_converter_conf =
  Dtools.Conf.void
    ~p:(video_conf#plug "converter")
    "Video conversion"
    ~comments:["Options related to video conversion."]

let preferred_converter_conf =
  Dtools.Conf.string
    ~p:(video_converter_conf#plug "preferred")
    ~d:"gavl" "Preferred video converter"

let proportional_scale_conf =
  Dtools.Conf.bool
    ~p:(video_converter_conf#plug "proportional_scale")
    ~d:true "Preferred proportional scale."

module Img = Image.Generic

(** [~proportional src dst] performs the 
  * conversion from frame src to frame dst.
  * raises Not_found if no conversion routine
  * was found *)
type converter = proportional:bool -> Img.t -> Img.t -> unit

(* A converter plugin is a name, a list of input formats, 
  * a list of output formats,
  * a fonction to create a converter. *)
type converter_plug =
  Img.Pixel.format list * Img.Pixel.format list * (unit -> converter)

let video_converters : converter_plug Plug.plug =
  Plug.create ~doc:"Methods for converting video frames." "video converters"

exception Exit of converter

(** Only log preferred decoder availability once at start. *)
let () =
  ignore
    (Dtools.Init.at_start (fun () ->
         let preferred = preferred_converter_conf#get in
         match video_converters#get preferred with
           | None ->
               log#important "Couldn't find preferred video converter: %s."
                 preferred
           | _ -> log#important "Using preferred video converter: %s." preferred))

let find_converter src dst =
  try
    begin
      let preferred = preferred_converter_conf#get in
      match video_converters#get preferred with
        | None -> ()
        | Some (sf, df, f) ->
            if List.mem src sf && List.mem dst df then raise (Exit (f ()))
            else
              log#important "Default video converter %s cannot do %s->%s."
                preferred
                (Img.Pixel.string_of_format src)
                (Img.Pixel.string_of_format dst)
    end;
    List.iter
      (fun (name, (sf, df, f)) ->
        log#info "Trying %s video converter..." name;
        if List.mem src sf && List.mem dst df then raise (Exit (f ())) else ())
      video_converters#get_all;
    log#important "Couldn't find a video converter from format %s to format %s."
      (Img.Pixel.string_of_format src)
      (Img.Pixel.string_of_format dst);
    raise Not_found
  with Exit x -> x ~proportional:proportional_scale_conf#get

let scaler () ?(proportional = true) =
  (* Video.Image.scale ~proportional *)
  ignore proportional;
  fun src dst ->
    find_converter (Image.Generic.Pixel.YUV Image.Generic.Pixel.YUVJ420)
      (Image.Generic.Pixel.YUV Image.Generic.Pixel.YUVJ420)
      (Image.Generic.of_YUV420 src)
      (Image.Generic.of_YUV420 dst)
