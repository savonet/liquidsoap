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

(** Save picture extracted from the video. *)

open Mm
open Source
open Extralib

class still_frame ~name (source : source) =
  object (self)
    inherit operator ~name [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    val mutable fname = None

    method save (f : string) =
      if not (String.ends_with f ".bmp") then
        self#log#severe
          "Only BMP files are supported for now, the filename should end with \
           .bmp"
      else fname <- Some f

    method private still buf =
      match fname with
        | None -> ()
        | Some f -> (
            let v = Content.Video.get_data (Frame.get buf Frame.Fields.video) in
            match v.Content.Video.data with
              | [] -> ()
              | (_, i) :: _ ->
                  let i =
                    i |> Video.Canvas.Image.render |> Image.YUV420.to_RGBA32
                    |> Image.RGBA32.to_BMP
                  in
                  let oc = open_out f in
                  output_string oc i;
                  close_out oc;
                  fname <- None)

    method private generate_frame =
      let buf = source#get_frame in
      self#still buf;
      buf
  end

let _ =
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:Modules.video "still_frame"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video
    ~descr:
      "Take still frames from a video source by calling the `save` method. For \
       now only bitmap output is supported."
    ~meth:
      Lang.
        [
          {
            name = "save";
            scheme = ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t);
            descr = "Save current image, argument is the file name to save to.";
            value =
              (fun s ->
                Lang.val_fun
                  [("", "", None)]
                  (fun p ->
                    s#save (List.assoc "" p |> Lang.to_string);
                    Lang.unit));
          };
        ]
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new still_frame ~name:"video.still_frame" s)
