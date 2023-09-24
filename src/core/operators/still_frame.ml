(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method seek_source = source
    method self_sync = source#self_sync
    method private _is_ready = source#is_ready
    method abort_track = source#abort_track
    val mutable fname = None

    method save (f : string) =
      if not (String.ends_with f ".bmp") then
        self#log#severe
          "Only BMP files are supported for now, the filename should end with \
           .bmp"
      else fname <- Some f

    method private get_frame buf =
      match fname with
        | None -> source#get buf
        | Some f -> (
            let v = VFrame.get_content buf source in
            match v with
              | Some (v, off, _) ->
                  let v = Content.Video.get_data v in
                  let i = Video.Canvas.get v off in
                  let i =
                    i |> Video.Canvas.Image.render |> Image.YUV420.to_RGBA32
                    |> Image.RGBA32.to_BMP
                  in
                  let oc = open_out f in
                  output_string oc i;
                  close_out oc;
                  fname <- None
              | None -> ())
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
      [
        ( "save",
          ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
          "Save current image, argument is the file name to save to.",
          fun s ->
            Lang.val_fun
              [("", "", None)]
              (fun p ->
                s#save (List.assoc "" p |> Lang.to_string);
                Lang.unit) );
      ]
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new still_frame ~name:"video.still_frame" s)
