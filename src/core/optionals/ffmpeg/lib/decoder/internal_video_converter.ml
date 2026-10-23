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
module Scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

type stage = {
  format : Avutil.Video.frame_format;
  time_base : Avutil.rational;
  source_id : Source_id.t;
  fps : Helpers.Fps.t;
  scaler : Scaler.t;
  fitted_width : int;
  fitted_height : int;
}

type t = {
  log : Log.t;
  width : int;
  height : int;
  target : Avutil.Video.frame_format -> Avutil.Pixel_format.t;
  mutable stage : stage option;
}

let create ~log ~width ~height ~target () =
  { log; width; height; target; stage = None }

let build t ~time_base ~source_id (format : Avutil.Video.frame_format) =
  let fitted_width, fitted_height =
    Helpers.Fit.fitted_size ?source_aspect:format.pixel_aspect ~width:t.width
      ~height:t.height
      (format.width, format.height)
  in
  {
    format;
    time_base;
    source_id;
    fps =
      Helpers.Fps.create
        ~target:(Lazy.Mutexed.force Frame.video_rate)
        ~start:`None ~time_base format;
    scaler =
      Scaler.create
        ~threads:(max 0 Settings.scaling_threads#get)
        [Pixel_formats.scaling_flag ()]
        format.width format.height format.pixel_format fitted_width
        fitted_height (t.target format);
    fitted_width;
    fitted_height;
  }

let image t stage frame =
  let planes = Scaler.convert stage.scaler frame in
  let image =
    Image_pack.unpack ~width:stage.fitted_width ~height:stage.fitted_height
      planes
  in
  Video.Canvas.Image.make image
  |> Video.Canvas.Image.translate
       ((t.width - stage.fitted_width) / 2)
       ((t.height - stage.fitted_height) / 2)
  |> Video.Canvas.Image.viewport t.width t.height

let deliver t stage ~put frame = put (image t stage frame) frame

let flush t ~put =
  match t.stage with
    | Some stage -> Helpers.Fps.eof stage.fps (deliver t stage ~put)
    | None -> ()

let current t ~time_base ~source_id ~put format =
  let rebuild () =
    let s = build t ~time_base ~source_id format in
    t.stage <- Some s;
    s
  in
  match t.stage with
    | Some s
      when s.source_id = source_id && s.time_base = time_base
           && Pixel_formats.same_video s.format format ->
        s
    | Some _ ->
        t.log#important "Video format change to %s"
          (Pixel_formats.describe_video format);
        flush t ~put;
        rebuild ()
    | None -> rebuild ()

let convert ?(source_id = Source_id.none) ?format t ~time_base ~put frame =
  let format =
    match format with Some f -> f | None -> Avutil.Video.frame_format frame
  in
  let stage = current t ~time_base ~source_id ~put format in
  Helpers.Fps.convert stage.fps (deliver t stage ~put) frame
