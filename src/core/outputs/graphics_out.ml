(*****************************************************************************

  Copyright 2003-2024 Savonet team

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

class output ~infallible ~register_telnet ~autostart source_val =
  let source = Lang.to_source source_val in
  object (self)
    inherit
      Output.output
        ~name:"graphics" ~output_kind:"output.graphics" ~infallible
          ~register_telnet source_val autostart

    val mutable sleep = false
    method stop = sleep <- true

    method start =
      let width, height = self#video_dimensions in
      Graphics.open_graph "";
      Graphics.set_window_title "Liquidsoap";
      Graphics.resize_window width height;
      sleep <- false

    method self_sync = source#self_sync

    method send_frame buf =
      match (VFrame.data buf).Content.Video.data with
        | [] -> ()
        | (_, img) :: _ ->
            let width, height = self#video_dimensions in
            let img =
              img
              |> Video.Canvas.Image.viewport width height
              |> Video.Canvas.Image.render ~transparent:false
              |> Image.YUV420.to_int_image |> Graphics.make_image
            in
            Graphics.draw_image img 0 0

    method! reset = ()
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:Modules.output "graphics"
    (Output.proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
    ~descr:"Display video stream using the Graphics library."
    (fun p ->
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let source = List.assoc "" p in
      (new output ~infallible ~register_telnet ~autostart source
        :> Output.output))
