(*****************************************************************************

  Copyright 2003-2022 Savonet team

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

class output ~infallible ~autostart ~on_start ~on_stop source =
  object (self)
    inherit
      Output.output
        ~name:"graphics" ~output_kind:"output.graphics" ~infallible ~on_start
          ~on_stop source autostart

    val mutable sleep = false
    method stop = sleep <- true

    method start =
      let width, height = self#video_dimensions in
      Graphics.open_graph "";
      Graphics.set_window_title "Liquidsoap";
      Graphics.resize_window width height;
      sleep <- false

    method send_frame buf =
      let img =
        let width, height = self#video_dimensions in
        Video.Canvas.get (VFrame.data buf) 0
        |> Video.Canvas.Image.viewport width height
        |> Video.Canvas.Image.render ~transparent:false
        |> Image.YUV420.to_int_image |> Graphics.make_image
      in
      Graphics.draw_image img 0 0

    method reset = ()
  end

let () =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator "output.graphics"
    (Output.proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:Output.meth
    ~descr:"Display video stream using the Graphics library."
    (fun p ->
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let source = List.assoc "" p in
      (new output ~infallible ~autostart ~on_start ~on_stop source
        :> Output.output))
