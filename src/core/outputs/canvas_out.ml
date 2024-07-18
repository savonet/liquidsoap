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
open OcamlCanvas.V1
module Queue = Liquidsoap_lang.Queues.Queue

let events = Queue.create ()
let retain_event e = Queue.push events e
let init = Lazy.from_fun Backend.init

class output ~infallible ~register_telnet ~autostart ~on_start ~on_stop source =
  object (self)
    inherit
      Output.output
        ~name:"canvas" ~output_kind:"output.canvas" ~register_telnet ~infallible
          ~on_start ~on_stop source autostart

    val mutable sleep = false
    method stop = ()
    val mutable canvas = None
    val mutable img = None

    method update =
      match img with
        | Some img' ->
            let width, height = self#video_dimensions in
            (* TODO: directly output a bigarray and use ImageData.of_bigarray *)
            let img =
              let img = ImageData.create (width, height) in
              for j = 0 to height - 1 do
                for i = 0 to width - 1 do
                  let r, g, b, _ = Image.YUV420.get_pixel_rgba img' i j in
                  ImageData.putPixel img (i, j) (Color.of_rgb r g b)
                done
              done;
              img
            in
            Canvas.putImageData (Option.get canvas) ~dpos:(0, 0) img
              ~spos:(0, 0) ~size:(width, height)
        | None -> ()

    method start =
      let width, height = self#video_dimensions in
      let c =
        Canvas.createOnscreen ~autocommit:true ~title:"Liquidsoap"
          ~size:(width, height) ()
      in
      canvas <- Some c;
      Canvas.show c;
      React.E.map (fun _ -> self#update) Event.frame |> retain_event;
      ignore (Thread.create (fun () -> Backend.run (fun () -> ())) ())

    method send_frame buf =
      let width, height = self#video_dimensions in
      match (VFrame.data buf).Content.Video.data with
        | [] -> ()
        | (_, i) :: _ ->
            let i =
              i
              |> Video.Canvas.Image.viewport width height
              |> Video.Canvas.Image.render ~transparent:false
            in
            img <- Some i

    method! reset = ()
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:Modules.output "canvas"
    (Output.proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:Output.meth
    ~descr:"Display video stream using the Canvas library."
    (fun p ->
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
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
      (new output
         ~infallible ~register_telnet ~autostart ~on_start ~on_stop source
        :> Output.output))
