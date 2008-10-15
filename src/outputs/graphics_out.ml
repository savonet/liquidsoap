(*****************************************************************************

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


class output source start =
  let video_channels = Fmt.video_channels () in
  let video_width = Fmt.video_width () in
  let video_height = Fmt.video_height () in
object (self)
  inherit Output.output ~name:"graphics" ~kind:"output.graphics" source start

  val mutable sleep = false
  method output_stop =
    sleep <- true

  method output_start =
    if video_channels > 0 then
      (
        Graphics.open_graph "";
        Graphics.set_window_title "Liquidsoap";
        Graphics.resize_window video_width video_height
      );
    sleep <- false

  method output_send buf =
    if video_channels > 0 then
      let rgb = VFrame.get_rgb buf in
        for frame = 0 to 0 (* Array.length rgb.(0) - 1 *) do
          let img = RGB.to_int_image rgb.(0).(frame) in
          let img = Graphics.make_image img in
            Graphics.draw_image img 0 0
        done

  method output_reset = ()
end

let () =
  Lang.add_operator "output.graphics"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Display video stream using the Graphics library."
    (fun p ->
       let start = Lang.to_bool (List.assoc "start" p) in
       let source = List.assoc "" p in
         ((new output source start):>Source.source))
