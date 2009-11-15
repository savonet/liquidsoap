(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

open Source

class visu ~kind source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val width = Lazy.force Frame.video_width
  val height = Lazy.force Frame.video_height

  val vol = Array.init channels (fun _ -> Array.make (Lazy.force Frame.video_width) 0.)

  method add_vol v =
    for c = 0 to channels - 1 do
      for i = 1 to width - 1 do
        vol.(c).(i - 1) <- vol.(c).(i)
      done;
      vol.(c).(width - 1) <- v.(c)
    done

  method render buf =
    let clip y =
      max 0 (min height y)
    in
    let pts =
      Array.mapi
        (fun j v ->
           Array.mapi
             (fun i x ->
                i,
                clip (height - (int_of_float (x *. float height) / channels + j * height / channels))
             ) v
        ) vol
    in
    let pts = Array.concat (Array.to_list pts) in
    let buf = VFrame.get_rgb buf in
      for f = 0 to Array.length buf - 1 do
        for c = 0 to Array.length buf.(f) - 1 do
          let buf_c = buf.(f).(c) in
            RGB.blank buf_c;
            Array.iter (fun (i,j) -> RGB.set_pixel buf_c i j (0xff, 0xff, 0xff, 0xff)) pts
        done
      done

  method get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let rms = AFrame.rms buf offset (AFrame.position buf - offset) in
      self#add_vol rms;
      self#render buf
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  Lang.add_operator "video.volume"
    [ "", Lang.source_t k, None, None ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Visualization
    ~descr:"Graphical visualization of the sound."
    (fun p kind ->
       let f v = List.assoc v p in
       let src =
         Lang.to_source (f "")
       in
         ((new visu ~kind src):>Source.source))
