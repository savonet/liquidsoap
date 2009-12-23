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

(** VU meter. *)

open Source

let backpoints = 200

class vumeter ~kind source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit operator kind [source] as super
  inherit GLvisualizer.base "Liquidsoap's volume"

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val vol = Array.init channels (fun _ -> Array.make backpoints 0.)

  method add_vol v =
    for c = 0 to channels - 1 do
      for i = 1 to backpoints - 1 do
        vol.(c).(i - 1) <- vol.(c).(i)
      done;
      vol.(c).(backpoints - 1) <- v.(c)
    done

  method render =
    let scalex x =
      2. *. float (x - backpoints / 2) /. float backpoints
    in
    let pts =
      Array.mapi
        (fun j v ->
           Array.mapi
             (fun i x ->
                scalex i,
                float (j - channels / 2) +. x
             ) v
        ) vol
    in
      GlClear.clear [ `color ];
      GlMat.load_identity ();
      GlDraw.color (1.0, 0.0, 0.0);
      GlDraw.begins `lines;

      Array.iter
        (fun pts ->
           Array.iter
             (fun p ->
                GlDraw.vertex2 p
             ) pts
        ) pts;
      GlDraw.ends ()

  method get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let rms = AFrame.rms buf offset (AFrame.position buf - offset) in
    self#add_vol rms
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "visu.glvolume"
    [ "", Lang.source_t k, None, None ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Visualization
    ~descr:"Graphical visualization of the volume using openGL."
    (fun p kind ->
       let f v = List.assoc v p in
       let src =
         Lang.to_source (f "")
       in
         ((new vumeter ~kind src):>Source.source))
