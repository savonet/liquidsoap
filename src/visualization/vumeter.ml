(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

let screen_width = 80
let rms_max = -5.
let rms_min = -25.

let vol n v =
  let v = Sutils.dB_of_lin v in
  let ans = ref "" in
  let barwidth = screen_width / n - 7 in
  let barlen = int_of_float (float barwidth *. min 1. (max 0. ((v -. rms_min) /. (rms_max -. rms_min)))) in
    for i = 0 to barlen - 1 do ans := !ans ^ "=" done;
    for i = 0 to barwidth - barlen - 1 do ans := !ans ^ "." done;
    Printf.sprintf "% 5.1f %s" v !ans

class vumeter source scroll =
object
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let rms = AFrame.rms buf offset (AFrame.position buf - offset) in
    let vol = Array.map (vol (Array.length rms)) rms in
    let vol = Array.fold_left (fun ans s -> ans ^ "  " ^ s) "" vol in
    let vol = String.sub vol 2 (String.length vol - 2) in
      if scroll then
        Printf.printf "%s\n%!" vol
      else
        Printf.printf "\r%s%!" vol
end

let () =
  Lang.add_operator "vumeter"
    [ "scroll", Lang.bool_t, Some (Lang.bool false), Some "Scroll.";
      "", Lang.source_t, None, None ]
    ~category:Lang.Visualization
    ~descr:"VU meter (display the volume)."
    (fun p ->
       let f v = List.assoc v p in
       let scroll, src =
         Lang.to_bool (f "scroll"),
         Lang.to_source (f "")
       in
         ((new vumeter src scroll):>Source.source))
