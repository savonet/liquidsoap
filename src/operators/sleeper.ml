(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

let minisleep t = ignore (Unix.select [] [] [] t)

class map ~kind source delay random =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method remaining = source#remaining
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method private get_frame buf =
    source#get buf;
    let delay = delay +. Random.float random in
    minisleep delay
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "sleeper"
    [
      "delay", Lang.float_t, Some (Lang.float 1.), Some "Amount of time to sleep at each frame, the unit being the frame length.";
      "random", Lang.float_t, Some (Lang.float 0.), Some "Maximal random amount of time added (unit is frame length).";
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~descr:"Sleep at each frame. Useful for emulating network delays, etc."
    ~category:Lang.SoundProcessing
    ~flags:[Lang.Hidden; Lang.Experimental]
    (fun p kind ->
      let delay = Lang.to_float (List.assoc "delay" p) in
      let delay = AFrame.duration () *. delay in
      let random = Lang.to_float (List.assoc "random" p) in
      let random = AFrame.duration () *. random in
      let src = Lang.to_source (List.assoc "" p) in
      new map ~kind src delay random)
