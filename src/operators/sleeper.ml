(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Source

class map ~kind source delay random freeze =
  let dt = AFrame.duration () in
  object
    inherit operator kind [source]

    val mutable lived = 0.

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private get_frame buf =
      source#get buf;
      let delay = delay +. Random.float random in
      Thread.delay delay;
      lived <- lived +. max dt delay;
      if freeze >= 0. && lived >= freeze then
        while true do
          Thread.delay 60.
        done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "sleeper"
    [
      ( "delay",
        Lang.float_t,
        Some (Lang.float 1.),
        Some
          "Amount of time to sleep at each frame, the unit being the frame \
           length." );
      ( "random",
        Lang.float_t,
        Some (Lang.float 0.),
        Some "Maximal random amount of time added (unit is frame length)." );
      ( "freeze",
        Lang.float_t,
        Some (Lang.float (-1.)),
        Some "Freeze after given amount of time (don't freeze if negative)." );
      ("", Lang.source_t k, None, None);
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
      let freeze = Lang.to_float (List.assoc "freeze" p) in
      let src = Lang.to_source (List.assoc "" p) in
      new map ~kind src delay random freeze)
