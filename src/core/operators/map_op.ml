(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

open Source

class map source f =
  object
    inherit operator ~name:"audio.map" [source]
    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method self_sync = source#self_sync
    method is_ready = source#is_ready
    method abort_track = source#abort_track

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      for i = offset to AFrame.position buf - 1 do
        for c = 0 to Array.length b - 1 do
          b.(c).(i) <- f b.(c).(i)
        done
      done
  end

let to_fun_float f x = Lang.to_float (Lang.apply f [("", Lang.float x)])

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.audio "map"
    [
      ("", Lang.fun_t [(false, "", Lang.float_t)] Lang.float_t, None, None);
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t
    ~descr:"Map a function to all audio samples. This is SLOW!" ~category:`Audio
    ~flags:[`Experimental] (* It works well but is probably useless. *)
    (fun p ->
      let f = to_fun_float (Lang.assoc "" 1 p) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new map src f)
