(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

class map ~field source f =
  object
    inherit operator ~name:"audio.map" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track

    method private generate_frame =
      let b = Content.Audio.get_data (source#get_mutable_content field) in
      for i = 0 to source#frame_audio_position - 1 do
        for c = 0 to Array.length b - 1 do
          b.(c).(i) <- f b.(c).(i)
        done
      done;
      source#set_frame_data field Content.Audio.lift_data b
  end

let to_fun_float f x = Lang.to_float (Lang.apply f [("", Lang.float x)])

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "map"
    [
      ("", Lang.fun_t [(false, "", Lang.float_t)] Lang.float_t, None, None);
      ("", frame_t, None, None);
    ]
    ~return_t:frame_t
    ~descr:"Map a function to all audio samples. This is SLOW!" ~category:`Audio
    ~flags:[`Experimental] (* It works well but is probably useless. *)
    (fun p ->
      let f = to_fun_float (Lang.assoc "" 1 p) in
      let field, src = Lang.to_track (Lang.assoc "" 2 p) in
      (field, new map ~field src f))
