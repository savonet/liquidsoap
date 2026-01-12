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

class compand ~field (source : source) mu =
  object (self)
    inherit operator ~name:"compand" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track

    method private generate_frame =
      let pos = source#frame_audio_position in
      let b = Content.Audio.get_data (source#get_mutable_content field) in
      for c = 0 to self#audio_channels - 1 do
        let b_c = b.(c) in
        for i = 0 to pos - 1 do
          (* Cf. http://en.wikipedia.org/wiki/Mu-law *)
          let sign = if b_c.(i) < 0. then -1. else 1. in
          b_c.(i) <-
            sign *. log (1. +. (mu *. Utils.abs_float b_c.(i))) /. log (1. +. mu)
        done
      done;
      source#set_frame_data field Content.Audio.lift_data b
  end

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "compand"
    [
      ("mu", Lang.float_t, Some (Lang.float 1.), None);
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"Compand the signal."
    (fun p ->
      let f v = List.assoc v p in
      let mu, (field, src) = (Lang.to_float (f "mu"), Lang.to_track (f "")) in
      (field, new compand ~field src mu))
