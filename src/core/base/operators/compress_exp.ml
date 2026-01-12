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

class compress ~field (source : source) mu =
  object (self)
    inherit operator ~name:"compress" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method effective_source = source#effective_source
    method self_sync = source#self_sync

    method private generate_frame =
      let b = Content.Audio.get_data (source#get_mutable_content field) in
      for c = 0 to self#audio_channels - 1 do
        let b_c = b.(c) in
        for i = 0 to source#frame_audio_position - 1 do
          let x = b_c.(i) in
          let sign = if x < 0. then -1. else 1. in
          b_c.(i) <- sign *. (1. -. ((1. -. Utils.abs_float x) ** mu))
        done
      done;
      source#set_frame_data field Content.Audio.lift_data b
  end

let _ =
  let return_t = Format_type.audio () in
  Lang.add_track_operator ~base:Compress.audio_compress "exponential"
    ~category:`Audio ~descr:"Exponential compressor."
    [
      ( "mu",
        Lang.float_t,
        Some (Lang.float 2.),
        Some "Exponential compression factor, typically greater than 1." );
      ("", return_t, None, None);
    ]
    ~return_t
    (fun p ->
      let f v = List.assoc v p in
      let mu, (field, src) = (Lang.to_float (f "mu"), Track.of_value (f "")) in
      (field, new compress ~field src mu))
