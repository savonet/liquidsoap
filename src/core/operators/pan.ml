(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm
open Source

class pan ~field (source : source) phi phi_0 =
  object
    inherit operator ~name:"pan" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private generate_frame =
      let buffer = Content.Audio.get_data (source#get_mutable_content field) in
      (* Degrees to radians + half field. *)
      let phi_0 = phi_0 () *. Float.pi /. 360. in
      (* Map -1 / 1 to radians. *)
      let phi = phi () *. phi_0 in
      let gain_left = (tan phi_0 -. tan phi) /. 2. in
      let gain_right = (tan phi_0 +. tan phi) /. 2. in
      let len = source#frame_audio_position in
      Audio.Mono.amplify gain_left buffer.(0) 0 len;
      Audio.Mono.amplify gain_right buffer.(1) 0 len;
      source#set_frame_data field Content.Audio.lift_data buffer
  end

let _ =
  let track_t = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:Stereo.stereo "pan"
    [
      ( "field",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 90.),
        Some "Field width in degrees (between 0 and 90)." );
      ( "",
        Lang.getter_t Lang.float_t,
        None,
        Some
          "Pan value. Should be between `-1` (left side) and `1` (right side)."
      );
      ("", track_t, None, None);
    ]
    ~return_t:track_t ~category:`Audio ~descr:"Pan a stereo sound."
    (fun p ->
      let phi_0 = Lang.to_float_getter (List.assoc "field" p) in
      let phi = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let field, s = Lang.to_track (Lang.assoc "" 2 p) in
      (field, new pan ~field s phi phi_0))
