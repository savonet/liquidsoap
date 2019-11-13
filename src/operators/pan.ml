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

class pan ~kind (source : source) phi phi_0 =
  object
    inherit operator ~name:"pan" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method seek = source#seek

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf ;
      let buffer = AFrame.content_of_type buf ~channels:2 offset in
      (* Degrees to radians + half field. *)
      let phi_0 = phi_0 () *. Utils.pi /. 360. in
      (* Map -1 / 1 to radians. *)
      let phi = phi () *. phi_0 in
      let gain_left = (tan phi_0 +. tan phi) /. 2. in
      let gain_right = (tan phi_0 -. tan phi) /. 2. in
      let len = AFrame.position buf - offset in
      Audio.Mono.amplify gain_left (Audio.Mono.sub buffer.(0) offset len) ;
      Audio.Mono.amplify gain_right (Audio.Mono.sub buffer.(1) offset len)
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_stereo in
  Lang.add_operator "stereo.pan"
    [ ( "pan",
        Lang.float_getter_t (),
        Some (Lang.float 0.),
        Some "Pan ranges between -1 and 1." );
      ( "field",
        Lang.float_getter_t (),
        Some (Lang.float 90.),
        Some "Field width in degrees (between 0 and 90)." );
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Pan a stereo sound."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let phi_0 = Lang.to_float_getter (Lang.assoc "field" 1 p) in
      let phi = Lang.to_float_getter (Lang.assoc "pan" 1 p) in
      new pan ~kind s phi phi_0)
