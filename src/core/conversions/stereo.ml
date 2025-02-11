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

(** These classes define conversion operators that accept a source streaming at
    least one audio channel, and no other kind of channel, and return a source
    that streams stereo audio. * We only have a basic implementation for now. In
    the future we may perform smart conversions from 5 channels audio. We may
    also want to detect stereo with one silent channel (often the right) and
    treat it as mono. *)

(** Duplicate mono into stereo, drop channels when there are more than two. *)
class basic ~field source =
  object
    inherit Source.operator [source] ~name:"stereo"

    inherit
      Conversion.base
        ~converter:(fun frame ->
          (* Set audio layer. *)
          let audio =
            match Content.Audio.get_data (Frame.get frame field) with
              | [||] ->
                  let len = AFrame.size () in
                  let buf = Audio.Mono.create len in
                  Audio.Mono.clear buf 0 len;
                  [| buf; buf |]
              | [| chan |] -> [| chan; chan |]
              | audio -> Array.sub audio 0 2
          in
          Frame.set_data frame field Content.Audio.lift_data audio)
        source
  end

let stereo =
  let input_type = Format_type.audio () in
  let output_type = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:Modules.track_audio "stereo"
    ~category:`Conversion
    ~descr:"Convert any pcm audio track into a stereo track."
    ~return_t:output_type
    [("", input_type, None, None)]
    (fun p ->
      let field, s = Lang.to_track (List.assoc "" p) in
      (field, new basic ~field s))
