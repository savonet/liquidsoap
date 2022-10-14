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

open Mm

(** These classes define conversion operators that accept a source streaming
  * at least one audio channel, and no other kind of channel, and return
  * a source that streams stereo audio.
  *
  * We only have a basic implementation for now. In the future we may
  * perform smart conversions from 5 channels audio. We may also want to
  * detect stereo with one silent channel (often the right) and treat
  * it as mono. *)

(** Duplicate mono into stereo, drop channels when there are more than two. *)
class basic source =
  object
    inherit Source.operator [source] ~name:"audio_to_stereo"

    inherit
      Conversion.base
        ~audio:true source
        ~converter:(fun ~frame tmp_frame ->
          (* Set audio layer. *)
          let audio =
            match AFrame.pcm tmp_frame with
              | [||] ->
                  let len = AFrame.size () in
                  let buf = Audio.Mono.create len in
                  Audio.Mono.clear buf 0 len;
                  [| buf; buf |]
              | [| chan |] -> [| chan; chan |]
              | audio -> Array.sub audio 0 2
          in
          Frame.set_audio frame (Content.Audio.lift_data audio))
  end

let () =
  let input_kind = Lang.audio_pcm in
  let input_type = Lang.frame_kind_t input_kind in
  let fields = Lang.of_frame_t input_type in
  let output_type =
    Lang.frame_t (Frame.set_audio_field fields (Lang.kind_t Frame.audio_stereo))
  in
  Lang.add_operator "audio_to_stereo" ~category:`Conversion
    ~descr:"Convert any pcm audio source into a stereo source."
    ~return_t:output_type
    [("", Lang.source_t input_type, None, None)]
    (fun p -> new basic (Lang.to_source (List.assoc "" p)))
