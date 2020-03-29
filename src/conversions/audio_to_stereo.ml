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

(** These classes define conversion operators that accept a source streaming
  * at least one audio channel, and no other kind of channel, and return
  * a source that streams stereo audio.
  *
  * We only have a basic implementation for now. In the future we may
  * perform smart conversions from 5 channels audio. We may also want to
  * detect stereo with one silent channel (often the right) and treat
  * it as mono. *)

(** Duplicate mono into stereo, drop channels when there are more than two. *)
class basic ~kind source =
  object
    inherit Source.operator kind [source] ~name:"audio_to_stereo"

    inherit
      Conversion.base
        ~audio:true source
        ~converter:(fun ~frame tmp_frame ->
          (* Set audio layer. *)
          let audio =
            match Frame.(tmp_frame.content.audio) with
              | [||] -> assert false
              | [| chan |] -> [| chan; chan |]
              | audio -> Array.sub audio 0 2
          in
          Frame.set_audio frame audio)
  end

let () =
  let input_kind = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "audio_to_stereo" ~category:Lang.Conversions
    ~descr:"Convert any kind of audio source into a stereo source."
    ~kind:Lang.audio_stereo
    [("", Lang.source_t input_kind, None, None)]
    (fun p kind ->
      let s = new basic ~kind (Lang.to_source (List.assoc "" p)) in
      (s :> Source.source))
