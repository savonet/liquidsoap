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

    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    val stereo = { Frame.audio = 2; video = 0; midi = 0 }

    method private get_frame frame =
      let start = Frame.position frame in
      let layers =
        source#get frame;
        Frame.get_content_layers frame
      in
      (* Install the final stereo layer, and copy everything to it *)
      let dst = Frame.content_of_type frame start stereo in
      let aux { Frame.content = src; start = pos; length = l } =
        if pos >= start then (
          assert (src.Frame.video = [||] && src.Frame.midi = [||]);
          match src.Frame.audio with
            | [||] -> assert false
            | [| chan |] ->
                let content = { src with Frame.audio = [| chan; chan |] } in
                Frame.blit_content content pos dst pos l
            | [| _; _ |] -> Frame.blit_content src pos dst pos l
            | audio ->
                let content = { src with Frame.audio = Array.sub audio 0 2 } in
                Frame.blit_content content pos dst pos l )
      in
      List.iter aux layers
  end

let () =
  let input_kind = Lang.kind_type_of_kind_format Lang.audio_variable in
  Lang.add_operator "audio_to_stereo" ~category:Lang.Conversions
    ~descr:"Convert any kind of audio source into a stereo source."
    ~kind:Lang.audio_stereo
    [("", Lang.source_t input_kind, None, None)]
    (fun p kind -> new basic ~kind (Lang.to_source (List.assoc "" p)))
