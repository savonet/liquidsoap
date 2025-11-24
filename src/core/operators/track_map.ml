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

open Source

class track_map ~name ~field ~fn s =
  object
    inherit operator ~name [s]
    method fallible = s#fallible
    method remaining = s#remaining
    method abort_track = s#abort_track
    method effective_source = s#effective_source
    method self_sync = s#self_sync
    method private can_generate_frame = s#is_ready

    method private generate_frame =
      let buf = s#get_frame in
      Frame.set buf field (fn (Frame.get buf field))
  end

let to_pcm_s16 c =
  Content_pcm_s16.(lift_data (from_audio (Content.Audio.get_data c)))

let _ =
  let content_t = Lang.univ_t () in
  let input_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_audio.kind, content_t))))
  in
  let output_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_pcm_s16.kind, content_t))))
  in
  Lang.add_track_operator ~base:Modules.track_encode_audio "pcm_s16"
    [("", input_t, None, None)]
    ~category:`Audio
    ~descr:"Encode an audio track using PCM signed 16 bit integers."
    ~return_t:output_t
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      ( field,
        new track_map ~name:"track.encode.audio.pcm_s16" ~field ~fn:to_pcm_s16 s
      ))

let from_pcm_s16 c =
  Content.Audio.lift_data Content_pcm_s16.(to_audio (get_data c))

let _ =
  let content_t = Lang.univ_t () in
  let input_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_pcm_s16.kind, content_t))))
  in
  let output_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_audio.kind, content_t))))
  in
  Lang.add_track_operator ~base:Modules.track_decode_audio "pcm_s16"
    [("", input_t, None, None)]
    ~category:`Audio
    ~descr:"Decode an audio track using PCM signed 16 bit integers."
    ~return_t:output_t
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      ( field,
        new track_map
          ~name:"track.decode.audio.pcm_s16" ~field ~fn:from_pcm_s16 s ))

let to_pcm_f32 c =
  Content_pcm_f32.(lift_data (from_audio (Content.Audio.get_data c)))

let _ =
  let content_t = Lang.univ_t () in
  let input_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_audio.kind, content_t))))
  in
  let output_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_pcm_f32.kind, content_t))))
  in
  Lang.add_track_operator ~base:Modules.track_encode_audio "pcm_f32"
    [("", input_t, None, None)]
    ~category:`Audio
    ~descr:"Encode an audio track using PCM signed 16 bit integers."
    ~return_t:output_t
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      ( field,
        new track_map ~name:"track.encode.audio.pcm_f32" ~field ~fn:to_pcm_f32 s
      ))

let from_pcm_f32 c =
  Content.Audio.lift_data Content_pcm_f32.(to_audio (get_data c))

let _ =
  let content_t = Lang.univ_t () in
  let input_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_pcm_f32.kind, content_t))))
  in
  let output_t =
    Type.(
      make (Custom (Format_type.kind_handler (Content_audio.kind, content_t))))
  in
  Lang.add_track_operator ~base:Modules.track_decode_audio "pcm_f32"
    [("", input_t, None, None)]
    ~category:`Audio
    ~descr:"Decode an audio track using PCM signed 16 bit integers."
    ~return_t:output_t
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      ( field,
        new track_map
          ~name:"track.decode.audio.pcm_f32" ~field ~fn:from_pcm_f32 s ))
