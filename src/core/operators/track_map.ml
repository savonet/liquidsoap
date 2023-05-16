(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  object (self)
    inherit operator ~name [s]
    method stype = s#stype
    method remaining = s#remaining
    method abort_track = s#abort_track
    method seek = s#seek
    method self_sync = s#self_sync
    method is_ready = s#is_ready
    val mutable tmp_frame = None

    method tmp_frame =
      match tmp_frame with
        | Some b -> b
        | None ->
            let format = Frame.Fields.find field s#content_type in
            let f =
              Frame.create (Frame.Fields.add field format Frame.Fields.empty)
            in
            tmp_frame <- Some f;
            f

    method private get_frame buf =
      let tmp_frame = self#tmp_frame in
      let start_pos = Frame.position buf in
      assert (start_pos = Frame.position tmp_frame);
      s#get tmp_frame;
      let end_pos = Frame.position tmp_frame in
      let len = end_pos - start_pos in
      let src = fn (Content.sub (Frame.get tmp_frame field) start_pos len) in
      Content.blit src 0 (Frame.get buf field) start_pos len;
      List.iter
        (fun (pos, m) ->
          if start_pos <= pos && pos < end_pos then
            Generator.add_metadata ~pos buf m)
        (Frame.get_all_metadata tmp_frame);
      Frame.add_break buf end_pos

    initializer self#on_after_output (fun () -> Frame.clear self#tmp_frame)
  end

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
      let fn c =
        Content_pcm_s16.(lift_data (from_audio (Content.Audio.get_data c)))
      in
      (field, new track_map ~name:"track.encode.audio.pcm_s16" ~field ~fn s))

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
      let fn c =
        Content.Audio.lift_data Content_pcm_s16.(to_audio (get_data c))
      in
      (field, new track_map ~name:"track.decode.audio.pcm_s16" ~field ~fn s))

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
      let fn c =
        Content_pcm_f32.(lift_data (from_audio (Content.Audio.get_data c)))
      in
      (field, new track_map ~name:"track.encode.audio.pcm_f32" ~field ~fn s))

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
      let fn c =
        Content.Audio.lift_data Content_pcm_f32.(to_audio (get_data c))
      in
      (field, new track_map ~name:"track.decode.audio.pcm_f32" ~field ~fn s))
