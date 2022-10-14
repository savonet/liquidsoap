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

(** Muxing takes a main and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the main has no channel of that kind, anything for the others. *)

open Producer_consumer

let create ~name ~main_source ~main_content ~aux_source ~aux_content () =
  let g = Generator.create `Both in
  let main_frame_t =
    Lang.frame_t Lang.unit_t
      (Frame.mk_fields
         ?audio:(if main_content = `Audio then Some (Lang.univ_t ()) else None)
         ?video:(if main_content = `Video then Some (Lang.univ_t ()) else None)
         ())
  in
  let main_name =
    match main_content with
      | `Audio -> "audio_main"
      | `Video -> "video_main"
      | _ -> assert false
  in
  let main =
    new consumer
      ~write_frame:(write_to_buffer ~content:main_content g)
      ~name:main_name ~source:main_source ()
  in
  Typing.(main#frame_type <: main_frame_t);
  Typing.(main_frame_t <: main#frame_type);
  let aux_frame_t =
    Lang.frame_t Lang.unit_t
      (Frame.mk_fields
         ?audio:(if aux_content = `Audio then Some (Lang.univ_t ()) else None)
         ?video:(if aux_content = `Video then Some (Lang.univ_t ()) else None)
         ())
  in
  let aux_name =
    match aux_content with
      | `Audio -> "audio_aux"
      | `Video -> "video_aux"
      | _ -> assert false
  in
  let aux =
    new consumer
      ~write_frame:(write_to_buffer ~content:aux_content g)
      ~name:aux_name ~source:aux_source ()
  in
  Typing.(aux#frame_type <: aux_frame_t);
  Typing.(aux_frame_t <: aux#frame_type);
  let muxed_frame_t =
    Lang.frame_t main_frame_t
      (Frame.mk_fields
         ?audio:
           (if aux_content = `Audio then
            Some (Frame_type.get_field aux_frame_t Frame.audio_field)
           else None)
         ?video:
           (if aux_content = `Video then
            Some (Frame_type.get_field aux_frame_t Frame.video_field)
           else None)
         ())
  in
  let producer =
    new producer (* We are expecting real-rate with a couple of hickups.. *)
      ~check_self_sync:false ~consumers:[main; aux] ~name g
  in
  Typing.(producer#frame_type <: muxed_frame_t);
  Typing.(muxed_frame_t <: producer#frame_type);
  producer

let () =
  let video_type = Lang.univ_t () in
  let main_t = Lang.univ_t () in
  let aux_t =
    Lang.frame_t (Lang.univ_t ()) (Frame.mk_fields ~video:video_type ())
  in
  let out_t = Frame_type.set_field main_t Frame.video_field video_type in
  Lang.add_operator "mux_video" ~category:`Conversion
    ~descr:
      "Add video channels to a stream. Track marks and metadata are taken from \
       both sources."
    ~return_t:out_t
    [
      ("video", Lang.source_t aux_t, None, None);
      ("", Lang.source_t main_t, None, None);
    ]
    (fun p ->
      let main_source = List.assoc "" p in
      let main_content = `Audio in
      let aux_source = List.assoc "video" p in
      let aux_content = `Video in
      create ~name:"mux_video" ~main_source ~main_content ~aux_source
        ~aux_content ())

let () =
  let audio_type = Lang.univ_t () in
  let main_t = Lang.univ_t () in
  let aux_t =
    Lang.frame_t (Lang.univ_t ()) (Frame.mk_fields ~audio:audio_type ())
  in
  let out_t = Frame_type.set_field main_t Frame.audio_field audio_type in
  Lang.add_operator "mux_audio" ~category:`Conversion
    ~descr:
      "Mux an audio stream into an audio-free stream. Track marks and metadata \
       are taken from both sources."
    ~return_t:out_t
    [
      ("audio", Lang.source_t aux_t, None, None);
      ("", Lang.source_t main_t, None, None);
    ]
    (fun p ->
      let main_source = List.assoc "" p in
      let main_content = `Video in
      let aux_source = List.assoc "audio" p in
      let aux_content = `Audio in
      create ~name:"mux_audio" ~main_source ~main_content ~aux_source
        ~aux_content ())
