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
  let main_kind =
    Kind.of_kind
      (Frame.mk_fields
         ~audio:(if main_content = `Audio then `Any else Frame.none)
         ~video:(if main_content = `Video then `Any else Frame.none)
         ~midi:`Any ())
  in
  let main_output_kind =
    match main_content with
      | `Audio -> "audio_main"
      | `Video -> "video_main"
      | _ -> assert false
  in
  let main =
    new consumer
      ~write_frame:(write_to_buffer ~content:main_content g)
      ~name:main_output_kind ~kind:main_kind ~source:main_source ()
  in
  let aux_kind =
    Kind.of_kind
      (Frame.mk_fields
         ~audio:(if aux_content = `Audio then `Any else Frame.none)
         ~video:(if aux_content = `Video then `Any else Frame.none)
         ~midi:Frame.none ())
  in
  let aux_output_kind =
    match aux_content with
      | `Audio -> "audio_aux"
      | `Video -> "video_aux"
      | _ -> assert false
  in
  let aux =
    new consumer
      ~write_frame:(write_to_buffer ~content:aux_content g)
      ~name:aux_output_kind ~kind:aux_kind ~source:aux_source ()
  in
  let muxed_kind =
    Kind.of_kind
      (Frame.mk_fields
         ~audio:
           (if aux_content = `Audio then Kind.find_audio aux#kind
           else Kind.find_audio main#kind)
         ~video:
           (if aux_content = `Video then Kind.find_video aux#kind
           else Kind.find_video main#kind)
         ~midi:(Kind.find_midi main#kind) ())
  in
  let producer =
    new producer (* We are expecting real-rate with a couple of hickups.. *)
      ~check_self_sync:false ~consumers:[main; aux]
      ~kind:(Kind.of_kind Lang.any) ~name g
  in
  Kind.unify muxed_kind producer#kind;
  producer

let () =
  let return_t = Lang.content_t Lang.any in
  let main_t = Lang.set_field_t return_t Frame.video_field Lang.kind_none_t in
  let aux_t =
    Lang.fields_t
      (Frame.mk_fields ~audio:Lang.kind_none_t
         ~video:(Lang.get_field_t return_t Frame.video_field)
         ~midi:Lang.kind_none_t ())
  in
  Lang.add_operator "mux_video" ~category:`Conversion
    ~descr:
      "Add video channels to a stream. Track marks and metadata are taken from \
       both sources."
    ~return_t
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
  let kind = Lang.any in
  let out_t = Lang.content_t kind in
  let main_t = Lang.set_field_t out_t Frame.audio_field Lang.kind_none_t in
  let aux_t =
    Lang.fields_t
      (Frame.mk_fields
         ~audio:(Lang.get_field_t out_t Frame.audio_field)
         ~video:Lang.kind_none_t ~midi:Lang.kind_none_t ())
  in
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
