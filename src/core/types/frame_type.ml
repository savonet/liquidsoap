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

module Type = Liquidsoap_lang.Type

let make ?pos base_type fields =
  Frame.Fields.fold
    (fun field field_type typ ->
      let field = Frame.Fields.string_of_field field in
      let meth =
        {
          Type.name = field;
          optional = false;
          scheme = ([], field_type);
          doc = { meth_descr = "Field " ^ field; category = `Method };
          json_name = None;
        }
      in
      Type.make ?pos (Type.Meth { meth; t = typ }))
    fields base_type

let internal_tracks ?pos () =
  Type.var ?pos ~constraints:[Format_type.internal_tracks] ()

let pcm_audio ?pos () = Type.var ?pos ~constraints:[Format_type.pcm_audio] ()

let set_field frame_type field field_type =
  let field = Frame.Fields.string_of_field field in
  let meth =
    {
      Type.name = field;
      optional = false;
      scheme = ([], field_type);
      doc = { meth_descr = "Field " ^ field; category = `Method };
      json_name = None;
    }
  in
  Type.make (Type.Meth { meth; t = frame_type })

let get_fields frame_type =
  let fields, _ = Type.split_meths frame_type in
  List.map (fun Type.{ name } -> Frame.Fields.register name) fields

let get_field frame_type field =
  let field = Frame.Fields.string_of_field field in
  let fields, _ = Type.split_meths frame_type in
  match
    List.find_map
      (fun Type.{ name = meth; scheme = _, field_type } ->
        if meth = field then Some field_type else None)
      fields
  with
    | Some v -> v
    | None -> raise Not_found

let content_type frame_type =
  let meths, base_type = Type.split_meths frame_type in
  let frame_type =
    match (meths, base_type.Type.descr) with
      (* If type is empty we add default formats. *)
      | [], Type.Var _ ->
          let audio =
            if Frame_settings.conf_audio_channels#get > 0 then
              Some
                (Format_type.audio_n ~pcm_kind:Content_audio.kind
                   Frame_settings.conf_audio_channels#get)
            else None
          in
          let video =
            if Frame_settings.conf_video_default#get then
              Some (Format_type.video ())
            else None
          in
          let default_t =
            make (Type.var ()) (Frame.Fields.make ?audio ?video ())
          in
          Typing.(frame_type <: default_t);
          default_t
      | _ ->
          (try
             (* Map audio and video fields to their default value when possible.
                This is in case the source has types such as { audio = 'a } *)
             Typing.satisfies_constraint base_type Format_type.internal_tracks;
             List.iter
               (function
                 | { Type.name = "audio"; scheme = [], ty } ->
                     Typing.(
                       ty <: Format_type.audio ~pcm_kind:Content_audio.kind ())
                 | { Type.name = "video"; scheme = [], ty } ->
                     Typing.(ty <: Format_type.video ())
                 | _ -> ())
               meths
           with _ -> ());
          frame_type
  in
  let meths, _ = Type.split_meths frame_type in
  let meths = List.filter (fun { Type.optional } -> not optional) meths in
  let content_type, resolved_frame_type =
    List.fold_left
      (fun (content_type, resolved_frame_type)
           ({ Type.name = field; scheme = _, ty } as meth) ->
        try
          let format = Format_type.content_type ty in
          let format_type = Type.make (Format_type.descr (`Format format)) in
          ( Frame.Fields.add (Frame.Fields.register field) format content_type,
            Type.make
              (Type.Meth
                 {
                   meth = { meth with Type.scheme = ([], format_type) };
                   t = resolved_frame_type;
                 }) )
        with Format_type.Never_type -> (content_type, resolved_frame_type))
      (Frame.Fields.empty, Type.make Type.unit)
      meths
  in
  Typing.(frame_type <: resolved_frame_type);
  content_type
