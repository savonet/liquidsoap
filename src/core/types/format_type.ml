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

type Type.custom += Kind of (Content_base.kind * Type.t)
type Type.custom += Format of Content_base.format
type Type.constr_t += InternalMedia | Media
type descr = [ `Format of Content_base.format | `Kind of Content_base.kind ]

let get_format = function Format f -> f | _ -> assert false
let get_kind = function Kind k -> k | _ -> assert false

let format_handler f =
  {
    Type.typ = Format f;
    copy_with = (fun _ f -> Format (Content_base.duplicate (get_format f)));
    occur_check = (fun _ _ -> ());
    filter_vars =
      (fun _ l f ->
        ignore (get_format f);
        l);
    repr =
      (fun _ _ f -> `Constr (Content_base.string_of_format (get_format f), []));
    subtype = (fun _ f f' -> Content_base.merge (get_format f) (get_format f'));
    sup =
      (fun _ f f' ->
        Content_base.merge (get_format f) (get_format f');
        f);
    to_string = (fun f -> Content_base.string_of_format (get_format f));
    to_json = (fun f -> Content_base.json_of_format (get_format f));
  }

let format_descr f = Type.Custom (format_handler f)

let string_of_kind (k, ty) =
  match (Type.deref ty).Type.descr with
    | Type.(Custom { typ = Format f }) -> Content_base.string_of_format f
    | _ ->
        Printf.sprintf "%s(%s)"
          (Content_base.string_of_kind k)
          (Type.to_string ty)

let repr_of_kind repr l (k, ty) =
  match (Type.deref ty).Type.descr with
    | Type.(Custom { typ = Format f }) ->
        `Constr (Content_base.string_of_format f, [])
    | _ -> `Constr (Content_base.string_of_kind k, [(`Covariant, repr l ty)])

let json_of_kind (_k, ty) =
  match (Type.deref ty).Type.descr with
    | Type.(Custom { typ = Format f }) -> Content_base.json_of_format f
    | _ -> failwith "TODO"

let kind_handler k =
  {
    Type.typ = Kind k;
    copy_with =
      (fun copy_with k ->
        let k, ty = get_kind k in
        Kind (k, copy_with ty));
    occur_check =
      (fun occur_check k ->
        let _, ty = get_kind k in
        occur_check ty);
    filter_vars =
      (fun filter_vars l k ->
        let _, ty = get_kind k in
        filter_vars l ty);
    repr = (fun repr l k -> repr_of_kind repr l (get_kind k));
    subtype =
      (fun subtype k k' ->
        let k, t = get_kind k in
        let k', t' = get_kind k' in
        assert (k = k');
        subtype t t');
    sup =
      (fun sup k k' ->
        let k, t = get_kind k in
        let k', t' = get_kind k' in
        assert (k = k');
        Kind (k, sup t t'));
    to_string = (fun k -> string_of_kind (get_kind k));
    to_json = (fun k -> json_of_kind (get_kind k));
  }

let descr descr =
  let k =
    match descr with
      | `Format f -> (Content_base.kind f, Type.make (format_descr f))
      | `Kind k -> (k, Type.var ())
  in
  Type.Custom (kind_handler k)

let rec content_type ~default ty =
  match (Type.demeth ty).Type.descr with
    | Type.Custom { Type.typ = Kind (k, ty) } ->
        content_type ~default:(fun () -> Content_base.default_format k) ty
    | Type.Custom { Type.typ = Format f } -> f
    | Type.Var _ -> default ()
    | _ -> assert false

let internal_media =
  {
    Type.t = InternalMedia;
    constr_descr =
      Printf.sprintf "an internal media type (%s or %s)"
        (Content_base.string_of_kind Content_audio.kind)
        (Content_base.string_of_kind Content_video.kind);
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        match (Type.deref b).Type.descr with
          | Type.Tuple [] -> ()
          | Type.Constr { params } ->
              List.iter (fun (_, typ) -> satisfies typ) params
          | Type.Meth _ ->
              let meths, base_type = Type.split_meths b in
              List.iter
                (fun Type.{ scheme = _, field_type } -> satisfies field_type)
                meths;
              satisfies base_type
          | Type.Custom { Type.typ = Kind (k, _) } when Content_audio.is_kind k
            ->
              ()
          | Type.Custom { Type.typ = Kind (k, _) } when Content_video.is_kind k
            ->
              ()
          | Type.Custom { Type.typ = Format f } when Content_audio.is_format f
            ->
              ()
          | Type.Custom { Type.typ = Format f } when Content_video.is_format f
            ->
              ()
          | _ -> raise Type.Unsatisfied_constraint);
  }

let media ~strict () =
  {
    Type.t = Media;
    constr_descr = "any media type (pcm, etc...)";
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        match (Type.deref b).Type.descr with
          | Type.Tuple [] -> ()
          | Type.Constr { params } when not strict ->
              List.iter (fun (_, typ) -> satisfies typ) params
          | Type.Meth _ when not strict ->
              let meths, base_type = Type.split_meths b in
              List.iter
                (fun Type.{ scheme = _, field_type } -> satisfies field_type)
                meths;
              satisfies base_type
          | Type.Custom { Type.typ = Kind _ }
          | Type.Custom { Type.typ = Format _ } ->
              ()
          | _ -> raise Type.Unsatisfied_constraint);
  }

let content_type = content_type ~default:(fun _ -> assert false)
let audio () = Type.make (descr (`Kind Content_audio.kind))

let () =
  Type.register_type (Content_base.string_of_kind Content_audio.kind) (fun () ->
      Type.make (Type.Custom (kind_handler (Content_audio.kind, Type.var ()))))

let audio_mono () =
  Type.make
    (descr
       (`Format Content_audio.(lift_params { channel_layout = lazy `Mono })))

let audio_stereo () =
  Type.make
    (descr
       (`Format Content_audio.(lift_params { channel_layout = lazy `Stereo })))

let audio_n n =
  Type.make
    (descr
       (`Format
         Content_audio.(
           lift_params
             {
               channel_layout =
                 lazy (Audio_converter.Channel_layout.layout_of_channels n);
             })))

let video () = Type.make (descr (`Kind Content_video.kind))

let () =
  Type.register_type (Content_base.string_of_kind Content_video.kind) (fun () ->
      Type.make (Type.Custom (kind_handler (Content_video.kind, Type.var ()))))

let midi () = Type.make (descr (`Kind Content_midi.kind))

let () =
  Type.register_type (Content_base.string_of_kind Content_midi.kind) (fun () ->
      Type.make (Type.Custom (kind_handler (Content_midi.kind, Type.var ()))))

let midi_n n =
  Type.make (descr (`Format Content_midi.(lift_params { channels = n })))

let track_marks = Type.make (descr (`Format Content_timed.Track_marks.format))

let () =
  Type.register_type "track_marks" (fun () ->
      Type.make (Type.Custom (format_handler Content_timed.Track_marks.format)))

let metadata = Type.make (descr (`Format Content_timed.Metadata.format))

let () =
  Type.register_type "metadata" (fun () ->
      Type.make (Type.Custom (format_handler Content_timed.Track_marks.format)))
