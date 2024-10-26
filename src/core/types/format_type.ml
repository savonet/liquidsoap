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

module Type_custom = Liquidsoap_lang.Type_custom

type descr = [ `Format of Content_base.format | `Kind of Content_base.kind ]

(* By convention, all format for pcm kind are from Content_audio to
   allow shared parameters between the different pcm implementations. *)
let normalize_format f =
  match f with
    | _ when Content_pcm_s16.is_format f ->
        Content_audio.lift_params (Content_pcm_s16.get_params f)
    | _ when Content_pcm_f32.is_format f ->
        Content_audio.lift_params (Content_pcm_f32.get_params f)
    | _ -> f

let denormalize_format k f =
  match k with
    | _ when Content_pcm_s16.is_kind k ->
        Content_pcm_s16.lift_params (Content_audio.get_params f)
    | _ when Content_pcm_f32.is_kind k ->
        Content_pcm_f32.lift_params (Content_audio.get_params f)
    | _ -> f

module FormatSpecs = struct
  type content = Content_base.format

  let name = "format"
  let copy_with _ = Content_base.duplicate
  let occur_check _ _ = ()
  let filter_vars _ l _ = l
  let repr _ _ _ = assert false
  let subtype _ f f' = Content_base.merge f f'

  let sup _ f f' =
    Content_base.(merge (duplicate f) (duplicate f'));
    f

  let to_string _ = assert false
end

module FormatType = struct
  include Type_custom.Make (FormatSpecs)

  let handler f = handler (normalize_format f)
end

let format_handler = FormatType.handler
let format_descr f = Type.Custom (format_handler f)

let string_of_kind (k, ty) =
  match (Type.deref ty).Type.descr with
    | Type.(Custom { custom_name = "format"; typ }) ->
        Content_base.string_of_format
          (denormalize_format k (FormatType.to_content typ))
    | _ ->
        Printf.sprintf "%s(%s)"
          (Content_base.string_of_kind k)
          (Type.to_string ty)

let repr_of_kind repr l (k, ty) =
  match (Type.deref ty).Type.descr with
    | Type.(Custom { custom_name = "format"; typ }) ->
        `Constr
          ( Content_base.string_of_format
              (denormalize_format k (FormatType.to_content typ)),
            [] )
    | _ -> `Constr (Content_base.string_of_kind k, [(`Covariant, repr l ty)])

module KindSpecs = struct
  type content = Content_base.kind * Type.t

  let name = "kind"
  let copy_with copy_with (k, ty) = (k, copy_with ty)
  let occur_check occur_check (_, ty) = occur_check ty
  let filter_vars filter_vars l (_, ty) = filter_vars l ty
  let repr = repr_of_kind

  let subtype subtype (k, t) (k', t') =
    assert (k = k');
    subtype t t'

  let sup sup (k, t) (k', t') =
    assert (k = k');
    (k, sup t t')

  let to_string = string_of_kind
end

module KindType = Type_custom.Make (KindSpecs)

let kind_handler = KindType.handler

let descr descr =
  let k =
    match descr with
      | `Format f ->
          let kind = Content_base.kind f in
          (kind, Type.make (format_descr f))
      | `Kind k -> (k, Type.var ())
  in
  Type.Custom (kind_handler k)

exception Never_type

let rec content_type ?kind ty =
  match ((Type.demeth ty).Type.descr, kind) with
    | Type.Never, None -> raise Never_type
    | Type.Custom { Type.custom_name = "kind"; typ }, None ->
        let kind, ty = KindType.to_content typ in
        content_type ~kind ty
    | Type.Custom { Type.custom_name = "format"; typ }, Some k ->
        let f = FormatType.to_content typ in
        denormalize_format k f
    | Type.Var _, Some kind -> Content_base.default_format kind
    | Type.Var _, None ->
        Runtime_error.raise
          ~pos:(match ty.Type.pos with Some p -> [p] | None -> [])
          ~message:
            "Untyped track value! Tracks must have a type to drive decoders \
             and encoders. Either use it in a track-specific operator, add a \
             type annotation or remove the variable."
          "eval"
    | _ ->
        Runtime_error.raise
          ~pos:(match ty.Type.pos with Some p -> [p] | None -> [])
          ~message:(Printf.sprintf "Invalid track type: %s" (Type.to_string ty))
          "eval"

module type Content = sig
  val kind : Content_base.kind
  val is_kind : Content_base.kind -> bool
  val is_format : Content_base.format -> bool
end

let pcm_modules =
  [
    (module Content_audio : Content);
    (module Content_pcm_s16 : Content);
    (module Content_pcm_f32 : Content);
  ]

module Content_metadata = struct
  include Content_timed.Metadata

  let kind = Content_base.kind format
end

module Content_track_marks = struct
  include Content_timed.Track_marks

  let kind = Content_base.kind format
end

let internal_modules =
  pcm_modules
  @ [
      (module Content_video : Content);
      (module Content_metadata : Content);
      (module Content_track_marks : Content);
    ]

let string_of_kind m =
  let module Content = (val m : Content) in
  Content_base.string_of_kind Content.kind

let is_kind k m =
  let module Content = (val m : Content) in
  Content.is_kind k

let is_format f m =
  let module Content = (val m : Content) in
  Content.is_format f

let check_track ?univ_descr modules =
  {
    Type.constr_descr =
      Printf.sprintf "a track of type: %s"
        (Utils.concat_with_last ~last:"or" ", "
           (List.map string_of_kind modules));
    univ_descr;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let b = Type.demeth b in
        match b.Type.descr with
          | Type.Var _ -> satisfies b
          | Type.Never -> ()
          | Type.Custom { Type.custom_name = "kind"; typ } ->
              let k, _ = KindType.to_content typ in
              if not (List.exists (is_kind k) modules) then
                raise Type.Unsatisfied_constraint
          | Type.Custom { Type.custom_name = "format"; typ } ->
              let f = FormatType.to_content typ in
              if not (List.exists (is_kind (Content_base.kind f)) modules) then
                raise Type.Unsatisfied_constraint
          | _ -> raise Type.Unsatisfied_constraint);
  }

let pcm_audio = check_track ~univ_descr:"pcm*" pcm_modules
let internal_track = check_track internal_modules

let internal_tracks =
  {
    Type.constr_descr = "a set of internal tracks";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let meths, base_type = Type.split_meths b in
        (match base_type.Type.descr with
          | Type.Var _ -> satisfies base_type
          | Type.Tuple [] -> ()
          | _ -> raise Type.Unsatisfied_constraint);
        List.iter
          (fun { Type.scheme = _, typ } ->
            match (Type.demeth typ).Type.descr with
              | Type.Never -> ()
              | Type.Custom { Type.custom_name = "kind"; typ } ->
                  let k, _ = KindType.to_content typ in
                  if not (List.exists (is_kind k) internal_modules) then
                    raise Type.Unsatisfied_constraint
              | Type.Custom { Type.custom_name = "format"; typ } ->
                  let f = FormatType.to_content typ in
                  if not (List.exists (is_format f) internal_modules) then
                    raise Type.Unsatisfied_constraint
              | Type.Var { contents = Free v } ->
                  v.constraints <-
                    Type.Constraints.add internal_track v.constraints
              | _ -> raise Type.Unsatisfied_constraint)
          meths);
  }

let track =
  {
    Type.constr_descr = "a track";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let b = Type.demeth b in
        match b.Type.descr with
          | Type.Var _ -> satisfies b
          | Type.Never
          | Type.Custom { Type.custom_name = "kind" }
          | Type.Custom { Type.custom_name = "format" } ->
              ()
          | _ -> raise Type.Unsatisfied_constraint);
  }

let muxed_tracks =
  {
    Type.constr_descr = "a set of tracks to be muxed into a source";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let meths, base_type = Type.split_meths b in
        (match (Type.demeth base_type).Type.descr with
          | Type.Var _ -> satisfies base_type
          | Type.Tuple [] -> ()
          | _ -> raise Type.Unsatisfied_constraint);
        List.iter
          (fun { Type.scheme = _, typ } ->
            match (Type.demeth typ).Type.descr with
              | Type.Never -> ()
              | Type.Custom { Type.custom_name = "kind" }
              | Type.Custom { Type.custom_name = "format" } ->
                  ()
              | Type.Var { contents = Free v } ->
                  v.constraints <- Type.Constraints.add track v.constraints
              | _ -> raise Type.Unsatisfied_constraint)
          meths);
  }

let content_type ty = content_type ty

let audio ?(pcm_kind = Content_audio.kind) () =
  Type.make (descr (`Kind pcm_kind))

let () =
  Type.register_type (Content_base.string_of_kind Content_audio.kind) (fun () ->
      Type.make (Type.Custom (kind_handler (Content_audio.kind, Type.var ()))))

let audio_n ?(pcm_kind = Content_audio.kind) n =
  Type.make
    (descr
       (`Format
         (Frame_base.audio_format ~pcm_kind
            {
              channel_layout =
                Lazy.from_val
                  (Audio_converter.Channel_layout.layout_of_channels n);
            })))

let audio_mono ?pcm_kind () = audio_n ?pcm_kind 1
let audio_stereo ?pcm_kind () = audio_n ?pcm_kind 2
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
      Type.make (descr (`Format Content_timed.Track_marks.format)))

let metadata = Type.make (descr (`Format Content_timed.Metadata.format))

let () =
  Type.register_type "metadata" (fun () ->
      Type.make (descr (`Format Content_timed.Track_marks.format)))
