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

let make ?pos ~audio ~video ~midi () =
  Type.make ?pos
    (Type.Constr
       {
         Type.constructor = "stream_kind";
         params =
           [
             (Type.Covariant, audio);
             (Type.Covariant, video);
             (Type.Covariant, midi);
           ];
       })

let univ ?pos () =
  make ?pos
    ~audio:(Liquidsoap_lang.Lang.univ_t ())
    ~video:(Liquidsoap_lang.Lang.univ_t ())
    ~midi:(Liquidsoap_lang.Lang.univ_t ())
    ()

let make_kind ?pos kind =
  let evar ?(constraints = []) () = Type.var ~constraints ?pos () in
  let mk_format f = Type.make ?pos (Format_type.descr f) in
  match kind with
    | `Any -> evar ()
    | `Internal -> evar ~constraints:[Format_type.internal_media] ()
    | `Kind k ->
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content_base.string_of_kind k;
               params = [(Type.Covariant, evar ())];
             })
    | `Format f ->
        let k = Content_base.kind f in
        Type.make ?pos
          (Type.Constr
             {
               Type.constructor = Content_base.string_of_kind k;
               params = [(Type.Covariant, mk_format f)];
             })

let set_audio t audio =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [
              (Type.Covariant, _);
              (Type.Covariant, video);
              (Type.Covariant, midi);
            ];
        } ->
        {
          t with
          Type.descr =
            Type.Constr
              {
                Type.constructor = "stream_kind";
                params =
                  [
                    (Type.Covariant, audio);
                    (Type.Covariant, video);
                    (Type.Covariant, midi);
                  ];
              };
        }
    | _ -> assert false

let set_video t video =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [
              (Type.Covariant, audio);
              (Type.Covariant, _);
              (Type.Covariant, midi);
            ];
        } ->
        {
          t with
          Type.descr =
            Type.Constr
              {
                Type.constructor = "stream_kind";
                params =
                  [
                    (Type.Covariant, audio);
                    (Type.Covariant, video);
                    (Type.Covariant, midi);
                  ];
              };
        }
    | _ -> assert false

let set_midi t midi =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [
              (Type.Covariant, audio);
              (Type.Covariant, video);
              (Type.Covariant, _);
            ];
        } ->
        {
          t with
          Type.descr =
            Type.Constr
              {
                Type.constructor = "stream_kind";
                params =
                  [
                    (Type.Covariant, audio);
                    (Type.Covariant, video);
                    (Type.Covariant, midi);
                  ];
              };
        }
    | _ -> assert false

let get_audio t =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [(Type.Covariant, audio); (Type.Covariant, _); (Type.Covariant, _)];
        } ->
        audio
    | _ -> assert false

let get_video t =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [(Type.Covariant, _); (Type.Covariant, video); (Type.Covariant, _)];
        } ->
        video
    | _ -> assert false

let get_midi t =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [(Type.Covariant, _); (Type.Covariant, _); (Type.Covariant, midi)];
        } ->
        midi
    | _ -> assert false

let to_string t = Type.to_string t

let content_type ~default t =
  match (Type.deref t).Type.descr with
    | Type.Constr { Type.constructor = name; params = [(_, t)] } -> (
        match (Type.deref t).Type.descr with
          | Type.Custom { typ = Format_type.Type f } -> f
          | Type.Var _ ->
              Content_base.default_format (Content_base.kind_of_string name)
          | _ -> assert false)
    | Type.Var _ -> default ()
    | _ -> assert false

let content_type t =
  match (Type.deref t).Type.descr with
    | Type.Constr
        {
          Type.constructor = "stream_kind";
          params =
            [
              (Type.Covariant, audio);
              (Type.Covariant, video);
              (Type.Covariant, midi);
            ];
        } ->
        let audio =
          content_type ~default:Content_internal.default_audio audio
        in
        let video =
          content_type ~default:Content_internal.default_video video
        in
        let midi = content_type ~default:Content_internal.default_midi midi in
        let ctype = Frame.mk_fields ~audio ~video ~midi () in
        let mk_format f =
          let k = Content_base.kind f in
          Type.make
            (Type.Constr
               {
                 Type.constructor = Content_base.string_of_kind k;
                 params = [(Type.Covariant, Type.make (Format_type.descr f))];
               })
        in
        let t' =
          Type.make
            (Type.Constr
               {
                 Type.constructor = "stream_kind";
                 params =
                   [
                     (Type.Covariant, mk_format audio);
                     (Type.Covariant, mk_format video);
                     (Type.Covariant, mk_format midi);
                   ];
               })
        in
        Typing.(t <: t');
        ctype
    | _ -> assert false
