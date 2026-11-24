(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Liquidsoap_lang

let raise_error ~pos message =
  Runtime_error.raise ~message
    ~pos:(match pos with None -> [] | Some pos -> [pos])
    "encoder"

let format_t ?pos k =
  Type.make ?pos
    (Type.Constr { Type.constructor = "format"; params = [(`Invariant, k)] })

let audio_type ~pcm_kind n =
  Fields.make
    ~audio:
      (Type.make
         (Format_type.descr
            (`Format (Pcm_format.format_of_channels ~pcm_kind n))))
    ()

let video_format () = Content_base.default_format Video_format.kind

let audio_video_type ~pcm_kind n =
  Fields.add Fields.video
    (Type.make (Format_type.descr (`Format (video_format ()))))
    (audio_type ~pcm_kind n)

let video_type () =
  Fields.make
    ~video:(Type.make (Format_type.descr (`Format (video_format ()))))
    ()

let reject_pcm_kind_params p =
  List.iter
    (function
      | `Anonymous s
        when List.mem (String.lowercase_ascii s) ["pcm_s16"; "pcm_f32"] ->
          raise_error ~pos:None
            (Printf.sprintf
               "%s audio is only supported by the %%ffmpeg encoder." s)
      | _ -> ())
    p

let encoders = Hashtbl.create 16

let register name type_of_encoder =
  Hashtbl.replace encoders name type_of_encoder

let find name = Hashtbl.find_opt encoders name

(* Each name registers how to type its terms: native formats build a format
   from their literal parameters (Format_builders), %ffmpeg types its own. *)
let type_of_encoder ~pos ((name, params) : Term.encoder) =
  match find name with
    | None -> raise_error ~pos ("unsupported format: " ^ name)
    | Some type_of_encoder ->
        let fields =
          try type_of_encoder params with
            | Runtime_error.Runtime_error { kind; msg; pos = [] } ->
                Runtime_error.raise ~message:msg ~pos:(Option.to_list pos) kind
            | Audio_layout.Unsupported ->
                raise_error ~pos "unsupported number of audio channels"
        in
        format_t ?pos (Frame_type.make Liquidsoap_lang.Lang.unit_t fields)

let () = Hooks.implement Hooks.type_of_encoder type_of_encoder
