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

module F = Encoder_format

let error ?pos message = Encoder_types.raise_error ~pos message
let position ({ Term.t = { Type.pos } } : Term.t) = pos

let markers params =
  List.filter_map
    (function `Anonymous s -> F.media_of_marker s | _ -> None)
    params

let literal_codec params =
  List.fold_left
    (fun codec -> function
      | `Labelled ("codec", { Term.term = `String s }) -> Some s
      | `Labelled ("codec", _) -> None
      | _ -> codec)
    None params

let pcm_kind params =
  List.fold_left
    (fun kind -> function
      | `Anonymous "pcm" -> `Pcm
      | `Anonymous "pcm_s16" -> `Pcm_s16
      | `Anonymous "pcm_f32" -> `Pcm_f32
      | _ -> kind)
    `Pcm params

(* The channel rule of §5: the last channel argument decides. *)
let channels params =
  let winner =
    List.fold_left
      (fun winner -> function
        | `Labelled ((("channel_layout" | "channels" | "ac") as l), tm) ->
            Some (l, tm)
        | _ -> winner)
      None params
  in
  let invalid tm =
    error ?pos:(position tm)
      (Printf.sprintf
         "Invalid value %s for channels parameter. Only static numbers are \
          allowed."
         (Term.to_string tm))
  in
  let layout name tm =
    let lookups = !F.lookups in
    match lookups.channels_of_layout name with
      | Some n -> `Channels n
      | None when lookups.authoritative ->
          error ?pos:(position tm)
            (Printf.sprintf "Unknown channel layout: %s" name)
      | None -> `Unknown
  in
  match winner with
    | None -> `Channels 2
    | Some (("channels" | "ac"), { Term.term = `Int n }) -> `Channels n
    | Some ("channel_layout", ({ Term.term = `String s } as tm)) -> layout s tm
    | Some ("channel_layout", ({ Term.term = `Float f } as tm)) ->
        layout (Printf.sprintf "%.1f" f) tm
    | Some (_, tm) -> invalid tm

let alpha params =
  List.fold_left
    (fun alpha -> function
      | `Labelled ("pixel_format", { Term.term = `String "guess" }) -> false
      | `Labelled ("pixel_format", { Term.term = `String s }) ->
          Option.value ~default:false (!F.lookups.pixel_format_has_alpha s)
      | `Labelled ("pixel_format", _) -> false
      | _ -> alpha)
    false params

let check_copy_params params =
  match params with
    | [] | [`Anonymous ("wait_for_keyframe" | "ignore_keyframe")] -> ()
    | arg :: _ ->
        let pos, name =
          match arg with
            | `Anonymous s -> (None, s)
            | `Labelled (l, tm) -> (position tm, l)
            | `Encoder (e, _) -> (None, "%" ^ e)
        in
        error ?pos (Printf.sprintf "Invalid argument %s for a copy track" name)

let shape_of_encoded ~mode ~field params : F.shape =
  match
    F.media_type ~markers:(markers params) ~field ~codec:(literal_codec params)
  with
    | None -> `Untyped
    | Some `Subtitle -> `Subtitle
    | Some `Audio when mode = `Raw -> `Raw_audio
    | Some `Video when mode = `Raw -> `Raw_video
    | Some `Audio -> (
        match channels params with
          | `Channels n -> `Audio (pcm_kind params, n)
          | `Unknown -> `Untyped)
    | Some `Video -> `Video (alpha params)

let shape_of_track (name, params) =
  let field, mode =
    try F.split_track_name name with Failure message -> error message
  in
  let shape =
    match mode with
      | `Drop ->
          if params <> [] then
            error (Printf.sprintf "Track %%%s.drop takes no argument" field);
          `Untyped
      | `Copy ->
          check_copy_params params;
          `Copy
      | (`Internal | `Raw) as mode -> shape_of_encoded ~mode ~field params
  in
  (field, shape)

let type_of_encoder params =
  let tracks =
    List.filter_map
      (function `Encoder track -> Some (shape_of_track track) | _ -> None)
      params
  in
  try F.fields_type tracks with Failure message -> error message

let () = Encoder_types.register "ffmpeg" type_of_encoder
