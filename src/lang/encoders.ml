(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

open Term
open Term.Ground

(** An encoder. *)
type encoder = {
  kind_of_encoder : Term.encoder_params -> Frame.kind Frame.fields;
      (** Compute the kind of the encoder. *)
  make : Value.encoder_params -> Encoder.format;
      (** Actually create the encoder. *)
}

let encoders = ref []

(** Register an encoder. *)
let register name kind_of_encoder make =
  encoders := (name, { kind_of_encoder; make }) :: !encoders

(** Find an encoder with given name. *)
let find_encoder name = List.assoc name !encoders

let channels_of_params ?(default = 2) p =
  match
    List.find_map
      (function
        | "", `Term { term = Ground (String "stereo") } -> Some 2
        | "", `Term { term = Ground (String "mono") } -> Some 1
        | "channels", `Term { term = Ground (Int n) } -> Some n
        | _ -> None)
      p
  with
    | Some n -> n
    | None -> default

(** Compute a kind from a non-fully evaluated format. This should give the same
    result than [Encoder.kind_of_format] once evaluated... *)
let kind_of_encoder ((e, p) : Term.encoder) = (find_encoder e).kind_of_encoder p

(*
(** Compute a kind from a non-fully evaluated format. This should give the same
    result than [Encoder.kind_of_format] once evaluated... *)
let kind_of_encoder (name, p) =
  let channels ?(default = 2) p =
    match
      List.find_map
        (function
          | "", `Term { term = Ground (String "stereo") } -> Some 2
          | "", `Term { term = Ground (String "mono") } -> Some 1
          | "channels", `Term { term = Ground (Int n) } -> Some n
          | _ -> None)
        p
    with
      | Some n -> n
      | None -> default
  in
  match name with
    | "wav" | "mp3" | "mp3.cbr" | "mp3.abr" | "mp3.vbr" | "mp3.fxp" | "shine"
    | "flac" | "fdkaac" | "vorbis" | "vorbis.cbr" | "vorbis.abr" | "opus"
    | "speex" ->
        Encoder.audio_kind (channels p)
    | "avi" -> Encoder.audio_video_kind (channels p)
    | "ogg" ->
        let audio =
          ["vorbis"; "vorbis.cbr"; "vorbis.abr"; "opus"; "speex"; "flac"]
        in
        let audio =
          List.find_map
            (function
              | "", `Encoder (e, p) -> if List.mem e audio then Some p else None
              | _ -> None)
            p
        in
        let channels = match audio with None -> 0 | Some p -> channels p in
        let video =
          List.exists
            (function "", `Encoder ("theora", _) -> true | _ -> false)
            p
        in
        if not video then Encoder.audio_kind channels
        else Encoder.audio_video_kind channels
    | "ffmpeg" ->
        let audio =
          List.fold_left
            (fun audio p ->
              match p with
                | "", `Encoder ("audio.copy", _) ->
                    `Format
                      Frame_content.(
                        default_format (kind_of_string "ffmpeg.audio.copy"))
                | "", `Encoder ("audio.raw", _) ->
                    `Format
                      Frame_content.(
                        default_format (kind_of_string "ffmpeg.audio.raw"))
                | "", `Encoder ("audio", p) ->
                    let channels =
                      try
                        let channels =
                          try List.assoc "channels" p
                          with Not_found -> List.assoc "ac" p
                        in
                        match channels with
                          | `Term { term = Ground (Int n) } -> n
                          | _ -> raise Exit
                      with
                        | Not_found -> 2
                        | Exit -> raise Not_found
                    in
                    `Format
                      Frame_content.(
                        Audio.lift_params
                          {
                            Contents.channel_layout =
                              lazy
                                (Audio_converter.Channel_layout
                                 .layout_of_channels channels);
                          })
                | _ -> audio)
            Frame.none p
        in
        let video =
          List.fold_left
            (fun audio p ->
              match p with
                | "", `Encoder ("video.copy", _) ->
                    `Format
                      Frame_content.(
                        default_format (kind_of_string "ffmpeg.video.copy"))
                | "", `Encoder ("video.raw", _) ->
                    `Format
                      Frame_content.(
                        default_format (kind_of_string "ffmpeg.video.raw"))
                | "", `Encoder ("video", _) ->
                    `Format Frame_content.(default_format Video.kind)
                | _ -> audio)
            Frame.none p
        in
        { Frame.audio; video; midi = Frame.none }
    | _ -> raise Not_found
*)

let type_of_encoder ~pos ~level e =
  let kind = kind_of_encoder e in
  let audio = kind_t ~pos ~level kind.Frame.audio in
  let video = kind_t ~pos ~level kind.Frame.video in
  let midi = kind_t ~pos ~level kind.Frame.midi in
  format_t ~pos ~level (frame_kind_t ~pos ~level audio video midi)

(*
(* TODO: it would be better if each encoder registered itself here than
   having this match *)
let make_encoder_base e p =
  let ogg_audio e p =
    match e with
      | "vorbis" -> Lang_vorbis.make p
      | "vorbis.cbr" -> Lang_vorbis.make_cbr p
      | "vorbis.abr" -> Lang_vorbis.make_abr p
      | "opus" -> Lang_opus.make p
      | "speex" -> Lang_speex.make p
      | "flac" -> Lang_flac.make_ogg p
      | _ -> raise Not_found
  in
  let ogg_audio_opt e p = try Some (ogg_audio e p) with Not_found -> None in
  let ogg_video e p =
    match e with "theora" -> Lang_theora.make p | _ -> raise Not_found
  in
  let ogg_video_opt e p = try Some (ogg_video e p) with Not_found -> None in
  match e with
    | "ogg" ->
        let audio =
          List.find_map
            (function "", `Encoder (e, p) -> ogg_audio_opt e p | _ -> None)
            p
        in
        let video =
          List.find_map
            (function "", `Encoder (e, p) -> ogg_video_opt e p | _ -> None)
            p
        in
        Encoder.Ogg { Ogg_format.audio; video }
    | "vorbis" ->
        Encoder.Ogg
          { Ogg_format.audio = Some (Lang_vorbis.make p); video = None }
    | "vorbis.cbr" ->
        Encoder.Ogg
          { Ogg_format.audio = Some (Lang_vorbis.make_cbr p); video = None }
    | "vorbis.abr" ->
        Encoder.Ogg
          { Ogg_format.audio = Some (Lang_vorbis.make_abr p); video = None }
    | "opus" ->
        Encoder.Ogg { Ogg_format.audio = Some (Lang_opus.make p); video = None }
    | "speex" ->
        Encoder.Ogg
          { Ogg_format.audio = Some (Lang_speex.make p); video = None }
    | "flac" -> Lang_flac.make p
    | "theora" ->
        Encoder.Ogg
          { Ogg_format.audio = None; video = Some (Lang_theora.make p) }
    | "ffmpeg" -> Lang_ffmpeg.make p
    | "external" -> Lang_external_encoder.make p
    | "gstreamer" -> Lang_gstreamer.make p
    | "wav" -> Lang_wav.make p
    | "avi" -> Lang_avi.make p
    | "mp3" | "mp3.cbr" -> Lang_mp3.make_cbr p
    | "mp3.abr" -> Lang_mp3.make_abr p
    | "mp3.vbr" -> Lang_mp3.make_vbr p
    | "mp3.fxp" | "shine" -> Lang_shine.make p
    | "fdkaac" -> Lang_fdkaac.make p
    | e -> (
        try
          Encoder.Ogg { Ogg_format.audio = Some (ogg_audio e p); video = None }
        with Not_found -> (
          try
            Encoder.Ogg
              { Ogg_format.audio = None; video = Some (ogg_video e p) }
          with Not_found -> raise (Error ("Unkown encoder " ^ e))))
*)

let make_encoder ~pos t ((e, p) : Value.encoder) =
  try
    let e = (find_encoder e).make p in
    let (_ : Encoder.factory) = Encoder.get_factory e in
    e
  with Not_found -> raise (Term.Unsupported_format (pos, Term.print_term t))
