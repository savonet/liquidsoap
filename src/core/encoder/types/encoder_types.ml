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
    (Type.Constr { Type.constructor = "format"; params = [(`Covariant, k)] })

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

let channels_of_params ?(default = 2) p =
  match
    List.find_map
      (function
        | `Anonymous s when String.lowercase_ascii s = "mono" -> Some 1
        | `Anonymous s when String.lowercase_ascii s = "stereo" -> Some 2
        | `Labelled ("stereo", { Term.term = `Bool b; _ }) ->
            Some (if b then 2 else 1)
        | `Labelled ("stereo", ({ Term.t = { Type.pos } } as tm)) ->
            raise_error ~pos
              (Printf.sprintf
                 "Invalid value %s for stereo mode. Only static `true` or \
                  `false` are allowed."
                 (Term.to_string tm))
        | `Labelled ("mono", { Term.term = `Bool b; _ }) ->
            Some (if b then 1 else 2)
        | `Labelled ("mono", ({ Term.t = { Type.pos } } as tm)) ->
            raise_error ~pos
              (Printf.sprintf
                 "Invalid value %s for mono mode. Only static `true` or \
                  `false` are allowed."
                 (Term.to_string tm))
        | `Labelled ("channels", { Term.term = `Int n }) -> Some n
        | `Labelled ("channels", ({ Term.t = { Type.pos } } as tm)) ->
            raise_error ~pos
              (Printf.sprintf
                 "Invalid value %s for channels mode. Only static numbers are \
                  allowed."
                 (Term.to_string tm))
        | _ -> None)
      p
  with
    | Some n -> n
    | None -> default

(* The frame type of an encoder that takes plain PCM audio and nothing else:
   what every audio-only encoder format uses. *)
let pcm_audio_type_of_encoder p =
  audio_type ~pcm_kind:Audio_format.kind (channels_of_params p)

let encoders = Hashtbl.create 16

let register name type_of_encoder =
  Hashtbl.replace encoders name type_of_encoder

let find name = Hashtbl.find_opt encoders name

(* Computing this from a term rather than from an encoder means a script's
   types are known without building anything it describes. *)
let type_of_encoder ~pos ((name, params) : Term.encoder) =
  match find name with
    | None -> raise_error ~pos ("unsupported format: " ^ name)
    | Some type_of_encoder ->
        let fields = type_of_encoder params in
        format_t ?pos (Frame_type.make Liquidsoap_lang.Lang.unit_t fields)

let () = Hooks.implement Hooks.type_of_encoder type_of_encoder
let audio_of_params p = pcm_audio_type_of_encoder p

let () =
  List.iter
    (fun name -> register name audio_of_params)
    [
      "fdkaac";
      "flac";
      "mp3";
      "mp3.abr";
      "mp3.cbr";
      "mp3.fxp";
      "mp3.vbr";
      "opus";
      "shine";
      "speex";
      "vorbis";
      "vorbis.abr";
      "vorbis.cbr";
      "wav";
    ]

let () =
  register "avi" (fun p ->
      audio_video_type ~pcm_kind:Audio_format.kind (channels_of_params p));
  register "theora" (fun _ -> video_type ())

let () =
  register "external" (fun p ->
      let channels = channels_of_params p in
      match
        List.find_map
          (function `Labelled ("video", p) -> Some p | _ -> None)
          p
      with
        | Some { Term.term = `Bool true } ->
            audio_video_type ~pcm_kind:Audio_format.kind channels
        | Some ({ Term.t = { Type.pos } } as tm) ->
            raise_error ~pos
              (Printf.sprintf
                 "Invalid value %s for value mode. Only `true` or `false is \
                  allowed."
                 (Term.to_string tm))
        | _ -> audio_type ~pcm_kind:Audio_format.kind channels)

let () =
  register "ndi" (fun p ->
      match
        List.fold_left
          (fun (audio, video) -> function
            | `Encoder ("audio", []) -> (true, video)
            | `Encoder ("audio.none", []) -> (false, video)
            | `Encoder ("video", []) -> (audio, true)
            | `Encoder ("video.none", []) -> (audio, false)
            | _ -> (audio, video))
          (true, true) p
      with
        | true, true -> audio_video_type ~pcm_kind:Audio_format.kind 2
        | false, true -> video_type ()
        | true, false -> audio_type ~pcm_kind:Audio_format.kind 2
        | _ -> raise_error ~pos:None "Invalid %%ndi encoder parameter!")

let () =
  register "ogg" (fun p ->
      let audio =
        ["vorbis"; "vorbis.cbr"; "vorbis.abr"; "opus"; "speex"; "flac"]
      in
      let audio =
        List.find_map
          (function
            | `Encoder (e, p) -> if List.mem e audio then Some p else None
            | _ -> None)
          p
      in
      let channels =
        match audio with None -> 0 | Some p -> channels_of_params p
      in
      let video =
        List.exists (function `Encoder ("theora", _) -> true | _ -> false) p
      in
      if not video then audio_type ~pcm_kind:Audio_format.kind channels
      else audio_video_type ~pcm_kind:Audio_format.kind channels)
