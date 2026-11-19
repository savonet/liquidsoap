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

type input_format = (Avutil.input, Avutil.audio) Avutil.format

type decoder_options = {
  audio : Avutil.opts;
  video : Avutil.opts;
  subtitle : Avutil.opts;
}

type t = {
  format : input_format option;
  opts : Avutil.opts;
  decoder : decoder_options;
}

let error message = Runtime_error.raise ~pos:[] ~message "ffmpeg_decoder"

let no_decoder_options () =
  {
    audio = Hashtbl.create 0;
    video = Hashtbl.create 0;
    subtitle = Hashtbl.create 0;
  }

let none () =
  { format = None; opts = Hashtbl.create 0; decoder = no_decoder_options () }

type media_options = {
  prefix : string;
  select : decoder_options -> Avutil.opts;
  accepted_by_some_decoder : string -> Avutil.value -> bool;
}

let accepted_by_some_decoder decoders name value =
  let option = Hashtbl.create 1 in
  Hashtbl.replace option name value;
  List.exists
    (fun decoder -> Avcodec.decoder_option_errors decoder option = [])
    decoders

let media_options =
  [
    {
      prefix = "audio.";
      select = (fun decoder -> decoder.audio);
      accepted_by_some_decoder = accepted_by_some_decoder Avcodec.Audio.decoders;
    };
    {
      prefix = "video.";
      select = (fun decoder -> decoder.video);
      accepted_by_some_decoder = accepted_by_some_decoder Avcodec.Video.decoders;
    };
    {
      prefix = "subtitle.";
      select = (fun decoder -> decoder.subtitle);
      accepted_by_some_decoder =
        accepted_by_some_decoder Avcodec.Subtitle.decoders;
    };
  ]

let media_of_key key =
  List.find_opt
    (fun media -> String.starts_with ~prefix:media.prefix key)
    media_options

let is_decoder_option key = media_of_key key <> None

let split_decoder_options opts =
  let decoder = no_decoder_options () in
  Hashtbl.filter_map_inplace
    (fun key value ->
      match media_of_key key with
        | None -> Some value
        | Some media ->
            let length = String.length media.prefix in
            let name = String.sub key length (String.length key - length) in
            if name = "" then
              error (Printf.sprintf "Decoder option without a name: %s" key);
            if not (media.accepted_by_some_decoder name value) then
              error
                (Printf.sprintf
                   "Decoder option %s=%s: no FFmpeg decoder accepts it" key
                   (Opts.string_of_value value));
            Hashtbl.replace (media.select decoder) name value;
            None)
    opts;
  decoder

let find_format name =
  match Av.Format.find_input_format name with
    | Some f -> f
    | None -> error (Printf.sprintf "Unknown FFmpeg input format: %s" name)

let parse ~what s =
  let invalid () = error (Printf.sprintf "Invalid %s value: %S" what s) in
  (* The parser wraps a script in the bindings it predefines. *)
  let rec encoder (tm : Term.t) =
    match tm.Term.term with
      | `Encoder (_, params) -> params
      | `Let { body } | `Seq (_, body) -> encoder body
      | _ -> invalid ()
  in
  let params =
    match Runtime.parse ("%ffmpeg(" ^ s ^ ")") with
      | _, tm -> encoder tm
      | exception _ -> invalid ()
  in
  let opts = Hashtbl.create 4 in
  let format =
    List.fold_left
      (fun format param ->
        let value : Term.t -> [ `Identifier of string | Avutil.value ] =
         fun tm ->
          match tm.Term.term with
            | `Var v -> `Identifier v
            | `String v -> `String v
            | `Int v -> `Int v
            | `Float v -> `Float v
            | _ -> invalid ()
        in
        match param with
          | `Labelled (("f" | "format"), tm) -> (
              match value tm with
                | `Identifier name | `String name -> Some (find_format name)
                | _ -> invalid ())
          | `Labelled (key, tm) ->
              (match value tm with
                | `Identifier v -> Hashtbl.replace opts key (`String v)
                | #Avutil.value as v -> Hashtbl.replace opts key v);
              format
          | _ -> invalid ())
      None params
  in
  let decoder = split_decoder_options opts in
  { format; opts; decoder }

let of_metadata metadata =
  match Frame.Metadata.find_opt "ffmpeg_options" metadata with
    | Some s -> parse ~what:"ffmpeg_options" s
    | None -> none ()

let of_mime mime =
  match String.split_on_char ';' mime with
    | "application/ffmpeg" :: rest ->
        parse ~what:"MIME" (String.concat ";" rest)
    | _ -> none ()

let add_image_options { opts } =
  Hashtbl.replace opts "loop" (`Int 1);
  Hashtbl.replace opts "framerate" (`Int (Lazy.Mutexed.force Frame.video_rate))

let check_unused { opts } =
  match Opts.unused_message [("input", opts)] with
    | Some message -> error message
    | None -> ()
