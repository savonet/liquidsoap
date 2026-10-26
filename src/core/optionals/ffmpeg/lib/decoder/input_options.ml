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
type t = { format : input_format option; opts : Avutil.opts }

let error message = Runtime_error.raise ~pos:[] ~message "ffmpeg_decoder"
let none () = { format = None; opts = Hashtbl.create 0 }

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
  { format; opts }

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
