(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

module Img = Image.RGBA32

(** Protocol plugin for speech synthesis *)

open Dtools

let dlog = Log.make ["protocols";"text"]

let text s ~log maxtime =
  let local = Filename.temp_file "text" ".bmp" in
  try
    let s = Annotate.lex s in
    let font = ref None in
    let size = ref None in
    let color = ref None in
    let rec aux = function
      | [`Kwd ":";`Ident s] -> s
      | (`Kwd ",")::s -> aux s
      | (`Ident key)::(`Kwd "=")::(`Ident value)::s ->
        (
          match key with
          | "font" -> font := Some value
          | "size" -> size := Some (int_of_string value)
          | "color" -> color := Some (int_of_string value)
          | _ -> ()
        );
        aux s
      | [`Ident s] -> s
      | _ -> failwith "Bad text options."
    in
    let s = aux s in
    Printf.printf "text: %s\n%!" s;
    let img = Decoder.get_text_decoder ?font:!font ?size:!size ?color:!color s in
    let img = match img with Some img -> img | None -> failwith "Could not find a text synthesizer" in
    let bmp = Img.to_BMP img in
    let oc = open_out local in
    output_string oc bmp;
    close_out oc;
    [Request.indicator ~temporary:true local]
  with
  | e ->
    dlog#f 3 "Failed to synthetize text: %s!"
      (match e with Failure s -> s | _ -> Utils.error_message e);
    log "text synthesis failed!" ;
    (try Unix.unlink local with _ -> ());
    []

let () =
  Request.protocols#register
    ~sdoc:"Text to image synthesis."
    "text"
    { Request.resolve = text ; Request.static = true }
