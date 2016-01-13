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

(** Protocol plugin for text synthesis *)

module Img = Image.RGBA32

open Stdlib
open Dtools
open Genlex

let dlog = Log.make ["protocols";"text"]

let text s ~log maxtime =
  let local = Filename.temp_file "text" ".bmp" in
  try
    let md, s = Annotate.parse s in
    let font = List.may_assoc "font" md in
    let size = Option.funct int_of_string (List.may_assoc "size" md) in
    let color = Option.funct int_of_string (List.may_assoc "color" md) in
    let img = Decoder.get_text_decoder ?font ?size ?color s in
    let img = match img with Some img -> img | None -> failwith "Could not find a text synthesizer" in
    let bmp = Img.to_BMP img in
    let oc = open_out local in
    output_string oc bmp;
    close_out oc;
    [Request.indicator ~temporary:true local]
  with
  | e ->
    dlog#f 3 "Failed to synthetize text: %s!"
      (match e with Failure s -> s | _ -> Printexc.to_string e);
    log "text synthesis failed!" ;
    (try Unix.unlink local with _ -> ());
    []

let () =
  Request.protocols#register
    ~sdoc:"Text to image synthesis."
    "text"
    { Request.resolve = text ; Request.static = true }
