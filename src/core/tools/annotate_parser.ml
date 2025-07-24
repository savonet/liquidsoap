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

exception Error of string

let log = Log.make ["annotate"]

let parse =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised
      Liquidsoap_lang.Parser.annotate
  in
  fun s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    try
      let tokenizer = Liquidsoap_lang.Preprocessor.mk_tokenizer lexbuf in
      let metadata = processor tokenizer in
      let b = Buffer.create 10 in
      let rec f () =
        match Sedlexing.next lexbuf with
          | Some c ->
              Buffer.add_utf_8_uchar b c;
              f ()
          | None -> Buffer.contents b
      in
      (metadata, f ())
    with _ ->
      let startp, endp = Sedlexing.loc lexbuf in
      let err = Printf.sprintf "Char %d-%d: Syntax error" startp endp in
      log#info "Error while parsing annotate URI %s: %s"
        (Lang_string.quote_string s)
        err;
      raise (Error err)
