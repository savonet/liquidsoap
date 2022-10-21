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

let description = "rosetta implementation"

let detect_native s =
  if String.is_valid_utf_8 s then `UTF_8
  else if String.is_valid_utf_16be s then `UTF_16BE
  else if String.is_valid_utf_16le s then `UTF_16LE
  else `UTF_8

let can_detect = [`UTF_8; `UTF_16BE; `UTF_16LE]
let can_decode = Charset_base.all_encodings
let can_encode = [`UTF_8; `UTF_16BE; `UTF_16LE]

let convert ?source ?target s =
  let source = match source with None -> detect_native s | Some x -> x in
  let buf = Buffer.create 10 in
  let add_char =
    match target with
      | None | Some `UTF_8 -> Buffer.add_utf_8_uchar buf
      | Some `UTF_16LE -> Buffer.add_utf_16le_uchar buf
      | Some `UTF_16BE -> Buffer.add_utf_16be_uchar buf
      | Some x -> raise (Charset_base.Unsupported_encoding x)
  in
  let result () = Buffer.contents buf in
  match source with
    | (`UTF_8 | `UTF_16LE | `UTF_16BE) as source ->
        let get_char =
          match source with
            | `UTF_8 -> String.get_utf_8_uchar
            | `UTF_16LE -> String.get_utf_16le_uchar
            | `UTF_16BE -> String.get_utf_16be_uchar
        in
        let len = String.length s in
        if target = Some `UTF_16BE || target = Some `UTF_16LE then
          add_char Uchar.bom;
        let rec f pos =
          if pos = len then result ()
          else (
            let d = get_char s pos in
            (match
               (Uchar.utf_decode_uchar d, Uchar.utf_decode_is_valid d, target)
             with
              | _, false, _ -> raise (Charset_base.Malformed_input s)
              | (c, _, None | c, _, Some `UTF_8)
                when pos = 0 && Uchar.equal c Uchar.bom ->
                  ()
              | c, _, _ -> add_char c);
            f (pos + Uchar.utf_decode_length d))
        in
        f 0
    | ( `ISO_8859_1 | `ISO_8859_10 | `ISO_8859_11 | `ISO_8859_13 | `ISO_8859_14
      | `ISO_8859_15 | `ISO_8859_16 | `ISO_8859_2 | `ISO_8859_3 | `ISO_8859_4
      | `ISO_8859_5 | `ISO_8859_6 | `ISO_8859_7 | `ISO_8859_8 | `ISO_8859_9
      | `KOI8_R | `KOI8_U | `UTF_7 ) as source ->
        let decoder = Rosetta.decoder source (`String s) in
        let rec f () =
          match Rosetta.decode decoder with
            | `Await -> assert false
            | `Uchar u ->
                add_char u;
                f ()
            | `End -> result ()
            | `Malformed err -> raise (Charset_base.Malformed_input err)
        in
        f ()
    | x when target = None || target = Some x -> s
    | x -> raise (Charset_base.Unsupported_encoding x)
