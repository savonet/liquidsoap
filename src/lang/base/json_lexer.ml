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

open Json_parser
open Json_base
open Lang_string

(* Json specs *)

let control_char = [%sedlex.regexp? 0x00 .. 0x1f]

let unicode_escape_sequence =
  [%sedlex.regexp?
    ( '\\',
      'u',
      ascii_hex_digit,
      ascii_hex_digit,
      ascii_hex_digit,
      ascii_hex_digit )]

let decimal_digit = [%sedlex.regexp? '0' .. '9']
let one_nine = [%sedlex.regexp? '1' .. '9']
let non_null_integer = [%sedlex.regexp? one_nine, Star decimal_digit]
let sign = [%sedlex.regexp? Opt '-']
let integer = [%sedlex.regexp? sign, ('0' | non_null_integer)]

let exponent =
  [%sedlex.regexp? ('e' | 'E'), Opt ('+' | '-'), Plus decimal_digit]

let fractional_part = [%sedlex.regexp? '.', Plus decimal_digit]
let float_extension = [%sedlex.regexp? fractional_part, Opt exponent | exponent]
let float = [%sedlex.regexp? integer, float_extension]
let skipped = [%sedlex.regexp? Plus (' ' | '\r' | '\n' | '\t')]

let rec json_token lexbuf =
  match%sedlex lexbuf with
    | skipped -> json_token lexbuf
    | "true" -> BOOL true
    | "false" -> BOOL false
    | "null" -> NULL
    | float -> FLOAT (float_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | integer -> (
        let m = Sedlexing.Utf8.lexeme lexbuf in
        try INT (int_of_string m) with _ -> FLOAT (float_of_string m))
    | '{' -> LCUR
    | '}' -> RCUR
    | '[' -> LBRA
    | ']' -> RBRA
    | ',' -> COMMA
    | ':' -> COLON
    | '"' ->
        STRING
          (read_string
             (fst (Sedlexing.lexing_bytes_positions lexbuf))
             (Buffer.create 17) lexbuf)
    | eof -> EOF
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Parse error";
             })

and read_string pos buf lexbuf =
  (* See: https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
    match%sedlex lexbuf with
    | '\\', ('"' | '\\' | '/') ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_char buf matched.[1];
        read_string pos buf lexbuf
    | '\\', 'b' ->
        Buffer.add_char buf '\b';
        read_string pos buf lexbuf
    | '\\', 'f' ->
        Buffer.add_char buf '\x0c';
        read_string pos buf lexbuf
    | '\\', 'n' ->
        Buffer.add_char buf '\n';
        read_string pos buf lexbuf
    | '\\', 'r' ->
        Buffer.add_char buf '\r';
        read_string pos buf lexbuf
    | '\\', 't' ->
        Buffer.add_char buf '\t';
        read_string pos buf lexbuf
    | unicode_escape_sequence ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_string buf (unescape_utf8_char matched);
        read_string pos buf lexbuf
    | Plus (Compl (control_char | '"' | '\\')) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_string pos buf lexbuf
    | '"' -> Buffer.contents buf
    | eof ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "String is not terminated";
             })
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message =
                 Printf.sprintf "Illegal string character: %S"
                   (Sedlexing.Utf8.lexeme lexbuf);
             })

(* Json 5 extension *)

(* From: https://262.ecma-international.org/5.1/#sec-7.6 *)
let unicode_letter = [%sedlex.regexp? lu | ll | lt | lm | lo | nl]
let unicode_combining_mark = [%sedlex.regexp? mn | mc]
let unicode_digit = [%sedlex.regexp? nd]
let unicode_connector_punctuation = [%sedlex.regexp? pc]

let identifier_start =
  [%sedlex.regexp? unicode_letter | '$' | '_' | unicode_escape_sequence]

let identifier_part =
  [%sedlex.regexp?
    ( identifier_start | unicode_combining_mark | unicode_digit
    | unicode_connector_punctuation | 0x200C | 0x200D )]

let identifier = [%sedlex.regexp? identifier_start, Star identifier_part]

(* From https://262.ecma-international.org/5.1/#sec-7.3 *)
let line_terminator = [%sedlex.regexp? 0xA | 0xD | 0x2028 | 0x2029]

let line_terminator_sequence =
  [%sedlex.regexp? 0xA | 0xD | 0x2028 | 0x2029 | 0xD, 0xA]

let hex_integer = [%sedlex.regexp? '0', ('x' | 'X'), Plus ascii_hex_digit]
let json5_sign = [%sedlex.regexp? Opt ('+' | '-')]
let json5_decimal_integer = [%sedlex.regexp? '0' | non_null_integer]

let json5_integer =
  [%sedlex.regexp? json5_sign, (json5_decimal_integer | hex_integer)]

let infinity = [%sedlex.regexp? "Infinity"]
let nan = [%sedlex.regexp? "NaN"]
let json5_integer_exponent = [%sedlex.regexp? json5_decimal_integer, exponent]
let json5_float_integer = [%sedlex.regexp? json5_integer, '.', Opt exponent]

let json5_float_decimal =
  [%sedlex.regexp? Opt json5_integer, '.', Plus decimal_digit, Opt exponent]

let json5_float_number =
  [%sedlex.regexp?
    json5_integer_exponent | json5_float_integer | json5_float_decimal]

let json5_float =
  [%sedlex.regexp? json5_sign, (json5_float_number | nan | infinity)]

(* From: https://spec.json5.org/#white-space *)
let json5_skipped =
  [%sedlex.regexp?
    0x9 | 0xA | 0xb | 0xc | 0xd | 0x20 | 0xA0 | 0x2028 | 0x2029 | 0xFEFF | zs]

let rec json5_token lexbuf =
  match%sedlex lexbuf with
    | Plus json5_skipped -> json5_token lexbuf
    | json5_float -> FLOAT (float_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | json5_integer -> (
        let m = Sedlexing.Utf8.lexeme lexbuf in
        try INT (int_of_string m) with _ -> FLOAT (float_of_string m))
    | "true" -> BOOL true
    | "false" -> BOOL false
    | "null" -> NULL
    | '{' -> LCUR
    | '}' -> RCUR
    | '[' -> LBRA
    | ']' -> RBRA
    | ',' -> COMMA
    | ':' -> COLON
    | identifier ->
        IDENTIFIER (Lang_string.unescape_string (Sedlexing.Utf8.lexeme lexbuf))
    | "//" ->
        read_single_line_comment
          (fst (Sedlexing.lexing_bytes_positions lexbuf))
          lexbuf;
        json5_token lexbuf
    | "/*" ->
        read_multiline_comment
          (fst (Sedlexing.lexing_bytes_positions lexbuf))
          lexbuf;
        json5_token lexbuf
    | '"' ->
        STRING
          (read_json5_string '"'
             (fst (Sedlexing.lexing_bytes_positions lexbuf))
             (Buffer.create 17) lexbuf)
    | '\'' ->
        STRING
          (read_json5_string '\''
             (fst (Sedlexing.lexing_bytes_positions lexbuf))
             (Buffer.create 17) lexbuf)
    | eof -> EOF
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Parse error";
             })

and read_single_line_comment pos lexbuf =
  match%sedlex lexbuf with
    | eof | line_terminator_sequence -> ()
    | Plus (Compl line_terminator) -> read_single_line_comment pos lexbuf
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Parse error";
             })

and read_multiline_comment pos lexbuf =
  match%sedlex lexbuf with
    | Plus '*', '/' -> ()
    | '*', Compl '/' | Plus (Compl '*') -> read_multiline_comment pos lexbuf
    | eof ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Comment is not terminated";
             })
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Parse error";
             })

and read_json5_string sep pos buf lexbuf =
  (* See: https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
    match%sedlex lexbuf with
    | '\\', ('"' | '\\' | '/') ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_char buf matched.[1];
        read_json5_string sep pos buf lexbuf
    | '\\', 'b' ->
        Buffer.add_char buf '\b';
        read_json5_string sep pos buf lexbuf
    | '\\', 'f' ->
        Buffer.add_char buf '\x0c';
        read_json5_string sep pos buf lexbuf
    | '\\', 'n' ->
        Buffer.add_char buf '\n';
        read_json5_string sep pos buf lexbuf
    | '\\', 'r' ->
        Buffer.add_char buf '\r';
        read_json5_string sep pos buf lexbuf
    | '\\', 't' ->
        Buffer.add_char buf '\t';
        read_json5_string sep pos buf lexbuf
    | '\\', '0' ->
        Buffer.add_char buf '\000';
        read_json5_string sep pos buf lexbuf
    | '\\', line_terminator_sequence -> read_json5_string sep pos buf lexbuf
    | '\\', Compl one_nine ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_substring buf matched 1 (String.length matched - 1);
        read_json5_string sep pos buf lexbuf
    | 0x2028 ->
        Buffer.add_utf_8_uchar buf (Uchar.of_int 0x2028);
        read_json5_string sep pos buf lexbuf
    | 0x2029 ->
        Buffer.add_utf_8_uchar buf (Uchar.of_int 0x2029);
        read_json5_string sep pos buf lexbuf
    | unicode_escape_sequence ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_string buf (unescape_utf8_char matched);
        read_json5_string sep pos buf lexbuf
    | Plus (Compl ('"' | '\'' | '\\' | line_terminator)) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_json5_string sep pos buf lexbuf
    | '"' | '\'' ->
        let m = Sedlexing.Utf8.lexeme lexbuf in
        if m.[0] = sep then Buffer.contents buf
        else (
          Buffer.add_string buf m;
          read_json5_string sep pos buf lexbuf)
    | eof ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "String is not terminated";
             })
    | _ ->
        raise
          (Parse_error
             {
               pos = Pos.of_lexing_pos (Sedlexing.lexing_bytes_positions lexbuf);
               message = "Parse error";
             })
