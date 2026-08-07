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

(** Decoding of string and regexp literal bodies.

    [Lexer] stores literals raw, so that the formatter can reproduce them
    verbatim; escapes are resolved later, by [render]. This lives outside
    [Lexer] because [Lexer] depends on [Parser] for the token type, while
    [Parser_helper] — which the grammar's semantic actions use — needs to decode
    literals too. *)

let skipped = [%sedlex.regexp? white_space | '\r' | '\t']
let oct_digit = [%sedlex.regexp? '0' .. '7']

(** [render ~pos ~sep s] resolves the escape sequences of a literal whose
    delimiter is [sep]. With [sep = '/'] (a regexp literal) the escapes the
    regexp engine handles itself are deliberately preserved.

    See https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
let render ~pos ~sep s =
  let buf = Buffer.create (String.length s) in
  let lexbuf = Sedlexing.Utf8.from_string (Printf.sprintf "%s%c" s sep) in
  let rec render () =
    match%sedlex lexbuf with
      | '\\', 'a' ->
          Buffer.add_char buf '\x07';
          render ()
      | '\\', 'b' ->
          Buffer.add_char buf '\b';
          render ()
      | '\\', 'e' ->
          Buffer.add_char buf '\x1b';
          render ()
      | '\\', 'f' ->
          Buffer.add_char buf '\x0c';
          render ()
      | '\\', 'n' ->
          Buffer.add_char buf '\n';
          render ()
      | '\\', 'r' ->
          Buffer.add_char buf '\r';
          render ()
      | '\\', 't' ->
          Buffer.add_char buf '\t';
          render ()
      | '\\', 'v' ->
          Buffer.add_char buf '\x0b';
          render ()
      | '\\', ('"' | '\'' | '/' | '\\') ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          (* For regexp, we want to make sure these are kept as-is
             and does not need any further escaping. *)
          if sep = '/' && matched.[1] <> '/' then
            Buffer.add_char buf matched.[0];
          Buffer.add_char buf matched.[1];
          render ()
      | '\\', '?' ->
          (* For regexp, we want to make sure \? is kept as-is
             and does not need any further escaping. *)
          if sep = '/' then (
            Buffer.add_char buf '\\';
            Buffer.add_char buf '?';
            render ())
          else (
            Buffer.add_char buf '\x3f';
            render ())
      | '\\', 'x', ascii_hex_digit, ascii_hex_digit ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let idx = String.index matched 'x' in
          let code = String.sub matched (idx + 1) 2 in
          let code = int_of_string (Printf.sprintf "0x%s" code) in
          Buffer.add_char buf (Char.chr code);
          render ()
      | '\\', oct_digit, oct_digit, oct_digit ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let idx = String.index matched '\\' in
          let code = String.sub matched (idx + 1) 3 in
          let code = min 255 (int_of_string (Printf.sprintf "0o%s" code)) in
          Buffer.add_char buf (Char.chr code);
          render ()
      | ( '\\',
          'u',
          ascii_hex_digit,
          ascii_hex_digit,
          ascii_hex_digit,
          ascii_hex_digit ) ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          Buffer.add_string buf (Lang_string.unescape_utf8_char matched);
          render ()
      (* Multiline string support: some text \
         Some more text *)
      | '\\', '\n', Star skipped -> render ()
      | '\\', any ->
          if sep <> '/' then (
            let pos = Pos.(to_string (of_lexing_pos pos)) in
            Printf.printf
              "Warning at position %s: illegal backslash escape in string.\n"
              pos);
          Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
          render ()
      | Plus (Compl ('"' | '\'' | '\\' | '/')) ->
          Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
          render ()
      | '"' | '\'' | '/' ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let c' = matched.[0] in
          if sep = c' then Buffer.contents buf
          else (
            Buffer.add_char buf c';
            render ())
      | eof ->
          let msg =
            if sep = '/' then "Regexp not terminated"
            else "String is not terminated"
          in
          raise (Term.Parse_error (pos, msg))
      | _ ->
          let msg =
            if sep = '/' then "Illegal regexp character: "
            else "Illegal string character: "
          in
          raise (Term.Parse_error (pos, msg ^ Sedlexing.Utf8.lexeme lexbuf))
  in
  render ()
