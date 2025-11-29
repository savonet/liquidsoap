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

open Parser

module String = struct
  include String

  (** Find character satisfying a predicate from a particular index. *)
  let indexp_from s n p =
    let l = String.length s in
    let n = ref n in
    try
      while !n < l do
        if p s.[!n] then raise Exit;
        incr n
      done;
      raise Not_found
    with Exit -> !n

  let rindexp s p =
    let n = ref (String.length s - 1) in
    try
      while !n >= 0 do
        if p s.[!n] then raise Exit;
        decr n
      done;
      raise Not_found
    with Exit -> !n
end

let parse_time t =
  let g sub n =
    (function
      | None | Some "" -> None
      | Some s -> Some (int_of_string (String.sub s 0 (String.length s - 1))))
      (try Some (Re.Pcre.get_substring sub n) with _ -> None)
  in
  try
    let rex =
      Re.Pcre.regexp "^((?:\\d+w)?)((?:\\d+h)?)((?:\\d+m)?)((?:\\d+s)?)$"
    in
    let sub = Re.Pcre.exec ~rex t in
    let g = g sub in
    { Parsed_term.week = g 1; hours = g 2; minutes = g 3; seconds = g 4 }
  with Not_found ->
    let rex = Re.Pcre.regexp "^((?:\\d+w)?)(\\d+h)(\\d+)$" in
    let sub = Re.Pcre.exec ~rex t in
    let g = g sub in
    {
      Parsed_term.week = g 1;
      hours = g 2;
      minutes = Some (int_of_string (Re.Pcre.get_substring sub 3));
      seconds = None;
    }

(* See: https://en.wikipedia.org/wiki/Whitespace_character *)
let line_break =
  [%sedlex.regexp? '\n' | '\r' | '\x0C' | '\x0B' | 0x2028 | 0x2029]

let white_space = [%sedlex.regexp? Sub (white_space, line_break)]
let skipped = [%sedlex.regexp? white_space | '\r' | '\t']
let decimal_digit = [%sedlex.regexp? '0' .. '9']

let decimal_literal =
  [%sedlex.regexp? decimal_digit, Star (decimal_digit | '_')]

let hex_literal =
  [%sedlex.regexp?
    '0', ('x' | 'X'), ascii_hex_digit, Star (ascii_hex_digit | '_')]

let oct_digit = [%sedlex.regexp? '0' .. '7']

let oct_literal =
  [%sedlex.regexp? '0', ('o' | 'O'), oct_digit, Star (oct_digit | '_')]

let bin_digit = [%sedlex.regexp? '0' | '1']

let bin_literal =
  [%sedlex.regexp? '0', ('b' | 'B'), bin_digit, Star (bin_digit | '_')]

let int_literal =
  [%sedlex.regexp? decimal_literal | hex_literal | oct_literal | bin_literal]

let var_char = [%sedlex.regexp? alphabetic]
let var_underscore = [%sedlex.regexp? '_', Plus '_']

let var_lit =
  [%sedlex.regexp?
    ( var_underscore
    | Star '_', var_char, Star (var_char | decimal_digit | '_' | '\'') )]

let var = [%sedlex.regexp? var_lit | so]
let encoder = [%sedlex.regexp? '%', Plus (var_char | decimal_digit | '.' | '_')]

let time =
  [%sedlex.regexp?
    ( Opt (decimal_literal, 'w'), decimal_literal, 'h', decimal_literal
    | ( decimal_literal,
        'w',
        Opt (decimal_literal, 'h'),
        Opt (decimal_literal, 'm'),
        Opt (decimal_literal, 's') )
    | ( Opt (decimal_literal, 'w'),
        decimal_literal,
        'h',
        Opt (decimal_literal, 'm'),
        Opt (decimal_literal, 's') )
    | ( Opt (decimal_literal, 'w'),
        Opt (decimal_literal, 'h'),
        decimal_literal,
        'm',
        Opt (decimal_literal, 's') )
    | ( Opt (decimal_literal, 'w'),
        Opt (decimal_literal, 'h'),
        Opt (decimal_literal, 'm'),
        decimal_literal,
        's' ) )]

let rec token lexbuf =
  match%sedlex lexbuf with
    | Plus skipped -> token lexbuf
    | Star white_space, '#', '<' ->
        let buf = Buffer.create 1024 in
        let ((startp, _) as pos) = Sedlexing.lexing_bytes_positions lexbuf in
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment pos buf lexbuf;
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        Parser_helper.append_comment ~pos:(startp, endp) (Buffer.contents buf);
        token lexbuf
    | "#", Star white_space ->
        let buf = Buffer.create 1024 in
        let ((startp, _) as pos) = Sedlexing.lexing_bytes_positions lexbuf in
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        let _, endp = read_comment pos buf lexbuf in
        Parser_helper.append_comment ~pos:(startp, endp) (Buffer.contents buf);
        token lexbuf
    | line_break -> PP_ENDL
    | "%ifdef" -> PP_IFDEF false
    | "%ifndef" -> PP_IFDEF true
    | "%ifversion" -> PP_IFVERSION
    | "%ifencoder" -> PP_IFENCODER false
    | "%ifnencoder" -> PP_IFENCODER true
    | "%else" -> PP_ELSE
    | "%endif" -> PP_ENDIF
    | "%include_extra", Star (white_space | '\t'), '"', Star (Compl '"'), '"' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '"' in
        let r = String.rindex matched '"' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        INCLUDE
          {
            inc_type = `Extra;
            inc_name = file;
            inc_pos = Sedlexing.lexing_positions lexbuf;
          }
    | "%include", Star (white_space | '\t'), '"', Star (Compl '"'), '"' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '"' in
        let r = String.rindex matched '"' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        INCLUDE
          {
            inc_type = `Default;
            inc_name = file;
            inc_pos = Sedlexing.lexing_positions lexbuf;
          }
    | "%include", Star (white_space | '\t'), '<', Star (Compl '>'), '>' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '<' in
        let r = String.rindex matched '>' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        INCLUDE
          {
            inc_type = `Lib;
            inc_name = file;
            inc_pos = Sedlexing.lexing_positions lexbuf;
          }
    | "%argsof" -> ARGS_OF
    | '#', Star (Compl '\n'), eof -> EOF
    | eof -> EOF
    | "def", Plus skipped, "rec", Plus skipped -> DEF `Recursive
    | "def", Plus skipped, "replaces", Plus skipped -> DEF `Replaces
    | "def" -> DEF `None
    | "try" -> TRY
    | "finally" -> FINALLY
    | "catch" -> CATCH
    | "do" -> DO
    | "let", Plus skipped, "replaces", Plus skipped -> LET `Replaces
    | "let", Plus skipped, "eval", Plus skipped -> LET `Eval
    | "let", Plus skipped, "json.parse", Star skipped, '[' ->
        LETLBRA `Json_parse
    | "let", Plus skipped, "json.parse", Plus skipped -> LET `Json_parse
    | "let", Plus skipped, "yaml.parse", Plus skipped -> LET `Yaml_parse
    | "let", Plus skipped, "sqlite.row", Plus skipped -> LET `Sqlite_row
    | "let", Plus skipped, "sqlite.query", Plus skipped -> LET `Sqlite_query
    | "let", Plus skipped, "xml.parse", Plus skipped -> LET `Xml_parse
    | "let" -> LET `None
    | "fun" -> FUN
    | '=' -> GETS
    | "end" -> END
    | "begin" -> BEGIN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "elsif" -> ELSIF
    | "->" -> YIELDS
    | "null", "." -> NULLDOT
    | "null" -> NULL
    | encoder ->
        let e = Sedlexing.Utf8.lexeme lexbuf in
        let e = String.sub e 1 (String.length e - 1) in
        ENCODER e
    | '.', Star skipped, var ->
        let m = Sedlexing.Utf8.lexeme lexbuf in
        let lexbuf = Sedlexing.Utf8.from_string m in
        let rec f () =
          match%sedlex lexbuf with
            | '.', Star skipped -> f ()
            | var -> DOTVAR (Sedlexing.Utf8.lexeme lexbuf)
            | _ -> assert false
        in
        f ()
    | '.' -> DOT
    | "..." -> DOTDOTDOT
    | '[' -> LBRA
    | ']' -> RBRA
    | '(' -> LPAR
    | ')' -> RPAR
    | '{' -> LCUR
    | '}' -> RCUR
    | ',' -> COMMA
    | '@' -> AT
    | "::" -> COLONCOLON
    | ':' -> COLON
    | ';' -> SEQ
    | ";;" -> SEQSEQ
    | "~" -> TILD
    | "?." -> QUESTION_DOT
    | "?" -> QUESTION
    | "-" -> MINUS
    | "while" -> WHILE
    | "for" -> FOR
    | "to" -> TO
    | "do" -> DO
    | "not" -> NOT
    | "open" -> OPEN
    | "and" -> AND
    | "or" -> OR
    | "!=" | "==" | "<" | "<=" | ">" | ">=" ->
        BIN1 (Sedlexing.Utf8.lexeme lexbuf)
    | "+" | "%" | "^" | "+." | "-." -> BIN2 (Sedlexing.Utf8.lexeme lexbuf)
    | "/" | "*." | "/." -> BIN3 (Sedlexing.Utf8.lexeme lexbuf)
    | "mod" -> BIN3 (Sedlexing.Utf8.lexeme lexbuf)
    | "??" -> COALESCE
    | "*" -> TIMES
    | "!" -> GET
    | ":=" -> SET
    | '_' -> UNDERSCORE
    | "true" -> BOOL true
    | "false" -> BOOL false
    | int_literal -> INT (Sedlexing.Utf8.lexeme lexbuf)
    | decimal_literal, ".{" ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let matched = String.sub matched 0 (String.length matched - 2) in
        PP_INT_DOT_LCUR matched
    | ( Star decimal_literal,
        Star ('.', Star decimal_literal),
        ('e' | 'E'),
        Star ('+' | '-'),
        decimal_literal ) ->
        FLOAT (Sedlexing.Utf8.lexeme lexbuf)
    | Star decimal_literal, '.', Star decimal_literal ->
        FLOAT (Sedlexing.Utf8.lexeme lexbuf)
    | ( decimal_literal,
        ".",
        decimal_literal,
        ".",
        decimal_literal,
        Star (Compl (white_space | line_break)) ) ->
        VERSION (Lang_string.Version.of_string (Sedlexing.Utf8.lexeme lexbuf))
    | time -> TIME (parse_time (Sedlexing.Utf8.lexeme lexbuf))
    | time, Star skipped, '-', Star skipped, time ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched '-' in
        let t1 = String.sub matched 0 idx in
        let t1 = String.trim t1 in
        let t2 =
          String.sub matched (idx + 1) (String.length matched - idx - 1)
        in
        let t2 = String.trim t2 in
        INTERVAL (parse_time t1, parse_time t2)
    | var -> VAR (Sedlexing.Utf8.lexeme lexbuf)
    | '"' ->
        let startp, _ = Sedlexing.lexing_bytes_positions lexbuf in
        let s = read_string '"' startp (Buffer.create 17) lexbuf in
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        PP_STRING ('"', s, (startp, endp))
    | '\'' ->
        let startp, _ = Sedlexing.lexing_bytes_positions lexbuf in
        let s = read_string '\'' startp (Buffer.create 17) lexbuf in
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        PP_STRING ('\'', s, (startp, endp))
    | "r/" ->
        let startp, _ = Sedlexing.lexing_bytes_positions lexbuf in
        let regexp = read_string '/' startp (Buffer.create 17) lexbuf in
        let flags = read_regexp_flags [] lexbuf in
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        PP_REGEXP (regexp, flags, (startp, endp))
    | any ->
        raise
          (Term_base.Parse_error
             ( Sedlexing.lexing_bytes_positions lexbuf,
               "Parse error: " ^ Sedlexing.Utf8.lexeme lexbuf ))
    | _ -> failwith "Internal error"

and read_regexp_flags flags lexbuf =
  match%sedlex lexbuf with
    | 'g' | 'i' | 's' | 'm' | 'u' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        read_regexp_flags (matched.[0] :: flags) lexbuf
    | _ ->
        Sedlexing.rollback lexbuf;
        flags

and read_comment_end ((startp, _) as pos) buf lexbuf =
  match%sedlex lexbuf with
    | '\n', Star white_space ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        (startp, endp)
    | _ ->
        raise
          (Term_base.Parse_error
             (pos, "Illegal character: " ^ Sedlexing.Utf8.lexeme lexbuf))

and read_comment ((startp, _) as pos) buf lexbuf =
  match%sedlex lexbuf with
    | '\n', Star white_space, '#', '<' ->
        Sedlexing.rollback lexbuf;
        read_comment_end pos buf lexbuf
    | '\n', Star white_space, '#', Star white_space ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        read_comment (startp, endp) buf lexbuf
    | '\n' -> pos
    | Plus (Compl '\n') ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        read_comment (startp, endp) buf lexbuf
    | eof -> pos
    | _ ->
        raise
          (Term_base.Parse_error
             (pos, "Illegal character: " ^ Sedlexing.Utf8.lexeme lexbuf))

and read_multiline_comment ?(level = 0) pos buf lexbuf =
  match%sedlex lexbuf with
    | '>', '#' ->
        Buffer.add_string buf ">#";
        if level = 0 then ()
        else read_multiline_comment ~level:(level - 1) pos buf lexbuf
    | '#', '<' ->
        Buffer.add_string buf "#<";
        read_multiline_comment ~level:(level + 1) pos buf lexbuf
    | '#' | '>' ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment ~level pos buf lexbuf
    | Plus (Intersect (Compl '>', Compl '#')) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment ~level pos buf lexbuf
    | eof ->
        raise (Term_base.Parse_error (pos, "Multiline comment not terminated!"))
    | _ ->
        raise
          (Term_base.Parse_error
             (pos, "Illegal character: " ^ Sedlexing.Utf8.lexeme lexbuf))

and read_string c pos buf lexbuf =
  match%sedlex lexbuf with
    | '\\', Opt any ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_string c pos buf lexbuf
    | Plus (Compl ('"' | '\'' | '\\' | '/')) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_string c pos buf lexbuf
    | '"' | '\'' | '/' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let c' = matched.[0] in
        if c = c' then Buffer.contents buf
        else (
          Buffer.add_char buf c';
          read_string c pos buf lexbuf)
    | eof ->
        let msg =
          if c = '/' then "Regexp not terminated"
          else "String is not terminated"
        in
        raise
          (Term_base.Parse_error
             ((pos, snd (Sedlexing.lexing_bytes_positions lexbuf)), msg))
    | _ ->
        let msg =
          if c = '/' then "Illegal regexp character: "
          else "Illegal string character: "
        in
        raise
          (Term_base.Parse_error
             ( (pos, snd (Sedlexing.lexing_bytes_positions lexbuf)),
               msg ^ Sedlexing.Utf8.lexeme lexbuf ))

let render_string ~pos ~sep s =
  let buf = Buffer.create (String.length s) in
  let lexbuf = Sedlexing.Utf8.from_string (Printf.sprintf "%s%c" s sep) in
  let rec render_string () =
    (* See: https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
      match%sedlex lexbuf with
      | '\\', 'a' ->
          Buffer.add_char buf '\x07';
          render_string ()
      | '\\', 'b' ->
          Buffer.add_char buf '\b';
          render_string ()
      | '\\', 'e' ->
          Buffer.add_char buf '\x1b';
          render_string ()
      | '\\', 'f' ->
          Buffer.add_char buf '\x0c';
          render_string ()
      | '\\', 'n' ->
          Buffer.add_char buf '\n';
          render_string ()
      | '\\', 'r' ->
          Buffer.add_char buf '\r';
          render_string ()
      | '\\', 't' ->
          Buffer.add_char buf '\t';
          render_string ()
      | '\\', 'v' ->
          Buffer.add_char buf '\x0b';
          render_string ()
      | '\\', ('"' | '\'' | '/' | '\\') ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          (* For regexp, we want to make sure these are kept as-is
             and does not need any further escaping. *)
          if sep = '/' && matched.[1] <> '/' then
            Buffer.add_char buf matched.[0];
          Buffer.add_char buf matched.[1];
          render_string ()
      | '\\', '?' ->
          (* For regexp, we want to make sure \? is kept as-is
             and does not need any further escaping. *)
          if sep = '/' then (
            Buffer.add_char buf '\\';
            Buffer.add_char buf '?';
            render_string ())
          else (
            Buffer.add_char buf '\x3f';
            render_string ())
      | '\\', 'x', ascii_hex_digit, ascii_hex_digit ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let idx = String.index matched 'x' in
          let code = String.sub matched (idx + 1) 2 in
          let code = int_of_string (Printf.sprintf "0x%s" code) in
          Buffer.add_char buf (Char.chr code);
          render_string ()
      | '\\', oct_digit, oct_digit, oct_digit ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let idx = String.index matched '\\' in
          let code = String.sub matched (idx + 1) 3 in
          let code = min 255 (int_of_string (Printf.sprintf "0o%s" code)) in
          Buffer.add_char buf (Char.chr code);
          render_string ()
      | ( '\\',
          'u',
          ascii_hex_digit,
          ascii_hex_digit,
          ascii_hex_digit,
          ascii_hex_digit ) ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          Buffer.add_string buf (Lang_string.unescape_utf8_char matched);
          render_string ()
      (* Multiline string support: some text \
         Some more text *)
      | '\\', '\n', Star skipped -> render_string ()
      | '\\', any ->
          if sep <> '/' then (
            let pos = Pos.(to_string (of_lexing_pos pos)) in
            Printf.printf
              "Warning at position %s: illegal backslash escape in string.\n"
              pos);
          Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
          render_string ()
      | Plus (Compl ('"' | '\'' | '\\' | '/')) ->
          Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
          render_string ()
      | '"' | '\'' | '/' ->
          let matched = Sedlexing.Utf8.lexeme lexbuf in
          let c' = matched.[0] in
          if sep = c' then Buffer.contents buf
          else (
            Buffer.add_char buf c';
            render_string ())
      | eof ->
          let msg =
            if sep = '/' then "Regexp not terminated"
            else "String is not terminated"
          in
          raise (Term_base.Parse_error (pos, msg))
      | _ ->
          let msg =
            if sep = '/' then "Illegal regexp character: "
            else "Illegal string character: "
          in
          raise
            (Term_base.Parse_error (pos, msg ^ Sedlexing.Utf8.lexeme lexbuf))
  in
  render_string ()

let () =
  Parser_helper.render_string_ref :=
    fun ~pos (sep, s) -> render_string ~pos ~sep s
