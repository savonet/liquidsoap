(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
      (List.nth sub.Regexp.matches n)
  in
  try
    let rex =
      Regexp.regexp "^((?:\\d+w)?)((?:\\d+h)?)((?:\\d+m)?)((?:\\d+s)?)$"
    in
    let sub = Regexp.exec rex t in
    let g = g sub in
    List.map g [1; 2; 3; 4]
  with Not_found ->
    let rex = Regexp.regexp "^((?:\\d+w)?)(\\d+h)(\\d+)$" in
    let sub = Regexp.exec rex t in
    let g = g sub in
    [
      g 1;
      g 2;
      Some (int_of_string (Option.get (List.nth sub.Regexp.matches 3)));
      None;
    ]

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

let var_char = [%sedlex.regexp? alphabetic | other_alphabetic]
let var_underscore = [%sedlex.regexp? '_', Plus '_']

let var_lit =
  [%sedlex.regexp?
    ( var_underscore
    | Star '_', var_char, Star (var_char | decimal_digit | '_' | '\'') )]

let var = [%sedlex.regexp? var_lit | so | math | other_math]
let encoder = [%sedlex.regexp? '%', Plus (var_char | decimal_digit | '.' | '_')]

let time =
  [%sedlex.regexp?
    ( Opt (Plus decimal_digit, 'w'), Plus decimal_digit, 'h', Plus decimal_digit
    | ( Plus decimal_digit,
        'w',
        Opt (Plus decimal_digit, 'h'),
        Opt (Plus decimal_digit, 'm'),
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Plus decimal_digit,
        'h',
        Opt (Plus decimal_digit, 'm'),
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Opt (Plus decimal_digit, 'h'),
        Plus decimal_digit,
        'm',
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Opt (Plus decimal_digit, 'h'),
        Opt (Plus decimal_digit, 'm'),
        Plus decimal_digit,
        's' ) )]

let rec token lexbuf =
  match%sedlex lexbuf with
    | skipped -> token lexbuf
    | Star white_space, '#', '<' ->
        let buf = Buffer.create 1024 in
        let pos = Sedlexing.lexing_bytes_positions lexbuf in
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment pos buf lexbuf;
        PP_COMMENT (Regexp.split (Regexp.regexp "\n") (Buffer.contents buf))
    | "#", Star white_space ->
        let buf = Buffer.create 1024 in
        let pos = Sedlexing.lexing_bytes_positions lexbuf in
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_comment pos buf lexbuf;
        PP_COMMENT (Regexp.split (Regexp.regexp "\n") (Buffer.contents buf))
    | line_break -> PP_ENDL
    | "%ifdef", Plus white_space, var, Star ("" | '.', var) ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.indexp_from matched 6 (fun c -> c <> ' ') in
        let r = String.rindexp matched (fun c -> c <> ' ') in
        PP_IFDEF (String.sub matched n (r - n + 1))
    | "%ifndef", Plus white_space, var, Star ("" | '.', var) ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.indexp_from matched 7 (fun c -> c <> ' ') in
        let r = String.rindexp matched (fun c -> c <> ' ') in
        PP_IFNDEF (String.sub matched n (r - n + 1))
    | ( "%ifversion",
        Plus white_space,
        ("==" | ">=" | "<=" | "<" | ">"),
        Plus ' ',
        Plus (decimal_digit | '.') ) ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n1 = String.indexp_from matched 10 (fun c -> c <> ' ') in
        let n2 = String.indexp_from matched n1 (fun c -> c = ' ') in
        let n3 = String.indexp_from matched n2 (fun c -> c <> ' ') in
        let r = String.rindexp matched (fun c -> c <> ' ') in
        let cmp = String.sub matched n1 (n2 - n1) in
        let ver = String.sub matched n3 (r - n3 + 1) in
        let cmp =
          match cmp with
            | "==" -> `Eq
            | ">=" -> `Geq
            | "<=" -> `Leq
            | "<" -> `Lt
            | ">" -> `Gt
            | _ -> assert false
        in
        PP_IFVERSION (cmp, ver)
    | "%ifencoder" -> PP_IFENCODER
    | "%ifnencoder" -> PP_IFNENCODER
    | "%else" -> PP_ELSE
    | "%endif" -> PP_ENDIF
    | "%include_extra", Star (white_space | '\t'), '"', Star (Compl '"'), '"' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '"' in
        let r = String.rindex matched '"' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        PP_INCLUDE_EXTRA file
    | "%include", Star (white_space | '\t'), '"', Star (Compl '"'), '"' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '"' in
        let r = String.rindex matched '"' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        PP_INCLUDE file
    | "%include", Star (white_space | '\t'), '<', Star (Compl '>'), '>' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '<' in
        let r = String.rindex matched '>' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        PP_INCLUDE (Filename.concat (!Hooks.liq_libs_dir ()) file)
    | "%argsof" -> ARGS_OF
    | '#', Star (Compl '\n'), eof -> EOF
    | eof -> EOF
    | "def", Plus skipped, "rec", Plus skipped -> PP_DEF `Recursive
    | "def", Plus skipped, "replaces", Plus skipped -> PP_DEF `Replaces
    | "def" -> PP_DEF `None
    | "try" -> TRY
    | "catch" -> CATCH
    | "do" -> DO
    | "let", Plus skipped, "replaces", Plus skipped -> LET `Replaces
    | "let", Plus skipped, "eval", Plus skipped -> LET `Eval
    | "let", Plus skipped, "json.parse", Star skipped, '[' ->
        LETLBRA `Json_parse
    | "let", Plus skipped, "json.parse", Plus skipped -> LET `Json_parse
    | "let", Plus skipped, "yaml.parse", Plus skipped -> LET `Yaml_parse
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
    | "::" -> COLONCOLON
    | ':' -> COLON
    | ';' -> SEQ
    | ";;" -> SEQSEQ
    | "~" -> TILD
    | "?" -> QUESTION
    | "-" -> MINUS
    | "while" -> WHILE
    | "for" -> FOR
    | "to" -> TO
    | "do" -> DO
    | "not" -> NOT
    | "open" -> OPEN
    | "and" | "or" -> BINB (Sedlexing.Utf8.lexeme lexbuf)
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
    | int_literal -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | Plus decimal_digit, ".{" ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let matched = String.sub matched 0 (String.length matched - 2) in
        PP_INT_DOT_LCUR (int_of_string matched)
    | Star decimal_digit, '.', Star decimal_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched '.' in
        let ipart = String.sub matched 0 idx in
        let fpart =
          String.sub matched (idx + 1) (String.length matched - idx - 1)
        in
        let fpart =
          if fpart = "" then 0.
          else
            float_of_string fpart /. (10. ** float_of_int (String.length fpart))
        in
        let ipart = if ipart = "" then 0. else float_of_string ipart in
        FLOAT (ipart +. fpart)
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
        PP_STRING (s, (startp, endp))
    | '\'' ->
        let startp, _ = Sedlexing.lexing_bytes_positions lexbuf in
        let s = read_string '\'' startp (Buffer.create 17) lexbuf in
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        PP_STRING (s, (startp, endp))
    | "r/" ->
        let startp, _ = Sedlexing.lexing_bytes_positions lexbuf in
        let regexp = read_string '/' startp (Buffer.create 17) lexbuf in
        let flags = read_regexp_flags [] lexbuf in
        let _, endp = Sedlexing.lexing_bytes_positions lexbuf in
        PP_REGEXP (regexp, flags, (startp, endp))
    | any ->
        raise
          (Term.Parse_error
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

and read_comment pos buf lexbuf =
  match%sedlex lexbuf with
    | '\n', Star white_space, '#', Star white_space ->
        Buffer.add_char buf '\n';
        read_comment pos buf lexbuf
    | '\n' -> ()
    | Plus (Compl '\n') ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_comment pos buf lexbuf
    | eof -> ()
    | _ ->
        raise
          (Term.Parse_error
             (pos, "Illegal character: " ^ Sedlexing.Utf8.lexeme lexbuf))

and read_multiline_comment ?(level = 0) pos buf lexbuf =
  match%sedlex lexbuf with
    | '>', '#' ->
        if level = 0 then ()
        else (
          Buffer.add_string buf ">#";
          read_multiline_comment ~level:(level - 1) pos buf lexbuf)
    | '#', '<' ->
        Buffer.add_string buf "#<";
        read_multiline_comment ~level:(level + 1) pos buf lexbuf
    | '#' | '>' ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment ~level pos buf lexbuf
    | Plus (Intersect (Compl '>', Compl '#')) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_multiline_comment ~level pos buf lexbuf
    | eof -> raise (Term.Parse_error (pos, "Multiline comment not terminated!"))
    | _ ->
        raise
          (Term.Parse_error
             (pos, "Illegal character: " ^ Sedlexing.Utf8.lexeme lexbuf))

and read_string c pos buf lexbuf =
  (* See: https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
  match%sedlex lexbuf with
    | '\\', 'a' ->
        Buffer.add_char buf '\x07';
        read_string c pos buf lexbuf
    | '\\', 'b' ->
        Buffer.add_char buf '\b';
        read_string c pos buf lexbuf
    | '\\', 'e' ->
        Buffer.add_char buf '\x1b';
        read_string c pos buf lexbuf
    | '\\', 'f' ->
        Buffer.add_char buf '\x0c';
        read_string c pos buf lexbuf
    | '\\', 'n' ->
        Buffer.add_char buf '\n';
        read_string c pos buf lexbuf
    | '\\', 'r' ->
        Buffer.add_char buf '\r';
        read_string c pos buf lexbuf
    | '\\', 't' ->
        Buffer.add_char buf '\t';
        read_string c pos buf lexbuf
    | '\\', 'v' ->
        Buffer.add_char buf '\x0b';
        read_string c pos buf lexbuf
    | '\\', ('"' | '\'' | '/' | '\\') ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        (* For regexp, we want to make sure these are kept as-is
           and does not need any further escaping. *)
        if c = '/' && matched.[1] <> '/' then Buffer.add_char buf matched.[0];
        Buffer.add_char buf matched.[1];
        read_string c pos buf lexbuf
    | '\\', '?' ->
        (* For regexp, we want to make sure \? is kept as-is
           and does not need any further escaping. *)
        if c = '/' then (
          Buffer.add_char buf '\\';
          Buffer.add_char buf '?';
          read_string c pos buf lexbuf)
        else (
          Buffer.add_char buf '\x3f';
          read_string c pos buf lexbuf)
    | '\\', 'x', ascii_hex_digit, ascii_hex_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched 'x' in
        let code = String.sub matched (idx + 1) 2 in
        let code = int_of_string (Printf.sprintf "0x%s" code) in
        Buffer.add_char buf (Char.chr code);
        read_string c pos buf lexbuf
    | '\\', oct_digit, oct_digit, oct_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched '\\' in
        let code = String.sub matched (idx + 1) 3 in
        let code = min 255 (int_of_string (Printf.sprintf "0o%s" code)) in
        Buffer.add_char buf (Char.chr code);
        read_string c pos buf lexbuf
    | ( '\\',
        'u',
        ascii_hex_digit,
        ascii_hex_digit,
        ascii_hex_digit,
        ascii_hex_digit ) ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_string buf (Lang_string.unescape_utf8_char matched);
        read_string c pos buf lexbuf
    (* Multiline string support: some text \
       Some more text *)
    | '\\', '\n', Star skipped -> read_string c pos buf lexbuf
    | '\\', any ->
        if c <> '/' then (
          let pos =
            Pos.to_string (pos, snd (Sedlexing.lexing_bytes_positions lexbuf))
          in
          Printf.printf
            "Warning at position %s: illegal backslash escape in string.\n" pos);
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
          (Term.Parse_error
             ((pos, snd (Sedlexing.lexing_bytes_positions lexbuf)), msg))
    | _ ->
        let msg =
          if c = '/' then "Illegal regexp character: "
          else "Illegal string character: "
        in
        raise
          (Term.Parse_error
             ( (pos, snd (Sedlexing.lexing_bytes_positions lexbuf)),
               msg ^ Sedlexing.Utf8.lexeme lexbuf ))
