{
(*****************************************************************************

  Liqi, a simple wiki-like langage
  Copyright 2008-2011 Savonet team

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

  open Liqi_parser
  open Lexing

  let incrline lexbuf =
    String.iter
      (fun c -> if c = '\n' then
        lexbuf.lex_curr_p <- {
          lexbuf.lex_curr_p with
          pos_bol = lexbuf.lex_curr_p.pos_cnum ;
          pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum })
      (lexeme lexbuf)

  let bol lexbuf =
    let p = Lexing.lexeme_start_p lexbuf in
      p.Lexing.pos_cnum = p.Lexing.pos_bol

}

(* Yes, we need an unicode lexer.. *)
let letter = [
  'a'-'z' 'A'-'Z' '0'-'9'
  '\195''\160''\168''\169''\180''\185''\174''\175''\137'
  '\170' '\162' '\167' '-' '_' '+' '>' '<' '='
  '~' '&' '/' '\\' '[' ']'  '|' '^' '%'
]
let letter = letter | ("&" ['a'-'z' 'A'-'Z' '0'-'9']+ ";")
let punctuation = [ ':' '.' ';' ',' '?' '!' ]
let quotes = [ '"' '\'' ]
let markup = ['*''_']*

rule token = parse
  | [' ''\t']* ('*'+ as indent) ' ' {
      (* This is an emergency fix to support * in normal text...
       * We differentiate * at the beginning of line and inside a line.
       * This requires to consume the initial spaces together with the star.
       * We also need to require a space after the star, otherwise there is
       * a conflict in the lexer with markup, eg. this is **bold**. *)
      if bol lexbuf then LI (String.length indent) else WORD (Lexing.lexeme lexbuf) }
  | '\n'                 { incrline lexbuf ; NEWLINE }
  | '\n' eof             { incrline lexbuf ; EOF }
  | '\n' '\n'+           { incrline lexbuf ;
                           MANY_NEWLINES }
  | ' '+                 { SPACE }
  | "<code>" ([^'<']* as code) "</code>"
  | "@" ([^'@']* as code) "@"
                         { incrline lexbuf ; CODE code }
  | '\n'*
    "h" (['1'-'9']+ as n)
    ('@' ([^'\n''.']+ as anchor))?
    "." ' '* ([^'\n']+ as t) '\n'
                         { incrline lexbuf ; HEADER (int_of_string n,anchor,t) }
  | '\n'*
  "%%" ('(' ([^')']+ as title) ')')?([^'\n'' ']+ as language)? '\n'
    (([^'%']|'%'[^'%'])* '\n' as body)
    "%%"                 { incrline lexbuf ;
                           SNIPPET (title, body, language) }
  | '\n'*
    "<pre>" ([^'<']* as body) "</pre>"
                         { incrline lexbuf ;
                           SNIPPET (None, body, None) }
  | '\n'*
    "title:" [' ']* ([^'\n']* as title)'\n'
                         { incrline lexbuf ;
                           TITLE title }
  | "`antiquotation`" ([^'`']* as code) "`antiquotation`"
                         { incrline lexbuf ; ANTIQUOTE code }

  | '\n'*
    "!"
    ([^'(']+ as url)
    "("
    ([^')']+ as label)
    ")!"                 { incrline lexbuf ;
                           IMAGE (label,url) }

  | '"' ([^'"']+ as text) '"'
    ':' ([^' ''\n']+ [^'.'','';'':'' ''\n''('')''?''!'] as url)
                         { HREF (text,url) }

  | markup punctuation + markup
  | markup '(' | ')' markup
  | markup quotes + markup
  | markup letter + markup
                         { WORD (lexeme lexbuf) }
  | eof                  { EOF }
