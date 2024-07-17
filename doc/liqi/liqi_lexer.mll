{
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
}

(* Yes, we need an unicode lexer.. *)
let letter = [
  'a'-'z' 'A'-'Z' '0'-'9'
  '\195''\160''\168''\169''\180''\185''\174''\175'
  '-' '_' '+'
  '~' '&' '/'
]
let punctuation = [ ':' '.' ';' ',' '?' '!' ]
let quotes = [ '"' '\'' ]
let markup = ['*''_']*

rule token = parse
  | '*'                  { LI }
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
    "%%" ('(' ([^')']+ as title) ')')? '\n'
    (([^'%']|'%'[^'%'])* '\n' as body)
    "%%"                 { incrline lexbuf ;
                           SNIPPET (title, body) }
  | '\n'*
    "<pre>" ([^'<']* as body) "</pre>"
                         { incrline lexbuf ;
                           SNIPPET (None, body) }
  | '\n'*
    "title:" [' ']* ([^'\n']* as title)'\n'
                         { incrline lexbuf ;
                           TITLE title }
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
