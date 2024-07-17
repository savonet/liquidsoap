%{
  open Liqi
%}

%token <string> CODE
%token <string option * string> SNIPPET
%token <string * string> IMAGE
%token <string*string> HREF
%token <int*string option*string> HEADER
%token <string> WORD
%token <string> TITLE
%token SPACE MANY_NEWLINES NEWLINE LI LEM REM LBF RBF EOF

%start doc_title
%type <string option * Liqi.doc> doc_title

%%

doc_title:
  | TITLE doc { Some $1, $2 }
  | doc { None, $1 }

doc:
  | HEADER doc { (Header $1) :: $2 }
  | SNIPPET doc { (Snippet $1) :: $2 }
  | IMAGE doc { (Image $1) :: $2 }
  | nl doc { $2 }
  | paragraph MANY_NEWLINES doc { Paragraph $1 :: $3 }
  | paragraph doc_nopar { Paragraph $1 :: $2 }
  | paragraph EOF { [Paragraph $1] }
  | EOF { [] }

doc_nopar:
  | HEADER doc { (Header $1) :: $2 }
  | SNIPPET doc { (Snippet $1) :: $2 }
  | IMAGE doc { (Image $1) :: $2 }

nl:
  | NEWLINE { () } | MANY_NEWLINES { () }

paragraph:
  | indentation line NEWLINE paragraph { ($1,$2) :: $4 }
  | indentation line { [$1,$2] }

indentation:
  | LI indentation { 1 + $2 }
  | spaces { 0 }
spaces:
  | { () } | SPACE spaces { () }

line:
  | item in_line { $1 :: $2 }
in_line:
  | { [] }
  | in_item in_line { $1 :: $2 }

item:
  | WORD { Word $1 }
  | HREF { HRef $1 }
  | CODE { Code $1 }
  | LEM line REM { Em $2 }
  | LBF line RBF { Bf $2 }
in_item:
  | SPACE { Space }
  | item  { $1 }
