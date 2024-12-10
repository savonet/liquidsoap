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

%{
open Parsed_term
(* All auxiliary functions for parser are there *)
open Parser_helper
%}

%token <string> VAR
%token <string> VARLPAR
%token <string> VARLBRA
%token <Lang_string.Version.t> VERSION
%token <char * string * Parsed_term.pos> PP_STRING
%token <string * char list * Parsed_term.pos> PP_REGEXP
%token <char * string > STRING
%token <string * char list > REGEXP
%token <string> INT PP_INT_DOT_LCUR
%token <string> FLOAT
%token <bool> BOOL
%token <Parsed_term.time_el> TIME
%token <Parsed_term.time_el * Parsed_term.time_el> INTERVAL
%token <string> ENCODER
%token EOF
%token <Parser_helper.lexer_let_decoration> LET
%token <Parser_helper.lexer_let_decoration> LETLBRA
%token BEGIN END GETS TILD QUESTION
%token QUESTION_DOT
(* name, arguments, methods *)
%token <Parser_helper.lexer_let_decoration> DEF
%token REPLACES
%token COALESCE
%token TRY CATCH FINALLY DO
%token IF THEN ELSE ELSIF
%token SLASH
%token OPEN
%token LPAR RPAR COMMA SEQ SEQSEQ COLON COLONCOLON DOT
%token <string> DOTVAR
%token LBRA RBRA LCUR RCUR
%token FUN YIELDS
%token DOTDOTDOT
%token AND OR
%token <string> BIN1
%token <string> BIN2
%token <string> BIN3
%token AT
%token TIMES
%token MINUS UMINUS
%token UNDERSCORE
%token NOT
%token GET SET
%token <bool> PP_IFDEF
%token PP_IFVERSION
%token ARGS_OF
%token <bool> PP_IFENCODER
%token PP_ELSE PP_ENDIF
%token PP_ENDL
%token <char> BEGIN_INTERPOLATION
%token END_INTERPOLATION
%token <string> INTERPOLATED_STRING
%token <Parsed_term.inc> INCLUDE
%token WHILE FOR TO

%nonassoc YIELDS       (* fun x -> (x+x) *)
%right SET             (* expr := (expr + expr), expr := (expr := expr) *)
%nonassoc QUESTION     (* x ? y : z *)
%left AND             (* ((x+(y*z))==3) or ((not a)==b) *)
%left OR
%nonassoc NOT
%left BIN1 AT
%left BIN2 MINUS
%left BIN3 TIMES
%nonassoc COALESCE     (* (x ?? y) == z *)
%nonassoc QUESTION_DOT (* (x ?. y) == z *)
%right COLONCOLON
%nonassoc GET          (* (!x)+2 *)
%left DOT
%nonassoc COLON

(* Read %ogg(...) as one block, shifting LPAR rather than reducing %ogg *)
%nonassoc no_app
%nonassoc LPAR

%nonassoc UMINUS

%start program
%type <Term.t> program

%start interactive
%type <Term.t> interactive

%start annotate
%type <(string * string) list> annotate

%start annotate_metadata_entry
%type <string * string> annotate_metadata_entry

%start time_predicate
%type <Term.t> time_predicate

%start plain_encoder_params
%type <Term.encoder_params> plain_encoder_params

%type <Parsed_term.let_decoration> _let
%type <string> annotate_key
%type <(string * string) list> annotate_metadata
%type <string> annotate_value
%type <Parsed_term.app_arg list> app_list
%type <Parsed_term.app_arg> app_list_elem
%type <Parser_helper.arglist> arglist
%type <string list * string list> args_of_params
%type <Term.type_annotation Type.argument list> argsty
%type <Term.type_annotation Type.argument> argty
%type <Parser_helper.explicit_binding> explicit_binding
%type <Parser_helper.binding> binding
%type <Term.encoder_params> encoder_params
%type <Term.t> expr
%type <Term.t> exprs
%type <Term.t> simple_fun_body
%type <unit> g
%type <(Term.t * Term.t) list * Term.t option> if_elsif
%type <string list> in_subfield
%type <string list> in_subfield_lbra
%type <Parsed_term.list_el list> inner_list
%type <Parsed_term.list_el> inner_list_item
%type <Term.t list> inner_tuple
%type <Parser_helper.let_opt_el list> let_opt
%type <Parser_helper.let_opt_el> let_opt_el
%type <Term.meth_annotation> meth_ty
%type <Term.t option> opt
%type <string> optvar
%type <Term.pattern> pattern
%type <Term.pattern list> pattern_list
%type <Parsed_term.methods list> record
%type <Term.meth_annotation list> record_ty
%type <unit> s
%type <string> spread
%type <string list> subfield
%type <string list> subfield_lbra
%type <Term.type_annotation> ty
%type <string * Term.track_annotation list> ty_content
%type <Term.track_annotation> ty_content_arg
%type <string * Term.source_annotation> ty_source
%type <Term.source_annotation> ty_source_tracks
%type <Term.type_annotation list> ty_tuple
%type <Parsed_term.list_el list> varlist
%type <string list> subfield_lpar

%%

program:
  | error { raise (Term_base.Parse_error ($loc, "Syntax error!")) }
  | EOF { mk ~pos:$loc `Eof }
  | exprs EOF { $1 }

interactive:
  | error { raise (Term_base.Parse_error ($loc, "Syntax error!")) }
  | exprs SEQSEQ { $1 }
  | EOF { raise End_of_file }

s: | {} | SEQ  {}
g: | {} | GETS {}

exprs:
  | OPEN expr s exprs        { mk ~pos:$loc (`Open ($2,$4)) }
  | expr s                   { $1 }
  | expr s exprs             { mk ~pos:$loc (`Seq ($1,$3)) }
  | binding s                { mk_let ~pos:$loc($1) $1 (mk ~pos:$loc `Eof) }
  | binding s exprs          { mk_let ~pos:$loc($1) $1 $3 }

(* Simple fun body, syntax: { ... }. Same as expressions except initial x = 1
   which conflicts with record declaration: { x = 1 } *)
simple_fun_body:
  | OPEN expr s exprs        { mk ~pos:$loc (`Open ($2,$4)) }
  | expr s                   { $1 }
  | expr s exprs             { mk ~pos:$loc (`Seq ($1,$3)) }
  | explicit_binding s       { mk_let ~pos:$loc($1) $1 (mk ~pos:$loc unit) }
  | explicit_binding s exprs { mk_let ~pos:$loc($1) $1 $3 }

(* General expressions. *)
expr:
  | INCLUDE                          { mk ~pos:$loc (`Include $1) }
  | if_def                           { mk ~pos:$loc (`If_def $1) }
  | if_encoder                       { mk ~pos:$loc (`If_encoder $1) }
  | if_version                       { mk ~pos:$loc (`If_version $1) }
  | LPAR expr COLON ty RPAR          { mk ~pos:$loc (`Cast {cast = $2; typ = $4}) }
  | UMINUS expr                      { mk ~pos:$loc (`Negative $2) }
  | LPAR expr RPAR                   { mk ~pos:$loc (`Parenthesis $2) }
  | INT                              { mk ~pos:$loc (`Int $1) }
  | NOT expr                         { mk ~pos:$loc (`Not $2) }
  | BOOL                             { mk ~pos:$loc (`Bool $1) }
  | FLOAT                            { mk ~pos:$loc (`Float $1) }
  | STRING                           { mk ~pos:$loc (`String $1) }
  | string_interpolation             { mk ~pos:$loc (`String_interpolation $1) }
  | VAR                              { mk ~pos:$loc (`Var $1) }
  | varlist                          { mk ~pos:$loc (`List $1) }
  | GET expr                         { mk ~pos:$loc (`Get $2) }
  | expr SET expr                    { mk ~pos:$loc (`Set ($1, $3)) }
  | ENCODER encoder_opt              { mk_encoder ~pos:$loc $1 $2 }
  | LPAR RPAR                        { mk ~pos:$loc (`Tuple []) }
  | LPAR inner_tuple RPAR            { mk ~pos:$loc (`Tuple $2) }
  | expr DOT LCUR record RCUR        { mk ~pos:$loc (`Methods (Some $1, $4)) }
  | expr DOT LCUR record optional_comma RCUR
                                     { mk ~pos:$loc (`Methods (Some $1, $4)) }
  | LCUR record RCUR                 { mk ~pos:$loc (`Methods (None, $2)) }
  | LCUR record optional_comma RCUR  { mk ~pos:$loc (`Methods (None, $2)) }
  | LCUR RCUR                        { mk ~pos:$loc (`Methods (None, [])) }
  | expr QUESTION_DOT invoke         { mk ~pos:$loc (`Invoke { invoked = $1; meth = $3; optional = true }) }
  | expr DOT invoke                  { mk ~pos:$loc (`Invoke { invoked = $1; meth = $3; optional = false }) }
  | VARLPAR app_list RPAR            { mk ~pos:$loc (`App (mk ~pos:$loc($1) (`Var $1), $2)) }
  | expr COLONCOLON expr             { mk ~pos:$loc (`Append ($1, $3)) }
  | VARLBRA expr RBRA                { mk ~pos:$loc (`Assoc (mk ~pos:$loc($1) (`Var $1), $2)) }
  | expr DOT VARLBRA expr RBRA       { let src = mk ~pos:($startpos($1),$endpos($3)) (`Invoke ({invoked = $1; optional = false; meth = `String $3})) in
                                       mk ~pos:$loc (`Assoc (src, $4)) }
  | BEGIN exprs END                  { mk ~pos:$loc (`Block $2) }
  | FUN LPAR arglist RPAR YIELDS expr{ mk_fun ~pos:$loc $3 $6 }
  | LCUR simple_fun_body RCUR        { mk ~pos:$loc (`Simple_fun $2) }
  | WHILE expr DO exprs END          { mk ~pos:$loc (`While {while_condition = $2; while_loop = $4 }) }
  | FOR optvar GETS expr TO expr DO exprs END
                                     { mk ~pos:$loc (`For { for_variable = $2; for_from = $4; for_to = $6; for_loop = $8 }) }
  | FOR optvar GETS expr DO exprs END
                                     { mk ~pos:$loc (`Iterable_for {
                                         iterable_for_variable = $2;
                                         iterable_for_iterator = $4;
                                         iterable_for_loop = $6
                                       } ) }
  | expr COALESCE expr               { mk ~pos:$loc (`Coalesce ($1, $3)) }
  | TRY exprs FINALLY exprs END          { mk_try ~pos:$loc ~ensure:$4 ~variable:"_" ~body:$2 () }
  | TRY exprs CATCH optvar COLON varlist DO exprs END
                                     { mk_try ~pos:$loc ~handler:$8 ~errors_list:(mk ~pos:$loc($6) (`List $6)) ~variable:$4 ~body:$2 () }
  | TRY exprs CATCH optvar COLON varlist DO exprs FINALLY exprs END
                                     { mk_try ~pos:$loc ~ensure:$10 ~handler:$8 ~errors_list:(mk ~pos:$loc($6) (`List $6)) ~variable:$4 ~body:$2 () }
  | TRY exprs CATCH optvar DO exprs END
                                     { mk_try ~pos:$loc ~handler:$6 ~variable:$4 ~body:$2 () }
  | TRY exprs CATCH optvar DO exprs FINALLY exprs END
                                     { mk_try ~pos:$loc ~ensure:$8 ~handler:$6 ~variable:$4 ~body:$2 () }
  | IF exprs THEN exprs if_elsif END { mk ~pos:$loc (`If {if_condition = $2; if_then = $4; if_elsif = fst $5; if_else = snd $5 }) }
  | REGEXP                           {  mk ~pos:$loc (`Regexp $1) }
  | expr QUESTION expr COLON expr    { mk ~pos:$loc (`Inline_if {if_condition = $1; if_then = $3; if_elsif = []; if_else = Some $5}) }
  | expr AND expr                  { match $1.term, $3.term with
                                       | `BoolOp ("and", l), `BoolOp ("and", l') -> mk ~pos:$loc (`BoolOp ("and", l@l'))
                                       |  `BoolOp ("and", l), _ -> mk ~pos:$loc (`BoolOp ("and", l@[$3]))
                                       |  _, `BoolOp ("and", l) -> mk ~pos:$loc (`BoolOp ("and", $1::l))
                                       | _ -> mk ~pos:$loc (`BoolOp ("and", [$1; $3])) }
  | expr OR expr                  { match $1.term, $3.term with
                                       | `BoolOp ("or", l), `BoolOp ("or", l') -> mk ~pos:$loc (`BoolOp ("or", l@l'))
                                       |  `BoolOp ("or", l), _ -> mk ~pos:$loc (`BoolOp ("or", l@[$3]))
                                       |  _, `BoolOp ("or", l) -> mk ~pos:$loc (`BoolOp ("or", $1::l))
                                       | _ -> mk ~pos:$loc (`BoolOp ("or", [$1; $3])) }
  | expr BIN1 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr BIN2 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr BIN3 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr TIMES expr                { mk ~pos:$loc (`Infix ($1, "*", $3)) }
  | expr MINUS expr                { mk ~pos:$loc (`Infix ($1, "-", $3)) }
  | expr AT expr                   { mk ~pos:$loc (`At ($1, $3)) }
  | time_predicate                 { $1 }

invoke:
  | VAR                   { `String $1 }
  | VARLPAR app_list RPAR { `App ($1, $2) }

time_predicate:
  | INTERVAL { mk ~pos:$loc (`Time_interval $1) }
  | TIME     { mk ~pos:$loc (`Time $1) }

ty:
  | UNDERSCORE                   { `Named "_" }
  | VAR                          { `Named $1 }
  | ty QUESTION                  { `Nullable $1 }
  | LBRA ty RBRA                 { `List $2 }
  | LBRA ty RBRA VAR VAR DOT VAR { mk_json_assoc_object_ty ~pos:$loc ($2,$4,$5,$7) }
  | LPAR ty_tuple RPAR           { `Tuple $2 }
  | LPAR argsty RPAR YIELDS ty   { `Arrow ($2,$5) }
  | LCUR record_ty RCUR          { `Record $2 }
  | ty DOT VAR                   { `Invoke ($1, $3) }
  | ty QUESTION_DOT LCUR record_ty RCUR
                                 { `Method (`Nullable $1, $4) }
  | ty DOT LCUR record_ty RCUR   { `Method ($1, $4) }
  | ty_source                    { `Source $1 }

record_ty:
  |                         { [] }
  | meth_ty                 { [$1] }
  | meth_ty COMMA record_ty { $1::$3 }

meth_ty:
  | VAR COLON ty            { { optional = false; name = $1; typ = $3; json_name = None } }
  | VAR QUESTION COLON ty   { { optional = true; name = $1; typ = $4; json_name = None } }
  | STRING VAR VAR COLON ty {
       match $2 with
         |"as" ->             { optional = false; name = $3; typ = $5; json_name = Some (render_string ~pos:$loc $1) }
         | _ -> raise (Term_base.Parse_error ($loc, "Invalid type constructor")) }
  | STRING VAR VAR QUESTION COLON ty {
       match $2 with
         |"as" ->             { optional = true; name = $3; typ = $6; json_name = Some (render_string ~pos:$loc $1) }
         | _ -> raise (Term_base.Parse_error ($loc, "Invalid type constructor")) }

ty_source:
  | VARLPAR RPAR                  { $1, { extensible = false; tracks = [] } }
  | VARLPAR ty_source_tracks RPAR { $1, $2 }

ty_source_tracks:
  | VAR GETS ty_content { { extensible = false; tracks = [{track_name = $1; track_type = fst $3; track_params = snd $3}] } }
  | DOTDOTDOT { { extensible = true; tracks = [] } }
  | VAR GETS ty_content COMMA ty_source_tracks { { $5 with tracks = { track_name = $1; track_type = fst $3; track_params = snd $3}::$5.tracks } }

ty_content:
  | VAR                           { $1, [] }
  | VAR DOT VAR                   { $1 ^ "." ^ $3, [] }
  | VAR DOT VAR DOT VAR           { $1 ^ "." ^ $3 ^ "." ^ $5, [] }
  | VARLPAR ty_content_args RPAR  { $1, $2 }
  | VAR DOT VARLPAR ty_content_args RPAR
                                  { $1 ^ "." ^ $3, $4 }
  | VAR DOT VAR DOT VARLPAR ty_content_args RPAR
                                  { $1 ^ "." ^ $3 ^ "." ^ $5, $6 }


ty_content_args:
  |                                      { [] }
  | ty_content_arg                       { [$1] }
  | ty_content_arg COMMA ty_content_args { $1::$3 }

ty_content_arg:
  | VAR                  { ("", `Verbatim $1) }
  | INT                  { ("", `Verbatim $1) }
  | FLOAT                { ("", `Verbatim $1) }
  | STRING               { ("", `String ($loc($1), $1)) }
  | VAR GETS VAR         { ($1, `Verbatim $3) }
  | VAR GETS STRING      { ($1, `String ($loc($3), $3)) }
  | VAR GETS INT         { ($1, `Verbatim $3) }
  | VAR GETS FLOAT       { ($1, `Verbatim $3) }

ty_tuple:
  | ty TIMES ty { [$1; $3] }
  | ty TIMES ty_tuple { $1::$3 }

argty:
  | ty                    { false,"",$1 }
  | VAR COLON ty          { false,$1,$3 }
  | QUESTION VAR COLON ty { true,$2,$4 }

argsty:
  |                    { [] }
  | argty              { [$1] }
  | argty COMMA argsty { $1::$3 }

varlist:
  | LBRA inner_list RBRA { $2 }

inner_list:
  | inner_list_item COMMA inner_list
                          { $1::$3 }
  | inner_list_item       { [$1] }
  |                       { [] }

inner_list_item:
  | DOTDOTDOT expr { `Ellipsis $2 }
  | expr           { `Term $1 }

inner_tuple:
  | expr COMMA expr { [$1;$3] }
  | expr COMMA inner_tuple { $1::$3 }

app_list_elem:
  | VAR GETS expr { `Term ($1,$3) }
  | expr          { `Term ("",$1) }
  | ARGS_OF LPAR VAR RPAR        { `Argsof {only = []; except = []; source = $3 } }
  | ARGS_OF LPAR subfield RPAR
                                 { `Argsof {only = []; except = []; source = String.concat "." $3 } }
  | ARGS_OF LPAR VARLBRA args_of_params RBRA RPAR {
                                   `Argsof {only = fst $4; except = snd $4; source = $3 }
                                 }
  | ARGS_OF LPAR subfield_lbra args_of_params RBRA RPAR
                                 { `Argsof {only = fst $4; except = snd $4; source = String.concat "." $3} }

app_list:
  |                              { [] }
  | app_list_elem                { [$1] }
  | app_list_elem COMMA app_list { $1::$3 }

optvar:
  | VAR        { $1 }
  | UNDERSCORE { "_" }

pattern_list:
  |                            { [] }
  | pattern                    { [$1] }
  | pattern_list COMMA pattern { $1@[$3] }

spread:
  | DOTDOTDOT        { "_" }
  | DOTDOTDOT optvar { $2 }

pattern_list_with_spread:
  | spread                                       { [], Some ($loc, $1),    [] }
  | pattern_list                                 { $1, None,               [] }
  | spread COMMA pattern_list                    { [], Some ($loc($1), $1), $3 }
  | pattern_list COMMA spread                    { $1, Some ($loc($3), $3), [] }
  | pattern_list COMMA spread COMMA pattern_list { $1, Some ($loc($3), $3), $5 }

tuple_pattern:
  | LPAR pattern_list RPAR             { `PTuple $2 }

list_pattern:
  | LBRA pattern_list_with_spread RBRA { `PList $2 }

meth_pattern_el:
  | VAR              { $1, `None }
  | VAR QUESTION     { $1, `Nullable }
  | VAR GETS pattern { $1, `Pattern $3 }

meth_pattern_list:
  |                                         { [] }
  | meth_pattern_el                         { [$1] }
  | meth_pattern_el COMMA meth_pattern_list { $1::$3 }

record_pattern:
  | LCUR meth_pattern_list RCUR { $2 }

meth_spread_list:
  | DOTDOTDOT                              { Some ({ pat_pos = $loc; pat_entry = `PVar ["_"] }), [] }
  | DOTDOTDOT optvar                       { Some ({ pat_pos = $loc($2); pat_entry = `PVar [$2] }), [] }
  | meth_pattern_el COMMA meth_spread_list { fst $3, $1::(snd $3) }

record_spread_pattern:
  | LCUR meth_spread_list RCUR { $2 }

meth_pattern:
  | record_spread_pattern            { `PMeth $1             }
  | record_pattern                   { `PMeth (None,     $1) }
  | VAR DOT record_pattern           { let pat = { pat_pos = $loc($1); pat_entry = `PVar [$1] } in
                                       `PMeth (Some pat, $3) }
  | UNDERSCORE DOT record_pattern    { let pat = { pat_pos = $loc; pat_entry = `PVar ["_"] } in
                                       `PMeth (Some pat, $3) }
  | tuple_pattern DOT record_pattern { let pat = { pat_pos = $loc($1); pat_entry = $1 } in
                                       `PMeth (Some pat,  $3) }
  | list_pattern DOT record_pattern  { let pat = { pat_pos = $loc($1); pat_entry = $1 } in
                                       `PMeth (Some pat,  $3) }

var_pattern:
  | optvar { `PVar [$1] }

pattern:
  | var_pattern   { { pat_pos = $loc; pat_entry = $1 } }
  | tuple_pattern { { pat_pos = $loc; pat_entry = $1 } }
  | list_pattern  { { pat_pos = $loc; pat_entry = $1 } }
  | meth_pattern  { { pat_pos = $loc; pat_entry = $1 } }

subfield:
  | VAR DOT in_subfield { $1::$3 }

in_subfield:
  | VAR                 { [$1] }
  | VAR DOT in_subfield { $1::$3 }

let_opt_el:
  | VAR           { $1, mk ~pos:$loc (`Var $1) }
  | VAR GETS expr { $1, $3 }

let_opt:
  | let_opt_el               { [$1] }
  | let_opt_el COMMA let_opt { $1::$3 }

_let:
  | LET { Parser_helper.let_decoration_of_lexer_let_decoration $1 }
  | LETLBRA let_opt RBRA {
      match $1 with
        | `Json_parse     -> `Json_parse (Parser_helper.args_of_json_parse ~pos:$loc $2)
        | _ -> raise (Term_base.Parse_error ($loc, "Invalid let constructor")) }

def:
  | DEF { Parser_helper.let_decoration_of_lexer_let_decoration $1 }

explicit_binding:
  | _let pattern GETS expr   { `Let Parser_helper.(let_args ~decoration:$1 ~pat:$2 ~def:$4 ()) }
  | _let LPAR pattern COLON ty RPAR GETS expr
                             { `Let Parser_helper.(let_args ~decoration:$1 ~pat:$3 ~def:$8 ~cast:$5 ()) }
  | _let subfield GETS expr  { `Let Parser_helper.(let_args ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~def:$4 ()) }
  | def optvar g exprs END   { `Def Parser_helper.(let_args ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar [$2] }) ~def:$4 ()) }
  | def LPAR optvar COLON ty RPAR g exprs END
                             { `Def Parser_helper.(let_args ~decoration:$1 ~pat:({ pat_pos = $loc($3); pat_entry =`PVar [$3] }) ~def:$8 ~cast:$5 ()) }
  | def subfield g exprs END { `Def Parser_helper.(let_args ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~def:$4 ()) }
  | def subfield_lpar arglist RPAR g exprs END
                             { `Def Parser_helper.(let_args ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~arglist:$3 ~def:$6 ()) }

binding:
  | optvar GETS expr         { `Binding Parser_helper.(let_args ~decoration:`None ~pat:({ pat_pos = $loc($1); pat_entry = `PVar [$1] }) ~def:$3 ()) }
  | explicit_binding         { ($1 :> binding) }

subfield_lpar:
  | VARLPAR               { [$1] }
  | VAR DOT subfield_lpar { $1::$3 }

arglist:
  |                       { [] }
  | arg                   { [$1] }
  | arg COMMA arglist     { $1::$3 }
arg:
  | TILD VAR opt { `Term {label = $2; as_variable = None; typ = None; default = $3} }
  | TILD LPAR VAR COLON ty RPAR opt {
                   `Term {label = $3; as_variable = None; typ =  Some $5; default = $7}
                 }
  | TILD VAR GETS UNDERSCORE opt {
                   `Term {label = $2; as_variable = Some "_"; typ = None; default = $5}
                 }
  | optvar opt   { `Term {label = ""; as_variable = Some $1; typ = None; default = $2} }
  | LPAR optvar COLON ty RPAR opt {
                   `Term {label = ""; as_variable =  Some $2; typ = Some $4; default =  $6}
                 }
  | ARGS_OF LPAR VAR RPAR {
                   `Argsof {only = []; except = []; source = $3 }
                 }
  | ARGS_OF LPAR subfield RPAR {
                   `Argsof {only = []; except = []; source = String.concat "." $3 }
                 }
  | ARGS_OF LPAR VARLBRA args_of_params RBRA RPAR {
                   `Argsof {only = fst $4; except = snd $4; source = $3 }
                }
  | ARGS_OF LPAR subfield_lbra args_of_params RBRA RPAR {
                   `Argsof {only = fst $4; except = snd $4; source = String.concat "." $3 }
                }

opt:
  | GETS expr { Some $2 }
  |           { None }
args_of_params:
  | VAR                          { [$1], [] }
  | GET VAR                      { [], [$2] }
  | VAR COMMA args_of_params     { $1::(fst $3), (snd $3) }
  | GET VAR COMMA args_of_params { (fst $4), $2::(snd $4) }

subfield_lbra:
  | VAR DOT in_subfield_lbra { $1::$3 }
in_subfield_lbra:
  | VARLBRA { [$1] }
  | VAR DOT in_subfield_lbra { $1::$3 }

if_elsif:
  | ELSIF exprs THEN exprs if_elsif { ($2, $4)::(fst $5), snd $5 }
  | ELSE exprs                      { [], Some $2 }
  |                                 { [], None }

encoder_opt:
  | %prec no_app { [] }
  | LPAR encoder_params RPAR { $2 }

encoder_param:
  | VAR GETS expr       { `Labelled (`Verbatim $1, $3) }
  | STRING GETS expr    { `Labelled (`String ($loc($1), $1), $3) }
  | VAR                 { `Anonymous (`Verbatim $1) }
  | STRING              { `Anonymous (`String (($loc($1), $1))) }
  | ENCODER encoder_opt { `Encoder ($1, $2) }

encoder_params:
  |                                    { [] }
  | encoder_param                      { [$1] }
  | encoder_param COMMA encoder_params { $1::$3 }

plain_encoder_params:
  | LPAR encoder_params RPAR { $2 }

optional_comma:
  | COMMA {}

record:
  | VAR GETS expr  { [`Method ($1, $3)] }
  | DOTDOTDOT expr { [`Ellipsis $2] }
  | record COMMA VAR GETS expr  { $1@[`Method ($3,$5)] }
  | record COMMA DOTDOTDOT expr { $1@[`Ellipsis $4] }

string_interpolation:
  | BEGIN_INTERPOLATION string_interpolation_elems END_INTERPOLATION { $1, $2 }

string_interpolation_elem:
  | INTERPOLATED_STRING  { `String $1 }
  | expr                 { `Term $1 }

string_interpolation_elems:
  | string_interpolation_elem { [$1] }
  | string_interpolation_elem string_interpolation_elems
                              { $1::$2 }

if_def_var:
  | VAR                { [$1] }
  | VAR DOT if_def_var { $1::$3 }

if_def:
  | PP_IFDEF if_def_var exprs PP_ENDIF { {
      if_def_negative = $1;
      if_def_condition = String.concat "." $2;
      if_def_then = $3;
      if_def_else = None
   } }
  | PP_IFDEF if_def_var exprs PP_ELSE exprs PP_ENDIF { {
      if_def_negative = $1;
      if_def_condition = String.concat "." $2;
      if_def_then = $3;
      if_def_else = Some $5;
  } }

if_encoder:
  | PP_IFENCODER ENCODER exprs PP_ENDIF { {
      if_encoder_negative = $1;
      if_encoder_condition = $2;
      if_encoder_then = $3;
      if_encoder_else = None
   } }
  | PP_IFENCODER ENCODER exprs PP_ELSE exprs PP_ENDIF { {
      if_encoder_negative = $1;
      if_encoder_condition = $2;
      if_encoder_then = $3;
      if_encoder_else = Some $5;
  } }

if_version_op:
  | BIN1 {
      match $1 with
        | "==" -> `Eq
        | ">=" -> `Geq
        | "<=" -> `Leq
        | "<" -> `Lt
        | ">" -> `Gt
        | _ -> raise (Term_base.Parse_error ($loc, "invalid %ifversion operand"))
  }

if_version_version:
  | VERSION  { $1 }
  | INT      { Lang_string.Version.of_string $1 }
  | FLOAT    { Lang_string.Version.of_string $1 }

if_version:
  | PP_IFVERSION if_version_op if_version_version exprs PP_ENDIF { {
      if_version_op = $2;
      if_version_version = $3;
      if_version_then = $4;
      if_version_else = None
   } }
  | PP_IFVERSION if_version_op if_version_version exprs PP_ELSE exprs PP_ENDIF { {
      if_version_op = $2;
      if_version_version = $3;
      if_version_then = $4;
      if_version_else = Some $6;
  } }

annotate:
  | annotate_metadata COLON { $1 }

annotate_metadata:
  | annotate_metadata_entry annotate_metadata { $1::$2 }
  | annotate_key GETS annotate_value { [$1, $3] }

annotate_metadata_entry:
  | annotate_key GETS annotate_value COMMA { $1, $3 }

annotate_key:
  | VAR { $1 }
  | STRING { render_string ~pos:$loc $1 }

annotate_value:
  | INT { $1 }
  | FLOAT { $1 }
  | BOOL { string_of_bool $1 }
  | VAR { $1 }
  | STRING { render_string ~pos:$loc $1 }
