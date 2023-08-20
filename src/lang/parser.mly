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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

%{
open Parsed_term
open Parsed_term.Ground
(* All auxiliary functions for parser are there *)
open Parser_helper
%}

%token <string> VAR
%token <string> VARLPAR
%token <string> VARLBRA
%token <Lang_string.Version.t> VERSION
%token <char * string * Pos.t> PP_STRING
%token <string * char list * Pos.t> PP_REGEXP
%token <char * string > STRING
%token <string * char list > REGEXP
%token <int> INT PP_INT_DOT_LCUR
%token <string * string> FLOAT
%token <bool> BOOL
%token <Parsed_term.time_el> TIME
%token <Parsed_term.time_el * Parsed_term.time_el> INTERVAL
%token <string> ENCODER
%token EOF
%token <Parser_helper.lexer_let_decoration> LET
%token <Parser_helper.lexer_let_decoration> LETLBRA
%token BEGIN END GETS TILD QUESTION
(* name, arguments, methods *)
%token <Parser_helper.lexer_let_decoration> DEF
%token REPLACES
%token COALESCE
%token TRY CATCH DO
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
%nonassoc COALESCE     (* (x ?? y) == z *)
%right SET             (* expr := (expr + expr), expr := (expr := expr) *)
%nonassoc QUESTION     (* x ? y : z *)
%left AND             (* ((x+(y*z))==3) or ((not a)==b) *)
%left OR
%left BIN1
%nonassoc NOT
%left BIN2 MINUS
%left BIN3 TIMES
%right COLONCOLON
%nonassoc GET          (* (!x)+2 *)
%left DOT
%nonassoc COLON

(* Read %ogg(...) as one block, shifting LPAR rather than reducing %ogg *)
%nonassoc no_app
%nonassoc LPAR

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
%type <Parser_helper.encoder_param list> plain_encoder_params

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
%type <Parser_helper.encoder_opt> encoder_opt
%type <Parser_helper.encoder_param> encoder_param
%type <Parser_helper.encoder_param list> encoder_params
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
%type <Term.pattern> list_pattern
%type <Term.pattern> meth_pattern
%type <Parser_helper.meth_pattern_el> meth_pattern_el
%type <Parser_helper.meth_pattern_el list> meth_pattern_list
%type <Term.meth_annotation> meth_ty
%type <Term.t option> opt
%type <string> optvar
%type <Term.pattern> pattern
%type <Term.pattern list> pattern_list
%type <Term.pattern list * string option * Term.pattern list> pattern_list_with_spread
%type <Parser_helper.record> record
%type <Parser_helper.meth_pattern_el list> record_pattern
%type <Term.meth_annotation list> record_ty
%type <unit> s
%type <string> spread
%type <string list> subfield
%type <string list> subfield_lbra
%type <Term.pattern> tuple_pattern
%type <Term.type_annotation> ty
%type <Parser_helper.ty_content> ty_content
%type <Parser_helper.ty_content_arg> ty_content_arg
%type <Parser_helper.ty_content_args> ty_content_args
%type <string * Term.source_annotation> ty_source
%type <Term.source_annotation> ty_source_tracks
%type <Term.type_annotation list> ty_tuple
%type <Term.pattern> var_pattern
%type <Parsed_term.list_el list> varlist
%type <string list> varlpar

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
   which conflcits with record declaration: { x = 1 } *)
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
  | LPAR expr COLON ty RPAR          { mk ~pos:$loc (`Cast ($2, $4)) }
  | UMINUS FLOAT                     { mk ~pos:$loc (`Float ("-" ^ (if fst $2 = "" then "0" else fst $2), snd $2)) }
  | UMINUS INT                       { mk ~pos:$loc (`Ground (Int (- $2))) }
  | UMINUS LPAR expr RPAR            { mk ~pos:$loc (`Negative $3) }
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk ~pos:$loc (`Ground (Int $1)) }
  | NOT expr                         { mk ~pos:$loc (`Not $2) }
  | BOOL                             { mk ~pos:$loc (`Ground (Bool $1)) }
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
  | expr DOT LCUR record optional_comma RCUR
                                     { $4 ~pos:$loc $1 }
  | LCUR DOTDOTDOT expr RCUR         { $3 }
  | LCUR record COMMA DOTDOTDOT expr RCUR
                                     { $2 ~pos:$loc $5 }
  | LCUR record optional_comma RCUR  { $2 ~pos:$loc (mk ~pos:$loc (`Tuple [])) }
  | LCUR RCUR                        { mk ~pos:$loc (`Tuple []) }
  | expr QUESTION DOT invoke         { mk ~pos:$loc (`Invoke { invoked = $1; meth = $4; optional = true }) }
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
  | TRY exprs CATCH optvar COLON varlist DO exprs END
                                     { mk~pos:$loc (`Try {
                                         Term.try_body = $2;
                                         try_variable = $4;
                                         try_errors_list = mk ~pos:$loc($6) (`List $6);
                                         try_handler = $8 }) }
  | TRY exprs CATCH optvar DO exprs END { mk~pos:$loc (`Try {
                                         Term.try_body = $2;
                                         try_variable = $4;
                                         try_errors_list =  mk ~pos:$loc `Null;
                                         try_handler = $6 }) }
  | IF exprs THEN exprs if_elsif END { mk ~pos:$loc (`If {if_condition = $2; if_then = $4; if_elsif = fst $5; if_else = snd $5 }) }
  | REGEXP                           {  mk ~pos:$loc (`Regexp $1) }
  | expr QUESTION expr COLON expr    { mk ~pos:$loc (`Inline_if {if_condition = $1; if_then = $3; if_elsif = []; if_else = Some $5}) }
  | expr AND expr                  { match $1.term, $3.term with
                                       | `Bool ("and", l), `Bool ("and", l') -> mk ~pos:$loc (`Bool ("and", l@l'))
                                       |  `Bool ("and", l), _ -> mk ~pos:$loc (`Bool ("and", l@[$3]))
                                       |  _, `Bool ("and", l) -> mk ~pos:$loc (`Bool ("and", $1::l))
                                       | _ -> mk ~pos:$loc (`Bool ("and", [$1; $3])) }
  | expr OR expr                  { match $1.term, $3.term with
                                       | `Bool ("or", l), `Bool ("or", l') -> mk ~pos:$loc (`Bool ("or", l@l'))
                                       |  `Bool ("or", l), _ -> mk ~pos:$loc (`Bool ("or", l@[$3]))
                                       |  _, `Bool ("or", l) -> mk ~pos:$loc (`Bool ("or", $1::l))
                                       | _ -> mk ~pos:$loc (`Bool ("or", [$1; $3])) }
  | expr BIN1 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr BIN2 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr BIN3 expr                 { mk ~pos:$loc (`Infix ($1, $2, $3)) }
  | expr TIMES expr                { mk ~pos:$loc (`Infix ($1, "*", $3)) }
  | expr MINUS expr                { mk ~pos:$loc (`Infix ($1, "-", $3)) }
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
  | VAR                  { "",$1 }
  | STRING               { "", render_string ~pos:$loc $1 }
  | VAR GETS VAR         { $1,$3 }
  | VAR GETS STRING      { $1, render_string ~pos:$loc $3}
  | VAR GETS INT         { $1,string_of_int $3}

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
  | spread                                       { [], Some $1, [] }
  | pattern_list                                 { [], None,    $1 }
  | spread COMMA pattern_list                    { [], Some $1, $3 }
  | pattern_list COMMA spread                    { $1, Some $3, [] }
  | pattern_list COMMA spread COMMA pattern_list { $1, Some $3, $5 }

tuple_pattern:
  | LPAR pattern_list RPAR             { `PTuple $2 }

list_pattern:
  | LBRA pattern_list_with_spread RBRA { `PList $2 }

meth_pattern_el:
  | VAR              { $1, None }
  | VAR GETS pattern { $1, Some $3 }

meth_pattern_list:
  |                                         { [] }
  | meth_pattern_el                         { [$1] }
  | meth_pattern_el COMMA meth_pattern_list { $1::$3 }

record_pattern:
  | LCUR meth_pattern_list RCUR { $2 }

meth_spread_list:
  | DOTDOTDOT                              { Some (`PVar ["_"]), [] }
  | DOTDOTDOT optvar                       { Some (`PVar [$2]), [] }
  | meth_pattern_el COMMA meth_spread_list { fst $3, $1::(snd $3) }

record_spread_pattern:
  | LCUR meth_spread_list RCUR { $2 }

meth_pattern:
  | record_spread_pattern            { `PMeth $1                      }
  | record_pattern                   { `PMeth (None,              $1) }
  | VAR DOT record_pattern           { `PMeth (Some (`PVar [$1]),  $3) }
  | UNDERSCORE DOT record_pattern    { `PMeth (Some (`PVar ["_"]), $3) }
  | tuple_pattern DOT record_pattern { `PMeth (Some $1,           $3) }
  | list_pattern DOT record_pattern  { `PMeth (Some $1,           $3) }

var_pattern:
  | optvar { `PVar [$1] }

pattern:
  | var_pattern   { $1 }
  | tuple_pattern { $1 }
  | list_pattern  { $1 }
  | meth_pattern  { $1 }

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
  | _let subfield GETS expr  { `Let Parser_helper.(let_args ~decoration:$1 ~pat:(`PVar $2) ~def:$4 ()) }
  | def pattern g exprs END  { `Def Parser_helper.(let_args ~decoration:$1 ~pat:$2 ~def:$4 ()) }
  | def LPAR pattern COLON ty RPAR g exprs END
                             { `Def Parser_helper.(let_args ~decoration:$1 ~pat:$3 ~def:$8 ~cast:$5 ()) }
  | def subfield g exprs END { `Def Parser_helper.(let_args ~decoration:$1 ~pat:(`PVar $2) ~def:$4 ()) }
  | def varlpar arglist RPAR g exprs END
                             { `Def Parser_helper.(let_args ~decoration:$1 ~pat:(`PVar $2) ~arglist:$3 ~def:$6 ()) }

binding:
  | optvar GETS expr         { `Binding Parser_helper.(let_args ~decoration:`None ~pat:(`PVar [$1]) ~def:$3 ()) }
  | explicit_binding         { ($1 :> binding) }

varlpar:
  | VARLPAR         { [$1] }
  | VAR DOT varlpar { $1::$3 }

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
  | VAR GETS expr       { $1, `Term $3 }
  | STRING GETS expr    { render_string ~pos:$loc $1, `Term $3 }
  | VAR                 { "", `Term (mk ~pos:$loc (`Ground (String $1))) }
  | STRING              { "", `Term (mk ~pos:$loc (`String $1)) }
  | ENCODER encoder_opt { "", `Encoder ($1, $2) }

encoder_params:
  |                                    { [] }
  | encoder_param                      { [$1] }
  | encoder_param COMMA encoder_params { $1::$3 }

plain_encoder_params:
  | LPAR encoder_params RPAR { $2 }

optional_comma:
  |       {}
  | COMMA {}

record:
  | VAR GETS expr {
      fun ~pos:_ e -> mk ~pos:$loc ~t:e.Term.t ~methods:(Methods.add $1 $3 e.methods) e.Term.term
  }
  | record COMMA VAR GETS expr {
      fun ~pos e ->
        let tm = $1 ~pos e in
        mk ~pos:$loc ~t:tm.Term.t ~methods:(Methods.add $3 $5 tm.methods) tm.Term.term
  }

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
  | INT      { Lang_string.Version.of_string (string_of_int $1) }
  | FLOAT    { Lang_string.Version.of_string (fst $1 ^ "." ^ snd $1) }

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
  | INT { string_of_int $1 }
  | FLOAT { fst $1 ^ "." ^ snd $1 }
  | BOOL { string_of_bool $1 }
  | VAR { $1 }
  | STRING { render_string ~pos:$loc $1 }
