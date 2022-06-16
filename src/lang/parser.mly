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

%{
open Term
open Term.Ground
(* All auxiliary functions for parser are there *)
open Parser_helper
%}

%token <string> VAR
%token <string> VARLPAR
%token <string> VARLBRA
%token <string * Pos.t> PP_STRING
%token <string * char list * Pos.t> PP_REGEXP
%token <string > STRING
%token <string * char list > REGEXP
%token <int> INT PP_INT_DOT_LCUR
%token <float> FLOAT
%token <bool> BOOL
%token <int option list> TIME
%token <int option list * int option list> INTERVAL
%token <string> ENCODER
%token EOF
%token <Parser_helper.lexer_let_decoration> LET
%token <Parser_helper.lexer_let_decoration> LETLBRA
%token BEGIN END GETS TILD QUESTION
(* name, arguments, methods *)
%token <(Doc.item * (string*string) list * (string*string) list)*Parser_helper.let_decoration> DEF
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
%token <string> BINB
%token <string> BIN1
%token <string> BIN2
%token <string> BIN3
%token TIMES
%token MINUS UMINUS
%token UNDERSCORE
%token NOT
%token GET SET
%token <string> PP_IFDEF PP_IFNDEF
%token <[ `Eq | `Geq | `Leq | `Gt | `Lt] * string> PP_IFVERSION
%token ARGS_OF
%token PP_IFENCODER PP_IFNENCODER PP_ELSE PP_ENDIF
%token <Parser_helper.lexer_let_decoration> PP_DEF
%token PP_ENDL PP_DEFINE
%token <string> PP_INCLUDE
%token <string list> PP_COMMENT
%token WHILE FOR TO

%nonassoc YIELDS       (* fun x -> (x+x) *)
%nonassoc COALESCE     (* (x ?? y) == z *)
%right SET             (* expr := (expr + expr), expr := (expr := expr) *)
%nonassoc TO
%nonassoc QUESTION    (* x ? y : z *)
%left BINB             (* ((x+(y*z))==3) or ((not a)==b) *)
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

%type <Parser_helper.let_decoration> _let
%type <string> annotate_key
%type <(string * string) list> annotate_metadata
%type <string> annotate_value
%type <Parser_helper.app_list_elem> app_list
%type <Parser_helper.app_list_elem> app_list_elem
%type <Parser_helper.arglist> arg
%type <Parser_helper.arglist> arglist
%type <string list * string list> args_of_params
%type <(bool * string * Type.t) list> argsty
%type <bool * string * Type.t> argty
%type <Parser_helper.binding> binding
%type <Parser_helper.encoder_opt> encoder_opt
%type <Parser_helper.encoder_param> encoder_param
%type <Parser_helper.encoder_param list> encoder_params
%type <Term.t> expr
%type <Term.t> exprs
%type <Term.t> exprss
%type <unit> g
%type <Term.t> if_elsif
%type <string list> in_subfield
%type <string list> in_subfield_lbra
%type <Parser_helper.inner_list> inner_list
%type <Parser_helper.inner_list_item> inner_list_item
%type <Term.t list> inner_tuple
%type <Parser_helper.let_opt_el list> let_opt
%type <Parser_helper.let_opt_el> let_opt_el
%type <Term.pattern> list_pattern
%type <Term.pattern> meth_pattern
%type <Parser_helper.meth_pattern_el> meth_pattern_el
%type <Parser_helper.meth_pattern_el list> meth_pattern_list
%type <string * Type.t * string option> meth_ty
%type <Term.t option> opt
%type <string> optvar
%type <Term.pattern> pattern
%type <Term.pattern list> pattern_list
%type <Term.pattern list * string option * Term.pattern list> pattern_list_with_spread
%type <Parser_helper.record> record
%type <Parser_helper.meth_pattern_el list> record_pattern
%type <Type.t> record_ty
%type <unit> s
%type <string> spread
%type <string list> subfield
%type <string list> subfield_lbra
%type <Term.pattern> tuple_pattern
%type <Type.t> ty
%type <Parser_helper.ty_content> ty_content
%type <Parser_helper.ty_content_arg> ty_content_arg
%type <Parser_helper.ty_content_args> ty_content_args
%type <Type.t> ty_source
%type <(string * Parser_helper.ty_content) list> ty_source_params
%type <Type.t list> ty_tuple
%type <Term.pattern> var_pattern
%type <Parser_helper.varlist> varlist
%type <string list> varlpar

%%

program:
  | error { raise (Parse_error ($loc, "Syntax error!")) }
  | EOF { mk ~pos:$loc unit }
  | exprs EOF { $1 }
interactive:
  | error { raise (Parse_error ($loc, "Syntax error!")) }
  | exprs SEQSEQ { $1 }
  | EOF { raise End_of_file }

s: | {} | SEQ  {}
g: | {} | GETS {}

exprs:
  | OPEN expr s exprs        { mk ~pos:$loc (Open ($2,$4)) }
  | expr s                   { $1 }
  | expr s exprs             { mk ~pos:$loc (Seq ($1,$3)) }
  | binding s                { mk_let ~pos:$loc($1) $1 (mk ~pos:$loc unit) }
  | binding s exprs          { mk_let ~pos:$loc($1) $1 $3 }

(* Sequences of expressions without bindings *)
exprss:
  | expr { $1 }
  | expr SEQ exprss { mk ~pos:$loc (Seq ($1,$3)) }

(* General expressions. *)
expr:
  | LPAR expr COLON ty RPAR          { mk ~pos:$loc (Cast ($2, $4)) }
  | UMINUS FLOAT                     { mk ~pos:$loc (Ground (Float (-. $2))) }
  | UMINUS INT                       { mk ~pos:$loc (Ground (Int (- $2))) }
  | UMINUS LPAR expr RPAR            { mk ~pos:$loc (App (mk ~pos:$loc($1) (Var "~-"), ["", $3])) }
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk ~pos:$loc (Ground (Int $1)) }
  | NOT expr                         { mk ~pos:$loc (App (mk ~pos:$loc($1) (Var "not"), ["", $2])) }
  | BOOL                             { mk ~pos:$loc (Ground (Bool $1)) }
  | FLOAT                            { mk ~pos:$loc (Ground (Float  $1)) }
  | STRING                           { mk ~pos:$loc (Ground (String $1)) }
  | VAR                              { mk ~pos:$loc (Var $1) }
  | varlist                          { mk_list ~pos:$loc $1 }
  | GET expr                         { mk ~pos:$loc (App (mk ~pos:$loc($1) (Invoke (mk ~pos:$loc($1) (Var "ref"), "get")), ["", $2])) }
  | expr SET expr                    { mk ~pos:$loc (App (mk ~pos:$loc($2) (Invoke (mk ~pos:$loc($1) (Var "ref"), "set")), ["", $1; "", $3])) }
  | ENCODER encoder_opt              { mk_encoder ~pos:$loc $1 $2 }
  | LPAR RPAR                        { mk ~pos:$loc (Tuple []) }
  | LPAR inner_tuple RPAR            { mk ~pos:$loc (Tuple $2) }
  | expr DOT LCUR record RCUR        { $4 ~pos:$loc $1 }
  | LCUR record RCUR                 { $2 ~pos:$loc (mk ~pos:$loc (Tuple [])) }
  | LCUR RCUR                        { mk ~pos:$loc (Tuple []) }
  | expr DOT VAR                     { mk ~pos:$loc (Invoke ($1, $3)) }
  | expr DOT VARLPAR app_list RPAR   { mk ~pos:$loc (App (mk ~pos:($startpos($1),$endpos($3)) (Invoke ($1, $3)), $4)) }
  | VARLPAR app_list RPAR            { mk ~pos:$loc (App (mk ~pos:$loc($1) (Var $1), $2)) }
  | expr COLONCOLON expr             { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var "_::_"), ["", $1; "", $3])) }
  | VARLBRA expr RBRA                { mk ~pos:$loc (App (mk ~pos:$loc (Var "_[_]"), ["", mk ~pos:$loc($1) (Var $1); "", $2])) }
  | expr DOT VARLBRA expr RBRA       { mk ~pos:$loc (App (mk ~pos:$loc (Var "_[_]"), ["", mk ~pos:($startpos($1),$endpos($3)) (Invoke ($1, $3)); "", $4])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr{ mk_fun ~pos:$loc $3 $6 }
  | LCUR exprss RCUR                 { mk_fun ~pos:$loc [] $2 }
  | WHILE expr DO exprs END          { mk ~pos:$loc (App (mk ~pos:$loc($1) (Var "while"), ["", mk_fun ~pos:$loc($2) [] $2; "", mk_fun ~pos:$loc($4) [] $4])) }
  | FOR optvar GETS expr DO exprs END
                                     { mk ~pos:$loc (App (mk ~pos:$loc($1) (Var "for"), ["", $4; "", mk_fun ~pos:$loc($6) ["", $2, Type.var ~pos:$loc($2) (), None] $6])) }
  | expr TO expr                     { mk ~pos:$loc (App (mk ~pos:$loc($2) (Invoke (mk ~pos:$loc($2) (Var "iterator"), "int")), ["", $1; "", $3])) }
  | expr COALESCE expr               { let null = mk ~pos:$loc($1) (Var "null") in
                                       let op =  mk ~pos:$loc($1) (Invoke (null, "default")) in
                                       let handler = mk_fun ~pos:$loc($3) [] $3 in
                                       mk ~pos:$loc (App (op, ["",$1;"",handler])) }
  | TRY exprs CATCH optvar COLON varlist DO exprs END
                                     { let fn = mk_fun ~pos:$loc($2) [] $2 in
                                       let err_arg = ["", $4, Type.var ~pos:$loc($4) (), None] in
                                       let errors = mk_list ~pos:$loc $6 in
                                       let handler =  mk_fun ~pos:$loc($8) err_arg $8 in
                                       let error_module = mk ~pos:$loc($1) (Var "error") in
                                       let op = mk ~pos:$loc($1) (Invoke (error_module, "catch")) in
                                       mk ~pos:$loc (App (op, ["errors", errors; "", fn; "", handler])) }
  | TRY exprs CATCH optvar DO exprs END { let fn = mk_fun ~pos:$loc($2) [] $2 in
                                       let err_arg = ["", $4, Type.var ~pos:$loc($4) (), None] in
                                       let handler = mk_fun ~pos:$loc($6) err_arg $6 in
                                       let errors = mk ~pos:$loc Null in
                                       let error_module = mk ~pos:$loc($1) (Var "error") in
                                       let op = mk ~pos:$loc($1) (Invoke (error_module, "catch")) in
                                       mk ~pos:$loc (App (op, ["errors", errors; "", fn; "", handler])) }
  | IF exprs THEN exprs if_elsif END { let cond = $2 in
                                       let then_b = mk_fun ~pos:($startpos($3),$endpos($4)) [] $4 in
                                       let else_b = $5 in
                                       let op = mk ~pos:$loc($1) (Var "if") in
                                       mk ~pos:$loc (App (op, ["", cond; "then", then_b; "else", else_b])) }
  | REGEXP                          {  let regexp, flags = $1 in
                                       let regexp =  mk ~pos:$loc (Ground (String regexp)) in
                                       let flags = List.map Char.escaped flags in
                                       let flags = List.map (fun s -> mk ~pos:$loc (Ground (String s))) flags in
                                       let flags = mk ~pos:$loc (List flags) in
                                       let op = mk ~pos:$loc($1) (Var "regexp") in
                                       mk ~pos:$loc (App (op, ["", regexp; "flags", flags])) }
  | expr QUESTION expr COLON expr    { let cond = $1 in
                                       let then_b = mk_fun ~pos:$loc($3) [] $3 in
                                       let else_b = mk_fun ~pos:$loc($5) [] $5 in
                                       let op = mk ~pos:$loc($1) (Var "if") in
                                       mk ~pos:$loc (App (op, ["", cond; "then", then_b; "else", else_b])) }

  | expr BINB expr                 { let left = mk_fun ~pos:$loc($1) [] $1 in
                                     let right= mk_fun ~pos:$loc($3) [] $3 in
                                     mk ~pos:$loc (App (mk ~pos:$loc($2) (Var $2), ["",left;"",right])) }
  | expr BIN1 expr                 { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var $2), ["",$1;"",$3])) }
  | expr BIN2 expr                 { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var $2), ["",$1;"",$3])) }
  | expr BIN3 expr                 { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var $2), ["",$1;"",$3])) }
  | expr TIMES expr                { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var "*"), ["",$1;"",$3])) }
  | expr MINUS expr                { mk ~pos:$loc (App (mk ~pos:$loc($2) (Var "-"), ["",$1;"",$3])) }
  | time_predicate                 { $1 }

time_predicate:
  | INTERVAL { mk_time_pred ~pos:$loc (between ~pos:$loc (fst $1) (snd $1)) }
  | TIME     { mk_time_pred ~pos:$loc (during ~pos:$loc $1) }

ty:
  | UNDERSCORE                   { Type.var ~pos:$loc () }
  | VAR                          { mk_ty ~pos:$loc $1 }
  | ty QUESTION                  { Type.make ~pos:$loc (Type.Nullable $1) }
  | LBRA ty RBRA                 { Type.make ~pos:$loc (Type.(List {t = $2; json_repr = `Tuple})) }
  | LBRA ty RBRA VAR VAR DOT VAR { mk_json_assoc_object_ty ~pos:$loc ($2,$4,$5,$7) }
  | LPAR ty_tuple RPAR           { Type.make ~pos:$loc (Type.Tuple $2) }
  | LPAR argsty RPAR YIELDS ty   { Type.make ~pos:$loc (Type.Arrow ($2,$5)) }
  | LCUR record_ty RCUR          { $2 }
  | ty DOT LCUR record_ty RCUR   { Type.remeth $4 $1 }
  | ty_source                    { $1 }

record_ty:
  |                         { Type.make ~pos:$loc (Type.Tuple []) }
  | meth_ty                 { let name, ty, json_name = $1 in
                              Type.meth ~pos:$loc ?json_name name ([], ty) (Type.make ~pos:$loc (Type.Tuple [])) }
  | meth_ty COMMA record_ty { let name, ty, json_name = $1 in
                              Type.meth ~pos:$loc ?json_name name ([], ty) $3 }

meth_ty:
  | VAR COLON ty            { $1, $3, None }
  | STRING VAR VAR COLON ty {
       match $2 with
         |"as" ->             $3, $5, Some $1
         | _ -> raise (Parse_error ($loc, "Invalid type constructor")) }

ty_source:
  | VARLPAR RPAR                  { mk_source_ty ~pos:$loc $1 [] }
  | VARLPAR ty_source_params RPAR { mk_source_ty ~pos:$loc $1 $2 }

ty_source_params:
  | VAR GETS ty_content { [$1,$3] }
  | VAR GETS ty_content COMMA ty_source_params { ($1,$3)::$5 }

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
  | STRING               { "",$1 }
  | VAR GETS VAR         { $1,$3 }
  | VAR GETS STRING      { $1,$3}
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
                          { append_list ~pos:$loc $1 $3 }
  | inner_list_item       { append_list ~pos:$loc $1 (`List []) }
  |                       { `List [] }

inner_list_item:
  | DOTDOTDOT expr { `Ellipsis $2 }
  | expr           { `Expr $1 }

inner_tuple:
  | expr COMMA expr { [$1;$3] }
  | expr COMMA inner_tuple { $1::$3 }

app_list_elem:
  | VAR GETS expr { [$1,$3] }
  | expr          { ["",$1] }
  | ARGS_OF LPAR VAR RPAR        { app_of ~only:[] ~except:[] ~pos:$loc $3 }
  | ARGS_OF LPAR subfield RPAR
                                 { app_of ~only:[] ~except:[] ~pos:$loc (String.concat "." $3) }
  | ARGS_OF LPAR VARLBRA args_of_params RBRA RPAR { app_of ~pos:$loc ~only:(fst $4) ~except:(snd $4) $3 }
  | ARGS_OF LPAR subfield_lbra args_of_params RBRA RPAR
                                 { app_of ~pos:$loc (String.concat "." $3) ~only:(fst $4) ~except:(snd $4) }
app_list:
  |                              { [] }
  | app_list_elem                { $1 }
  | app_list_elem COMMA app_list { $1@$3 }

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
  | LPAR pattern_list RPAR             { PTuple $2 }

list_pattern:
  | LBRA pattern_list_with_spread RBRA { PList $2 }

meth_pattern_el:
  | VAR              { $1, None }
  | VAR GETS pattern { $1, Some $3 }

meth_pattern_list:
  |                                         { [] }
  | meth_pattern_el                         { [$1] }
  | meth_pattern_el COMMA meth_pattern_list { $1::$3 }

record_pattern:
  | LCUR meth_pattern_list RCUR { $2 }

meth_pattern:
  | record_pattern                   { PMeth (None,              $1) }
  | VAR DOT record_pattern           { PMeth (Some (PVar [$1]),  $3) }
  | UNDERSCORE DOT record_pattern    { PMeth (Some (PVar ["_"]), $3) }
  | tuple_pattern DOT record_pattern { PMeth (Some $1,           $3) }
  | list_pattern DOT record_pattern  { PMeth (Some $1,           $3) }

var_pattern:
  | optvar { PVar [$1] }

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
  | VAR           { $1, mk ~pos:$loc (Var $1) }
  | VAR GETS expr { $1, $3 }

let_opt:
  | let_opt_el               { [$1] }
  | let_opt_el COMMA let_opt { $1::$3 }

_let:
  | LET { Parser_helper.let_decoration_of_lexer_let_decoration $1 }
  | LETLBRA let_opt RBRA {
      match $1 with
        | `Json_parse     -> `Json_parse (Parser_helper.args_of_json_parse ~pos:$loc $2)
        | `Json_stringify -> `Json_stringify (Parser_helper.args_of_json_stringify ~pos:$loc $2)
        | _ -> raise (Parse_error ($loc, "Invalid let constructor")) }

binding:
  | optvar GETS expr         { (Doc.none (),[],[]), `None,  PVar [$1], None,    $3, None }
  | _let pattern GETS expr   { (Doc.none (),[],[]), $1,     $2,        None,    $4, None }
  | _let LPAR pattern COLON ty RPAR GETS expr
                             { (Doc.none (),[],[]), $1,     $3,        None,    $8, Some $5 }
  | _let subfield GETS expr  { (Doc.none (),[],[]), $1,     PVar $2,   None,    $4, None }
  | DEF pattern g exprs END  { fst $1,              snd $1, $2,        None,    $4, None }
  | DEF LPAR pattern COLON ty RPAR g exprs END
                             { fst $1,              snd $1, $3,        None,    $8, Some $5 }
  | DEF subfield g exprs END { fst $1,              snd $1, PVar $2,   None,    $4, None }
  | DEF varlpar arglist RPAR g exprs END
                             { fst $1,              snd $1, PVar $2, Some $3,   $6, None }

varlpar:
  | VARLPAR         { [$1] }
  | VAR DOT varlpar { $1::$3 }

arglist:
  |                       { [] }
  | arg                   { $1 }
  | arg COMMA arglist     { $1@$3 }
arg:
  | TILD VAR opt { [$2, $2, Type.var ~pos:$loc($2) (), $3] }
  | TILD LPAR VAR COLON ty RPAR opt { [$3, $3, $5, $7 ] }
  | TILD VAR GETS UNDERSCORE opt { [$2, "_", Type.var ~pos:$loc($2) (), $5] }
  | optvar opt  { ["", $1, Type.var ~pos:$loc($1) (), $2] }
  | LPAR optvar COLON ty RPAR opt { ["", $2, $4, $6] }
  | ARGS_OF LPAR VAR RPAR { args_of ~only:[] ~except:[] ~pos:$loc $3 }
  | ARGS_OF LPAR subfield RPAR
                          { args_of ~only:[] ~except:[] ~pos:$loc (String.concat "." $3) }
  | ARGS_OF LPAR VARLBRA args_of_params RBRA RPAR { args_of ~pos:$loc ~only:(fst $4) ~except:(snd $4) $3 }
  | ARGS_OF LPAR subfield_lbra args_of_params RBRA RPAR
                          { args_of ~pos:$loc (String.concat "." $3) ~only:(fst $4) ~except:(snd $4) }
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
  | ELSIF exprs THEN exprs if_elsif { let cond = $2 in
                                      let then_b = mk_fun ~pos:($startpos($3), $endpos($4)) [] $4 in
                                      let else_b = $5 in
                                      let op = mk ~pos:$loc($1) (Var "if") in
                                      mk_fun ~pos:$loc [] (mk ~pos:$loc (App (op,["",cond; "then",then_b; "else",else_b]))) }
  | ELSE exprs                      { mk_fun ~pos:$loc($2) [] $2 }
  |                                 { mk_fun ~pos:$loc [] (mk ~pos:$loc unit) }

encoder_opt:
  | %prec no_app { [] }
  | LPAR encoder_params RPAR { $2 }

encoder_param:
  | VAR GETS expr       { $1, `Term $3 }
  | STRING GETS expr    { $1, `Term $3 }
  | VAR                 { "", `Term (mk ~pos:$loc (Ground (String $1))) }
  | STRING              { "", `Term (mk ~pos:$loc (Ground (String $1))) }
  | ENCODER encoder_opt { "", `Encoder ($1, $2) }

encoder_params:
  |                                    { [] }
  | encoder_param                      { [$1] }
  | encoder_param COMMA encoder_params { $1::$3 }

record:
  | VAR GETS expr { fun ~pos e -> mk ~pos (Meth ($1, $3, e)) }
  | record COMMA VAR GETS expr { fun ~pos e -> mk ~pos (Meth ($3, $5, $1 ~pos e)) }

annotate:
  | annotate_metadata COLON { $1 }

annotate_metadata:
  | annotate_metadata_entry annotate_metadata { $1::$2 }
  | annotate_key GETS annotate_value { [$1, $3] }

annotate_metadata_entry:
  | annotate_key GETS annotate_value COMMA { $1, $3 }

annotate_key:
  | VAR { $1 }
  | STRING { $1 }

annotate_value:
  | INT { string_of_int $1 }
  | FLOAT { string_of_float $1 }
  | BOOL { string_of_bool $1 }
  | VAR { $1 }
  | STRING { $1 }
