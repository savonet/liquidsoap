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

%{
open Parsed_term
(* All auxiliary functions for parser are there *)
open Parser_helper
%}

%token <string> VAR
%token <string> VARLPAR
%token <string> VARLBRA
%token <Lang_string.Version.t> VERSION
%token <char> PP_STRING_START
%token <string * char list * Parsed_term.pos> PP_REGEXP
%token <char * string > STRING
%token <string * string> RAW_STRING
%token <string * char list > REGEXP
%token <string> INT PP_INT_DOT_LCUR
%token <string> FLOAT
%token NULL
%token NULLDOT
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
%token COALESCE
%token TRY CATCH FINALLY DO
%token IF THEN ELSE ELSIF
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
%type <Parsed_term.t> program

%start interactive
%type <Parsed_term.t> interactive

%start annotate
%type <(string * string) list> annotate

%start annotate_metadata_entry
%type <string * string> annotate_metadata_entry

%start time_predicate
%type <Parsed_term.t> time_predicate

%start plain_encoder_params
%type <Parsed_term.encoder_params> plain_encoder_params

%type <Parsed_term.let_decoration> _let
%type <string> annotate_key
%type <(string * string) list> annotate_metadata
%type <string> annotate_value
%type <Parsed_term.app_arg list> app_list
%type <Parsed_term.app_arg> app_list_elem
%type <Parser_helper.arglist> arglist
%type <string list * string list> args_of_params
%type <Parsed_term.type_annotation Type.argument list> argsty
%type <Parsed_term.type_annotation Type.argument> argty
%type <Parsed_term._let> explicit_binding
%type <Parsed_term._let> binding
%type <Parsed_term.encoder_params> encoder_params
%type <Parsed_term.t> expr
%type <Parsed_term.statement list> exprs
%type <Parsed_term.statement list> simple_fun_body
%type <unit> g
%type <Parsed_term.if_elsif list * (Parsed_term.pos * Parsed_term.statement list) option> if_elsif
%type <string list> in_subfield
%type <string list> in_subfield_lbra
%type <Parsed_term.list_el list> inner_list
%type <Parsed_term.list_el> inner_list_item
%type <Parsed_term.t list> inner_tuple
%type <Parser_helper.let_opt_el list> let_opt
%type <Parser_helper.let_opt_el> let_opt_el
%type <Parsed_term.meth_annotation> meth_ty
%type <Parsed_term.t option> opt
%type <string> optvar
%type <Parsed_term.pattern> pattern
%type <Parsed_term.pattern list> pattern_list
%type <Parsed_term.methods list> record
%type <Parsed_term.meth_annotation list> record_ty
%type <unit> s
%type <string> spread
%type <string list> subfield
%type <string list> subfield_lbra
%type <Parsed_term.type_annotation> ty
%type <string * Parsed_term.track_annotation list> ty_content
%type <Parsed_term.track_annotation> ty_content_arg
%type <Parsed_term.source_annotation> ty_source_tracks
%type <Parsed_term.type_annotation list> ty_tuple
%type <Parsed_term.list_el list> varlist
%type <string list> subfield_lpar

%%

program:
  | error { raise (Term.Parse_error ($loc, "Syntax error!")) }
  | EOF { mk ~pos:$loc (`Block (mk_block ~pos:$loc [])) }
  | exprs EOF { mk ~pos:$loc (`Block (mk_block ~pos:$loc $1)) }

interactive:
  | error { raise (Term.Parse_error ($loc, "Syntax error!")) }
  | exprs SEQSEQ { mk ~pos:$loc (`Block (mk_block ~pos:$loc $1)) }
  | EOF { raise End_of_file }

s: | {} | SEQ  {}
g: | {} | GETS {}

(* A block is a flat list of statements. A binding scopes over the statements
   that follow it in the same block; that is the only scoping rule, and the
   reducer implements it in exactly one place. *)
block_body(STMT):
  | STMT s                   { [$1] }
  | STMT s block_body(STMT)  { $1::$3 }

exprs: block_body(stmt) { $1 }

(* Inside `{ ... }` only the *first* statement is restricted: a leading
   `x = 1` is the record literal `{ x = 1 }`, so it must be written
   `let x = 1`. Once past that, `{ ... }` is an ordinary block. *)
simple_fun_body:
  | fun_stmt s        { [$1] }
  | fun_stmt s exprs  { $1::$3 }

stmt:
  | expr             { mk_stmt ~pos:$loc (`Expr $1) }
  | binding          { mk_stmt ~pos:$loc (`Binding $1) }
  | common_stmt      { mk_stmt ~pos:$loc $1 }

fun_stmt:
  | expr             { mk_stmt ~pos:$loc (`Expr $1) }
  | explicit_binding { mk_stmt ~pos:$loc (`Binding $1) }
  | common_stmt      { mk_stmt ~pos:$loc $1 }

common_stmt:
  | OPEN expr        { `Open $2 }
  | INCLUDE          { `Include $1 }

(* General expressions. *)
expr:
  | static_if                        { mk ~pos:$loc (`Static_if $1) }
  | LPAR expr COLON ty RPAR          { mk ~pos:$loc (`Cast {cast = $2; typ = $4}) }
  | UMINUS expr                      { mk ~pos:$loc (`Negative $2) }
  | LPAR expr RPAR                   { mk ~pos:$loc (`Parenthesis $2) }
  | INT                              { mk ~pos:$loc (`Int $1) }
  | NOT expr                         { mk ~pos:$loc (`Not $2) }
  | BOOL                             { mk ~pos:$loc (`Bool $1) }
  | FLOAT                            { mk ~pos:$loc (`Float $1) }
  | STRING                           { mk ~pos:$loc (`String $1) }
  | RAW_STRING                       { mk ~pos:$loc (`Raw_string $1) }
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
  | NULL                             { mk ~pos:$loc `Null }
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
  | BEGIN exprs END                  { mk ~pos:$loc (`Block (mk_block ~pos:($startpos($1),$endpos($3)) $2)) }
  | FUN LPAR arglist RPAR YIELDS expr{ mk_fun ~pos:$loc $3 $6 }
  | LCUR simple_fun_body RCUR        { mk ~pos:$loc (`Simple_fun (mk_block ~pos:($startpos($1),$endpos($3)) $2)) }
  | WHILE expr DO exprs END
      { mk ~pos:$loc (`While {
          while_condition = $2;
          while_do_block = { block_body = $4;
                             block_pos = ($startpos($3), $startpos($5)) } }) }
  | FOR optvar GETS expr TO expr DO exprs END
      { mk ~pos:$loc (`For {
          for_variable = $2;
          for_from = $4;
          for_to = $6;
          for_do_block = { block_body = $8;
                           block_pos = ($startpos($7), $startpos($9)) } }) }
  | FOR optvar GETS expr DO exprs END
      { mk ~pos:$loc (`Iterable_for {
          iterable_for_variable = $2;
          iterable_for_iterator = $4;
          iterable_for_do_block = { block_body = $6;
                                    block_pos = ($startpos($5), $startpos($7)) } }) }
  | expr COALESCE expr               { mk ~pos:$loc (`Coalesce ($1, $3)) }
  | TRY exprs FINALLY exprs END
      { mk_try ~pos:$loc
          ~body_block:{ block_body = $2; block_pos = ($startpos($1), $startpos($3)) }
          ~finally_block:{ block_body = $4; block_pos = ($startpos($3), $startpos($5)) }
          () }
  | TRY exprs CATCH optvar COLON varlist DO exprs END
      { mk_try ~pos:$loc
          ~body_block:{ block_body = $2; block_pos = ($startpos($1), $startpos($3)) }
          ~handler:{ try_handler_variable = $4;
                     try_handler_errors_list = Some (mk ~pos:$loc($6) (`List $6));
                     try_handler_block = { block_body = $8; block_pos = ($startpos($7), $startpos($9)) };
                     try_handler_pos = ($startpos($3), $endpos($3)) }
          () }
  | TRY exprs CATCH optvar COLON varlist DO exprs FINALLY exprs END
      { mk_try ~pos:$loc
          ~body_block:{ block_body = $2; block_pos = ($startpos($1), $startpos($3)) }
          ~handler:{ try_handler_variable = $4;
                     try_handler_errors_list = Some (mk ~pos:$loc($6) (`List $6));
                     try_handler_block = { block_body = $8; block_pos = ($startpos($7), $startpos($9)) };
                     try_handler_pos = ($startpos($3), $endpos($3)) }
          ~finally_block:{ block_body = $10; block_pos = ($startpos($9), $startpos($11)) }
          () }
  | TRY exprs CATCH optvar DO exprs END
      { mk_try ~pos:$loc
          ~body_block:{ block_body = $2; block_pos = ($startpos($1), $startpos($3)) }
          ~handler:{ try_handler_variable = $4;
                     try_handler_errors_list = None;
                     try_handler_block = { block_body = $6; block_pos = ($startpos($5), $startpos($7)) };
                     try_handler_pos = ($startpos($3), $endpos($3)) }
          () }
  | TRY exprs CATCH optvar DO exprs FINALLY exprs END
      { mk_try ~pos:$loc
          ~body_block:{ block_body = $2; block_pos = ($startpos($1), $startpos($3)) }
          ~handler:{ try_handler_variable = $4;
                     try_handler_errors_list = None;
                     try_handler_block = { block_body = $6; block_pos = ($startpos($5), $startpos($7)) };
                     try_handler_pos = ($startpos($3), $endpos($3)) }
          ~finally_block:{ block_body = $8; block_pos = ($startpos($7), $startpos($9)) }
          () }
  | IF exprs THEN exprs if_elsif END
      { let (if_elsif, else_opt) = $5 in
        let if_else_block =
          match else_opt with
            | None -> None
            | Some (p, t) -> Some { block_body = t; block_pos = (fst p, $startpos($6)) }
        in
        let if_then_end = match if_elsif with
          | { elsif_pos; _ } :: _ -> fst elsif_pos
          | [] -> (match if_else_block with
              | Some b -> fst b.block_pos
              | None -> $startpos($6))
        in
        mk ~pos:$loc (`If {
          if_condition = expr_of_block ~pos:$loc($2) (mk_block ~pos:$loc($2) $2);
          if_then_block = { block_body = $4; block_pos = ($startpos($3), if_then_end) };
          if_elsif;
          if_else_block;
          if_end_pos = ($startpos($6), $endpos($6)) }) }
  | REGEXP                           {  mk ~pos:$loc (`Regexp $1) }
  | expr QUESTION expr COLON expr
      { mk ~pos:$loc (`Inline_if {
          if_condition = $1;
          if_then_block = { block_body = [mk_stmt ~pos:$loc($3) (`Expr $3)];
                            block_pos = ($startpos($2), $startpos($4)) };
          if_elsif = [];
          if_else_block = Some { block_body = [mk_stmt ~pos:$loc($5) (`Expr $5)];
                                 block_pos = ($startpos($4), $endpos($5)) };
          if_end_pos = ($startpos($5), $endpos($5)) }) }
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

(* Contextual keywords. These are ordinary identifiers elsewhere, so they are
   matched as variables and their spelling is checked in the action. %inline
   means the generated automaton is the same as writing the tokens out. *)
%inline as_kw:
  | VAR { Parser_helper.expect_keyword ~pos:$loc "as" $1 }

%inline json_object_kw:
  | VAR DOT VAR { Parser_helper.expect_keyword ~pos:$loc($1) "json" $1;
                  Parser_helper.expect_keyword ~pos:$loc($3) "object" $3 }

ty:
  | UNDERSCORE                   { `Named "_" }
  | VAR                          { `Named $1 }
  | ty QUESTION                  { `Nullable $1 }
  | LBRA ty RBRA                 { `List $2 }
  | LBRA ty RBRA as_kw json_object_kw
                                 { mk_json_object_ty ~pos:$loc($2) $2 }
  | LPAR ty_tuple RPAR           { `Tuple $2 }
  | LPAR argsty RPAR YIELDS ty   { `Arrow ($2,$5) }
  | LCUR record_ty RCUR          { `Record $2 }
  | ty DOT VAR                   { `Invoke ($1, $3) }
  | ty QUESTION_DOT LCUR record_ty RCUR
                                 { `Method (`Nullable $1, $4) }
  | ty DOT LCUR record_ty RCUR   { `Method ($1, $4) }
  | VARLPAR RPAR                  { mk_named_ty ~pos:$loc $1 None }
  | VARLPAR ty RPAR               { mk_named_ty ~pos:$loc $1 (Some $2) }
  | VARLPAR ty_source_tracks RPAR { mk_source_ty ~pos:$loc $1 $2 }

record_ty:
  |                         { [] }
  | meth_ty                 { [$1] }
  | meth_ty COMMA record_ty { $1::$3 }

meth_ty:
  | VAR COLON ty            { { optional_meth = false; name = $1; typ = $3; json_name = None } }
  | VAR QUESTION COLON ty   { { optional_meth = true; name = $1; typ = $4; json_name = None } }
  | STRING as_kw VAR COLON ty
                            { { optional_meth = false; name = $3; typ = $5; json_name = Some (render_string ~pos:$loc($1) $1) } }
  | STRING as_kw VAR QUESTION COLON ty
                            { { optional_meth = true; name = $3; typ = $6; json_name = Some (render_string ~pos:$loc($1) $1) } }

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
        | _ -> raise (Term.Parse_error ($loc, "Invalid let constructor")) }

def:
  | DEF { Parser_helper.let_decoration_of_lexer_let_decoration $1 }

explicit_binding:
  | _let pattern GETS expr   { Parser_helper.(let_args ~kind:`Let ~decoration:$1 ~pat:$2 ~def:$4 ()) }
  | _let LPAR pattern COLON ty RPAR GETS expr
                             { Parser_helper.(let_args ~kind:`Let ~decoration:$1 ~pat:$3 ~def:$8 ~cast:$5 ()) }
  | _let subfield GETS expr  { Parser_helper.(let_args ~kind:`Let ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~def:$4 ()) }
  | def optvar g exprs END   { Parser_helper.(let_args ~kind:`Def ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar [$2] }) ~def:(block_expr ~pos:$loc($4) $4) ()) }
  | def LPAR optvar COLON ty RPAR g exprs END
                             { Parser_helper.(let_args ~kind:`Def ~decoration:$1 ~pat:({ pat_pos = $loc($3); pat_entry =`PVar [$3] }) ~def:(block_expr ~pos:$loc($8) $8) ~cast:$5 ()) }
  | def subfield g exprs END { Parser_helper.(let_args ~kind:`Def ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~def:(block_expr ~pos:$loc($4) $4) ()) }
  | def subfield_lpar arglist RPAR g exprs END
                             { Parser_helper.(let_args ~kind:`Def ~decoration:$1 ~pat:({ pat_pos = $loc($2); pat_entry = `PVar $2 }) ~arglist:$3 ~def:(block_expr ~pos:$loc($6) $6) ()) }

binding:
  | expr GETS expr           { let pat, cast = Parser_helper.binding_target $1 in
                               Parser_helper.(let_args ~kind:`Bare ~decoration:`None ~pat ?cast ~def:$3 ()) }
  | UNDERSCORE GETS expr     { Parser_helper.(let_args ~kind:`Bare ~decoration:`None ~pat:({ pat_pos = $loc($1); pat_entry = `PVar ["_"] }) ~def:$3 ()) }
  | explicit_binding         { $1 }

subfield_lpar:
  | VARLPAR               { [$1] }
  | VAR DOT subfield_lpar { $1::$3 }

arglist:
  |                       { [] }
  | arg                   { [$1] }
  | arg COMMA arglist     { $1::$3 }
arg:
  | TILD VAR opt { `Term {label = $2; as_variable = None; typ = None; default = $3; annotations = []; pos = $loc($2) } }
  | TILD LPAR VAR COLON ty RPAR opt {
                   `Term {label = $3; as_variable = None; typ =  Some $5; default = $7; annotations = []; pos = $loc($3) }
                 }
  | TILD VAR GETS UNDERSCORE opt {
                    `Term {label = $2; as_variable = Some { pat_pos = $loc($4); pat_entry = `PVar ["_"] }; typ = None; default = $5;
                           annotations = [`Deprecated (Printf.sprintf "Use `~%s:_`" $2)];
                           pos = $loc($2) }
                 }
  | TILD VAR COLON pattern opt {
                   `Term {label = $2; as_variable = Some $4; typ =  None; default = $5; annotations = []; pos = $loc($4) }
                 }
  | TILD VAR COLON LPAR pattern COLON ty RPAR opt {
                   `Term {label = $2; as_variable = Some $5; typ =  Some $7; default = $9; annotations = []; pos = $loc($5) }
                 }
  | pattern opt   { `Term {label = ""; as_variable = Some $1; typ = None; default = $2; annotations = []; pos = $loc($1)} }
  | LPAR pattern COLON ty RPAR opt {
                   `Term {label = ""; as_variable =  Some $2; typ = Some $4; default =  $6; annotations = []; pos = $loc($2) }
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
  | ELSIF exprs THEN exprs if_elsif
      { let (rest, else_opt) = $5 in
        let e : Parsed_term.if_elsif = {
            elsif_condition = expr_of_block ~pos:$loc($2) (mk_block ~pos:$loc($2) $2);
            elsif_then_block = { block_body = $4; block_pos = ($startpos($3), $endpos($4)) };
            elsif_pos = ($startpos($1), $endpos($1)) }
        in (e :: rest, else_opt) }
  | ELSE exprs                      { [], Some (($startpos($1), $endpos($1)), $2) }
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

(* %ifdef / %ifversion / %ifencoder. These are statements, not expressions:
   the selected branch splices its statements into the enclosing block, so a
   binding made under a `%ifdef` scopes over the code that follows it. *)
static_if:
  | PP_IFDEF if_def_var static_body PP_ENDIF
      { { static_cond = `Defined ($1, String.concat "." $2);
          static_then = mk_block ~pos:($startpos($3), $startpos($4)) $3;
          static_else = None } }
  | PP_IFDEF if_def_var static_body PP_ELSE static_body PP_ENDIF
      { { static_cond = `Defined ($1, String.concat "." $2);
          static_then = mk_block ~pos:($startpos($3), $startpos($4)) $3;
          static_else = Some (mk_block ~pos:($startpos($4), $startpos($6)) $5) } }
  | PP_IFENCODER ENCODER static_body PP_ENDIF
      { { static_cond = `Encoder ($1, $2);
          static_then = mk_block ~pos:($startpos($3), $startpos($4)) $3;
          static_else = None } }
  | PP_IFENCODER ENCODER static_body PP_ELSE static_body PP_ENDIF
      { { static_cond = `Encoder ($1, $2);
          static_then = mk_block ~pos:($startpos($3), $startpos($4)) $3;
          static_else = Some (mk_block ~pos:($startpos($4), $startpos($6)) $5) } }
  | PP_IFVERSION if_version_op if_version_version static_body PP_ENDIF
      { { static_cond = `Version ($2, $3);
          static_then = mk_block ~pos:($startpos($4), $startpos($5)) $4;
          static_else = None } }
  | PP_IFVERSION if_version_op if_version_version static_body PP_ELSE static_body PP_ENDIF
      { { static_cond = `Version ($2, $3);
          static_then = mk_block ~pos:($startpos($4), $startpos($5)) $4;
          static_else = Some (mk_block ~pos:($startpos($5), $startpos($7)) $6) } }

static_body: exprs { $1 }

if_version_op:
  | BIN1 {
      match $1 with
        | "==" -> `Eq
        | ">=" -> `Geq
        | "<=" -> `Leq
        | "<" -> `Lt
        | ">" -> `Gt
        | _ -> raise (Term.Parse_error ($loc, "invalid %ifversion operand"))
  }

if_version_version:
  | VERSION  { $1 }
  | INT      { Lang_string.Version.of_string $1 }
  | FLOAT    { Lang_string.Version.of_string $1 }

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
