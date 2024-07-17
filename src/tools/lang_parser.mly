/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

 *****************************************************************************/

%{

  open Source
  open Lang

  (** Locations *)

  let curpos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

  let mk ?(kind = Lang.fresh_kindvar()) ?(pos = curpos ()) e =
    if Lang.debug then
      Printf.eprintf "%s: %s\n" (print_pos pos) (print_kind kind) ;
    { pos = pos ; kind = kind ; value = e }

  (** We're building an AST annotated with unknown types.
    * There is a need to keep track of the type variables introduced for
    * abstractions and let-in constructs. The same system is used for
    * registering the constants of type source, i.e. the sources
    * and operators defined in liquidsoap. *)

  let bind ?(kind = Lang.fresh_kindvar()) var =
    if Lang.debug then
      Printf.eprintf "%s: %s <- %s\n"
        (print_pos (curpos())) var (print_kind kind) ;
    bindings :=
      (var,{ pos = dummy_pos ; kind = kind ; value = Var var })::!bindings

  let pop () = bindings := List.tl !bindings
  let rec popn n = if n > 0 then ( pop () ; popn (n-1) )

  let lookup var =
    try
      { (List.assoc var !bindings) with pos = curpos () }
    with
      | Not_found -> raise (Lang.Unbound var)

  (** Time intervals *)

  let time_units = [| 7*24*60*60 ; 24*60*60 ; 60*60 ; 60 ; 1 |]

  let date =
    let to_int = function None -> 0 | Some i -> i in
    let rec aux = function
      | None::tl -> aux tl
      | [] -> failwith "Invalid time"
      | l ->
          let a = Array.of_list l in
          let n = Array.length a in
          let tu = time_units and tn = Array.length time_units in
            Array.fold_left (+) 0
              (Array.mapi (fun i s ->
                             let s =
                               if n=4 && i=0 then
                                 (to_int s) mod 7
                               else
                                 to_int s
                             in
                               tu.(tn-1 + i - n+1) * s) a)
    in
      aux

  let rec last_index e n = function
    | x::tl -> if x=e then last_index e (n+1) tl else n
    | [] -> n

  let precision d = time_units.(last_index None 0 d)
  let duration d = time_units.(Array.length time_units - 1 - 
                               last_index None 0 (List.rev d))

  let between d1 d2 =
    let p1 = precision d1 in
    let p2 = precision d2 in
    let t1 = date d1 in
    let t2 = date d2 in
      if p1<>p2 then failwith "Invalid time interval: precisions differ" ;
      (t1,t2,p1)

  let during d =
    let t,d,p = date d, duration d, precision d in
      (t,t+d,p)

  let mk_time_pred (a,b,c) =
    let args = List.map (fun x -> "", mk (Lang.Int x)) [a;b;c] in
      mk (Lang.App (lookup "time_in_mod", args))

%}

%token <string> VAR
%token <string> VARLPAR
%token <string> VARLBRA
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <int option list> TIME
%token <int option list * int option list> INTERVAL
%token EOF
%token SET DEF BEGIN END GETS TILD
%token IF THEN ELSE
%token LPAR RPAR COMMA SEQ
%token LBRA RBRA LCUR RCUR
%token FUN YIELDS
%token <string> BIN0
%token <string> BIN1
%token <string> BIN2
%token <string> BIN3
%token MINUS
%token NOT
%token PP_IFDEF PP_ENDIF PP_ENDL

%nonassoc COERCION
%left BIN0
%left BIN1
%left BIN2 MINUS NOT
%left BIN3

%start scheduler
%type <Lang.value> scheduler

%%

scheduler:
  | settings exprs EOF { $2 }

settings:
  | { (* Make sure that the builtins are in the binding stack before
       * parsing the expression. *)
      init_bindings () }
  | settings_base settings { }
settings_base:
  | SET VAR GETS STRING      { Dtools.Var.set_string $2 $4 }
  | SET VAR GETS INT         { Dtools.Var.set_int    $2 $4 }
  | SET VAR GETS FLOAT       { Dtools.Var.set_float  $2 $4 }
  | SET VAR GETS BOOL        { Dtools.Var.set_bool   $2 $4 }
  | SET VAR GETS string_list { Dtools.Var.set_list   $2 $4 }

string_list:
  | LBRA RBRA                        { [] }
  | LBRA inner_string_list RBRA      { $2 }

inner_string_list:
  | STRING                           { [$1] }
  | STRING COMMA inner_string_list   { $1::$3 }


/* The code **************************************************************** */

s: | {} | SEQ  {}
g: | {} | GETS {}

exprs:
  | cexpr s                  { $1 }
  | cexpr s exprs            { mk (Lang.Seq ($1,$3)) }
  | binding s                { let name,def = $1 in
                                 pop () ;
                                 mk ~kind:Lang.unit_t
                                   (Lang.Let (name,def,mk Lang.Unit)) }
  | binding s exprs          { let name,def = $1 in
                                 pop () ;
                                 mk ~kind:$3.kind
                                   (Lang.Let (name,def,$3)) }

list:
  | LBRA inner_list RBRA { $2 }
inner_list:
  | expr COMMA inner_list  { $1::$3 }
  | expr                   { [$1] }
  |                        { [] }

expr:
  | cexpr %prec COERCION             { $1 }
  | MINUS INT                        { mk (Lang.Int (~- $2)) }
  | MINUS FLOAT                      { mk (Lang.Float (~-. $2)) }
cexpr:
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk (Lang.Int $1) }
  | NOT cexpr                        { mk (Lang.App (lookup "not", ["", $2])) }
  | BOOL                             { mk (Lang.Bool $1) }
  | FLOAT                            { mk (Lang.Float  $1) }
  | STRING                           { mk (Lang.String $1) }
  | list                             { mk (Lang.List $1) }
  | LPAR RPAR                        { mk Lang.Unit }
  | LPAR expr COMMA expr RPAR        { mk (Lang.Product ($2,$4)) }
  | VAR                              { lookup $1 }
  | VARLPAR app_list RPAR            { mk (Lang.App (lookup $1,$2)) }
  | VARLBRA expr RBRA                { mk (Lang.App (lookup "_[_]",
                                           ["",$2;"",lookup $1])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { let arglist = $3 in
                                         popn (List.length arglist) ;
                                         mk (Lang.Fun (arglist,$6)) }
  | LCUR exprs RCUR                  { mk (Lang.Fun ([],$2)) }
  | IF exprs THEN exprs END          { let test = $2 in
                                       let block = mk (Lang.Fun ([],$4)) in
                                       let op = lookup "if" in
                                         mk (Lang.App (op,["",test;
                                                           "then",block])) }
  | IF exprs THEN exprs ELSE exprs END
                                     { let test = $2 in
                                       let blockt = mk (Lang.Fun ([],$4)) in
                                       let blocke = mk (Lang.Fun ([],$6)) in
                                       let op = lookup "if" in
                                         mk (Lang.App (op,["",test;
                                                           "then",blockt;
                                                           "else",blocke])) }
  | cexpr BIN0 cexpr                 { mk (Lang.App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN1 cexpr                 { mk (Lang.App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN2 cexpr                 { mk (Lang.App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN3 cexpr                 { mk (Lang.App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr MINUS cexpr                { mk (Lang.App (lookup "-",
                                                     ["",$1;"",$3])) }
  | INTERVAL                       { mk_time_pred (between (fst $1) (snd $1)) }
  | TIME                           { mk_time_pred (during $1) }

app_list_elem:
  | VAR GETS expr { $1,$3 }
  | expr          { "",$1 }
app_list:
  |                                { [] }
  | app_list_elem                 { [$1] }
  | app_list_elem COMMA app_list { $1::$3 }

binding:
  | VAR GETS expr {
       let body = $3 in
         bind ~kind:body.kind $1 ;
         $1,body
    }
  | DEF VAR g exprs END {
      let body = $4 in
        bind ~kind:body.kind $2 ;
        $2,body
    }
  | DEF VARLPAR arglist RPAR g exprs END {
      let arglist = $3 in
      let body = mk (Lang.Fun (arglist,$6)) in
        popn (List.length arglist) ;
        bind ~kind:body.kind $2 ;
        $2,body
    }

arglist:
  |                   { [] }
  | arg               { [$1] }
  | arg COMMA arglist { $1::$3 }
arg:
  | TILD VAR opt { let kind = fresh_kindvar () in
                     bind ~kind $2 ;
                     $2,$2,kind,$3 }
  | VAR opt      { let kind = fresh_kindvar () in
                     bind ~kind $1 ;
                     "",$1,kind,$2 }
opt:
  | GETS expr { Some $2 }
  |           { None }
