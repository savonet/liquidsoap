/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
  open Lang_values

  (** Locations *)

  let curpos ?pos () =
    match pos with
      | None -> Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()
      | Some (i,j) -> Parsing.rhs_start_pos i, Parsing.rhs_end_pos j

  let fresh_kindvar ?pos () =
    T.fresh_evar ~level:(List.length !bindings) ~pos:(Some (curpos ?pos ()))

  (** Create a new value with an unknown type. *)
  let mk ?pos e =
    let kind = fresh_kindvar ?pos () in
      if Lang_values.debug then
        Printf.eprintf "%s: %s\n"
          (T.print_pos (Utils.get_some kind.T.pos)) (T.print kind) ;
      { t = kind ; value = e }

  let bind ~kind var =
    if Lang_values.debug then
      Printf.eprintf "%s: %s <- %s\n"
        (T.print_pos (curpos())) var (T.print kind) ;
    bindings :=
      (var,{ t = kind ; value = Var var })::!bindings

  let pop () = bindings := List.tl !bindings
  let rec popn n = if n > 0 then ( pop () ; popn (n-1) )

  let lookup ?pos var =
    try
      let v = List.assoc var !bindings in
        (* Copy the value so that each instance of the variable
         * can get its type instantiated without affecting the others.
         * By the way, create a meaningless link to the variable in order to
         * store the parsing location. *)
        { v with t = T.make ~pos:(Some (curpos ?pos ())) (T.Link v.t) }
    with
      | Not_found -> raise (Lang_values.Unbound var)

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
    let args = List.map (fun x -> "", mk (Int x)) [a;b;c] in
      mk (App (lookup "time_in_mod", args))

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
%token BEGIN END GETS TILD
%token <Doc.item * (string*string) list> DEF
%token IF THEN ELSE ELSIF
%token LPAR RPAR COMMA SEQ
%token LBRA RBRA LCUR RCUR
%token FUN YIELDS
%token <string> BIN0
%token <string> BIN1
%token <string> BIN2
%token <string> BIN3
%token MINUS
%token NOT
%token PP_IFDEF PP_ENDIF PP_ENDL PP_INCLUDE PP_DEF
%token <string list> PP_COMMENT

%nonassoc COERCION
%left BIN0
%left BIN1
%left BIN2 MINUS NOT
%left BIN3

%start scheduler
%type <Lang_values.value> scheduler

%%

scheduler:
  | init exprs EOF { $2 }

init:
  | { (* Make sure that the builtins are in the binding stack before
       * parsing the expression. *)
      init_bindings () }

s: | {} | SEQ  {}
g: | {} | GETS {}

exprs:
  | cexpr s                  { $1 }
  | cexpr s exprs            { mk (Seq ($1,$3)) }
  | binding s                { let doc,name,def = $1 in
                                 pop () ;
                                 mk (Let (doc,name,def,mk Unit)) }
  | binding s exprs          { let doc,name,def = $1 in
                                 pop () ;
                                 mk (Let (doc,name,def,$3)) }

list:
  | LBRA inner_list RBRA { $2 }
inner_list:
  | expr COMMA inner_list  { $1::$3 }
  | expr                   { [$1] }
  |                        { [] }

expr:
  | cexpr %prec COERCION             { $1 }
  | MINUS INT                        { mk (Int (~- $2)) }
  | MINUS FLOAT                      { mk (Float (~-. $2)) }
cexpr:
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk (Int $1) }
  | NOT cexpr                        { mk (App (lookup "not", ["", $2])) }
  | BOOL                             { mk (Bool $1) }
  | FLOAT                            { mk (Float  $1) }
  | STRING                           { mk (String $1) }
  | list                             { mk (List $1) }
  | LPAR RPAR                        { mk Unit }
  | LPAR expr COMMA expr RPAR        { mk (Product ($2,$4)) }
  | VAR                              { lookup $1 }
  | VARLPAR app_list RPAR            { mk (App (lookup ~pos:(1,1) $1,$2)) }
  | VARLBRA expr RBRA                { mk (App (lookup "_[_]",
                                           ["",$2;"",lookup ~pos:(1,1) $1])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { let arglist = $3 in
                                         popn (List.length arglist) ;
                                         mk (Fun (arglist,$6)) }
  | LCUR exprs RCUR                  { mk (Fun ([],$2)) }
  | IF exprs THEN exprs if_elsif END
                                     { let cond = $2 in
                                       let then_b =
                                         mk ~pos:(3,4) (Fun ([],$4))
                                       in
                                       let else_b = $5 in
                                       let op = lookup ~pos:(1,1) "if" in
                                         mk (App (op,["",cond;
                                                      "else",else_b;
                                                      "then",then_b])) }
  | cexpr BIN0 cexpr                 { mk (App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN1 cexpr                 { mk (App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN2 cexpr                 { mk (App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr BIN3 cexpr                 { mk (App (lookup $2,
                                                     ["",$1;"",$3])) }
  | cexpr MINUS cexpr                { mk (App (lookup "-",
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
         bind ~kind:body.t $1 ;
         (Doc.none (),[]),$1,body
    }
  | DEF VAR g exprs END {
      let body = $4 in
        bind ~kind:body.t $2 ;
        $1,$2,body
    }
  | DEF VARLPAR arglist RPAR g exprs END {
      let arglist = $3 in
      let body = mk (Fun (arglist,$6)) in
        popn (List.length arglist) ;
        bind ~kind:body.t $2 ;
        $1,$2,body
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

if_elsif:
  | ELSIF exprs THEN exprs if_elsif { let cond = $2 in
                                      let then_b =
                                        mk ~pos:(3,4) (Fun ([],$4))
                                      in
                                      let else_b = $5 in
                                      let op = lookup ~pos:(1,1) "if" in
                                        mk (Fun ([],
                                          mk (App (op,["",cond;
                                                       "else",else_b;
                                                       "then",then_b])))) }
  | ELSE exprs                      { mk ~pos:(1,2) (Fun([],$2)) }
  |                                 { mk (Fun ([],mk Unit)) }
