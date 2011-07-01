/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

  (** Parsing locations. *)
  let curpos ?pos () =
    match pos with
      | None -> Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()
      | Some (i,j) -> Parsing.rhs_start_pos i, Parsing.rhs_end_pos j

  (** Create a new value with an unknown type. *)
  let mk ?pos e =
    let kind =
      T.fresh_evar ~level:(-1) ~pos:(Some (curpos ?pos ()))
    in
      if Lang_values.debug then
        Printf.eprintf "%s (%s): assigned type var %s\n"
          (T.print_pos (Utils.get_some kind.T.pos))
          (try Lang_values.print_term {t=kind;term=e} with _ -> "<?>")
          (T.print kind) ;
      { t = kind ; term = e }

  let mk_fun ?pos args body =
    let bound = List.map (fun (_,x,_,_) -> x) args in
    let fv = Lang_values.free_vars ~bound body in
      mk ?pos (Fun (fv,args,body))

  (** Time intervals *)

  let time_units = [| 7*24*60*60 ; 24*60*60 ; 60*60 ; 60 ; 1 |]

  (** Given a date specified as a list of four values (whms),
    * return a date in seconds from the beginning of the week. *)
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

  (** Give the index of the first non-None value in the list. *)
  let last_index l =
    let rec last_index n = function
      | x::tl -> if x=None then last_index (n+1) tl else n
      | [] -> n
    in
      last_index 0 l

  (** Give the precision of a date-as-list.
    * For example, the precision of Xs is 1, XmYs is 60, XhYmZs 3600, etc. *)
  let precision d = time_units.(last_index d)

  (** Give the duration of a data-as-list.
   * For example, the duration of Xs is 1, Xm 60, XhYm 60, etc. *)
  let duration d = time_units.(Array.length time_units - 1 - 
                               last_index (List.rev d))

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
      mk (App (mk (Var "time_in_mod"), args))

  let mk_ty name args =
    match name with
      | "_" -> Lang_types.fresh_evar ~level:(-1) ~pos:None
      | "unit" -> Lang_types.make (Lang_types.Ground Lang_types.Unit)
      | "bool" -> Lang_types.make (Lang_types.Ground Lang_types.Bool)
      | "int" -> Lang_types.make (Lang_types.Ground Lang_types.Int)
      | "float" -> Lang_types.make (Lang_types.Ground Lang_types.Float)
      | "string" -> Lang_types.make (Lang_types.Ground Lang_types.String)
      | "source" ->
          (* TODO less confusion in hiding the stream_kind constructed type *)
          (* TODO print position in error message *)
          let audio,video,midi =
            match args with
              | ["",a;"",v;"",m] -> a,v,m
              | _::_::_::_ -> failwith "invalid type parameters"
              | l ->
                  List.iter
                    (fun (lbl,_) ->
                      if not (List.mem lbl ["audio";"video";"midi"]) then
                        failwith "invalid type parameters")
                    l ;
                  let assoc x =
                    try List.assoc x l with
                      | Not_found ->
                          Lang_types.fresh_evar ~level:(-1) ~pos:None
                  in
                    assoc "audio", assoc "video", assoc "midi"
          in
            Lang_values.source_t (Lang_values.frame_kind_t audio video midi)
      | _ -> failwith "unknown type constructor"

  open Lang_encoders

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
%token OGG FLAC VORBIS VORBIS_CBR VORBIS_ABR THEORA DIRAC SPEEX
%token WAV VOAACENC AACPLUS MP3 EXTERNAL
%token EOF
%token BEGIN END GETS TILD
%token <Doc.item * (string*string) list> DEF
%token IF THEN ELSE ELSIF
%token LPAR RPAR COMMA SEQ SEQSEQ COLON
%token LBRA RBRA LCUR RCUR
%token FUN YIELDS
%token <string> BIN0
%token <string> BIN1
%token <string> BIN2
%token <string> BIN3
%token TIMES
%token MINUS
%token NOT
%token REF GET SET
%token PP_IFDEF PP_ENDIF PP_ENDL PP_DEF
%token <string> PP_INCLUDE
%token <string list> PP_COMMENT

%nonassoc YIELDS       /* fun x -> (x+x) */
%right SET             /* expr := (expr + expr), expr := (expr := expr) */
%nonassoc REF          /* ref (1+2) */
%left BIN0             /* ((x+(y*z))==3) or ((not a)==b) */
%left BIN1
%nonassoc NOT
%left BIN2 MINUS
%left BIN3 TIMES
%nonassoc GET          /* (!x)+2 */

/* When in doubt, take LPAR as the beginning of an application (app_opt).
 * It's scary to give a precendence to LPAR but the hope is that there's
 * no ambiguity around it except for optional applications after %formats. */
%nonassoc no_app
%nonassoc LPAR

%start program
%type <Lang_values.term> program

%start interactive
%type <Lang_values.term> interactive

%%

program:
  | EOF { mk Unit }
  | exprs EOF { $1 }
interactive:
  | exprs SEQSEQ { $1 }
  | EOF { raise End_of_file }

s: | {} | SEQ  {}
g: | {} | GETS {}

/* A sequence of (concatenable) expressions and bindings,
 * separated by optional semicolon */
exprs:
  | cexpr s                  { $1 }
  | cexpr s exprs            { mk (Seq ($1,$3)) }
  | binding s                { let doc,name,def = $1 in
                                 mk (Let { doc=doc ; var=name ;
                                           gen = [] ; def=def ;
                                           body = mk Unit }) }
  | binding s exprs          { let doc,name,def = $1 in
                                 mk (Let { doc=doc ; var=name ;
                                           gen = [] ; def=def ;
                                           body = $3 }) }

/* General expressions.
 * The only difference with cexpr is the ability to start with an unary MINUS.
 * But having two rules, one coercion and one for MINUS expr, would
 * be wrong: we want to parse -3*2 as (-3)*2. */
expr:
  | LPAR expr COLON ty RPAR          { Lang_types.(<:) $2.Lang_values.t $4 ;
                                       $2 }
  /* The next three rules create reduce/reduce conflicts (98 currently).
   * Ocamlyacc doesn't let us solve it with precedence, only the
   * relative order of the rules matter (and we get a warning). */
  | MINUS FLOAT                      { mk (Float (-. $2)) }
  | MINUS INT                        { mk (Int (- $2)) }
  | MINUS expr                       { mk (App (mk ~pos:(1,1) (Var "~-"),
                                                ["", $2])) }
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk (Int $1) }
  | NOT expr                         { mk (App (mk ~pos:(1,1) (Var "not"),
                                                ["", $2])) }
  | BOOL                             { mk (Bool $1) }
  | FLOAT                            { mk (Float  $1) }
  | STRING                           { mk (String $1) }
  | list                             { mk (List $1) }
  | REF expr                         { mk (Ref $2) }
  | GET expr                         { mk (Get $2) }
  | expr SET expr                    { mk (Set ($1,$3)) }
  | MP3 app_opt                      { mk_mp3 $2 }
  | AACPLUS app_opt                  { mk_aacplus $2 }
  | VOAACENC app_opt                  { mk_voaacenc $2 }
  | FLAC app_opt                     { mk_flac $2 }
  | EXTERNAL app_opt                 { mk_external $2 }
  | WAV app_opt                      { mk_wav $2 }
  | OGG LPAR ogg_items RPAR          { mk (Encoder (Encoder.Ogg $3)) }
  | top_level_ogg_item               { mk (Encoder (Encoder.Ogg [$1])) }
  | LPAR RPAR                        { mk Unit }
  | LPAR expr COMMA expr RPAR        { mk (Product ($2,$4)) }
  | VAR                              { mk (Var $1) }
  | VARLPAR app_list RPAR            { mk (App (mk ~pos:(1,1) (Var $1),$2)) }
  | VARLBRA expr RBRA                { mk (App (mk ~pos:(1,1) (Var "_[_]"),
                                           ["",$2;"",mk ~pos:(1,1) (Var $1)])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { mk_fun $3 $6 }
  | LCUR exprs RCUR                  { mk_fun [] $2 }
  | IF exprs THEN exprs if_elsif END
                                     { let cond = $2 in
                                       let then_b =
                                         mk_fun ~pos:(3,4) [] $4
                                       in
                                       let else_b = $5 in
                                       let op = mk ~pos:(1,1) (Var "if") in
                                         mk (App (op,["",cond;
                                                      "else",else_b;
                                                      "then",then_b])) }
  | expr BIN0 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | expr BIN1 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | expr BIN2 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | expr BIN3 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | expr TIMES expr                { mk (App (mk ~pos:(2,2) (Var "*"),
                                                ["",$1;"",$3])) }
  | expr MINUS expr                { mk (App (mk ~pos:(2,2) (Var "-"),
                                                ["",$1;"",$3])) }
  | INTERVAL                       { mk_time_pred (between (fst $1) (snd $1)) }
  | TIME                           { mk_time_pred (during $1) }

ty:
  | VAR                       { mk_ty $1 [] }
  | VARLPAR ty_args RPAR      { mk_ty $1 $2 }
  | LBRA ty RBRA              { Lang_types.make (Lang_types.List $2) }
  | LPAR ty TIMES ty RPAR     { Lang_types.make (Lang_types.Product ($2,$4)) }
  | INT                       { Lang_values.type_of_int $1 }
  | TIMES                     { Lang_values.variable_t }
  /* | TIMES PLUS INT            { Lang_values.variable_t + type_of_int $2 } */

ty_args:
  |                      { [] }
  | ty_arg               { [$1] }
  | ty_arg COMMA ty_args { $1::$3 }

ty_arg:
  | ty { "",$1 }
  | VAR GETS ty { $1,$3 }

/* An expression,
 * in a restricted form that can be concenated without ambiguity */
cexpr:
  | LPAR expr RPAR                   { $2 }
  | LPAR expr COLON ty RPAR          { Lang_types.(<:) $2.Lang_values.t $4 ;
                                       $2 }
  | INT                              { mk (Int $1) }
  | NOT expr                         { mk (App (mk ~pos:(1,1) (Var "not"),
                                                ["", $2])) }
  | BOOL                             { mk (Bool $1) }
  | FLOAT                            { mk (Float  $1) }
  | STRING                           { mk (String $1) }
  | list                             { mk (List $1) }
  | REF expr                         { mk (Ref $2) }
  | GET expr                         { mk (Get $2) }
  | cexpr SET expr                   { mk (Set ($1,$3)) }
  | MP3 app_opt                      { mk_mp3 $2 }
  | FLAC app_opt                     { mk_flac $2 }
  | AACPLUS app_opt                  { mk_aacplus $2 }
  | VOAACENC app_opt                  { mk_voaacenc $2 }
  | EXTERNAL app_opt                 { mk_external $2 }
  | WAV app_opt                      { mk_wav $2 }
  | OGG LPAR ogg_items RPAR          { mk (Encoder (Encoder.Ogg $3)) }
  | top_level_ogg_item               { mk (Encoder (Encoder.Ogg [$1])) }
  | LPAR RPAR                        { mk Unit }
  | LPAR expr COMMA expr RPAR        { mk (Product ($2,$4)) }
  | VAR                              { mk (Var $1) }
  | VARLPAR app_list RPAR            { mk (App (mk ~pos:(1,1) (Var $1),$2)) }
  | VARLBRA expr RBRA                { mk (App (mk ~pos:(1,1) (Var "_[_]"),
                                           ["",$2;"",mk ~pos:(1,1) (Var $1)])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { mk_fun $3 $6 }
  | LCUR exprs RCUR                  { mk_fun [] $2 }
  | IF exprs THEN exprs if_elsif END
                                     { let cond = $2 in
                                       let then_b =
                                         mk_fun ~pos:(3,4) [] $4
                                       in
                                       let else_b = $5 in
                                       let op = mk ~pos:(1,1) (Var "if") in
                                         mk (App (op,["",cond;
                                                      "else",else_b;
                                                      "then",then_b])) }
  | cexpr BIN0 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | cexpr BIN1 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | cexpr BIN2 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | cexpr BIN3 expr                 { mk (App (mk ~pos:(2,2) (Var $2),
                                                ["",$1;"",$3])) }
  | cexpr TIMES expr                { mk (App (mk ~pos:(2,2) (Var "*"),
                                                ["",$1;"",$3])) }
  | cexpr MINUS expr                { mk (App (mk ~pos:(2,2) (Var "-"),
                                                ["",$1;"",$3])) }
  | INTERVAL                       { mk_time_pred (between (fst $1) (snd $1)) }
  | TIME                           { mk_time_pred (during $1) }

list:
  | LBRA inner_list RBRA { $2 }
inner_list:
  | expr COMMA inner_list  { $1::$3 }
  | expr                   { [$1] }
  |                        { [] }

app_list_elem:
  | VAR GETS expr { $1,$3 }
  | expr          { "",$1 }
/* Note that we can get rid of the COMMA iff we use cexpr instead of expr
 * for unlabelled parameters. */
app_list:
  |                              { [] }
  | app_list_elem                { [$1] }
  | app_list_elem COMMA app_list { $1::$3 }

binding:
  | VAR GETS expr {
       let body = $3 in
         (Doc.none (),[]),$1,body
    }
  | DEF VAR g exprs END {
      let body = $4 in
        $1,$2,body
    }
  | DEF VARLPAR arglist RPAR g exprs END {
      let arglist = $3 in
      let body = mk_fun arglist $6 in
        $1,$2,body
    }

arglist:
  |                   { [] }
  | arg               { [$1] }
  | arg COMMA arglist { $1::$3 }
arg:
  | TILD VAR opt { $2,$2,
                   T.fresh_evar ~level:(-1) ~pos:(Some (curpos ~pos:(2,2) ())),
                   $3 }
  | VAR opt      { "",$1,
                   T.fresh_evar ~level:(-1) ~pos:(Some (curpos ~pos:(1,1) ())),
                   $2 }
opt:
  | GETS expr { Some $2 }
  |           { None }

if_elsif:
  | ELSIF exprs THEN exprs if_elsif { let cond = $2 in
                                      let then_b =
                                        mk_fun ~pos:(3,4) [] $4
                                      in
                                      let else_b = $5 in
                                      let op = mk ~pos:(1,1) (Var "if") in
                                        mk_fun []
                                          (mk (App (op,["",cond;
                                                        "else",else_b;
                                                        "then",then_b]))) }
  | ELSE exprs                      { mk_fun ~pos:(1,2) [] $2 }
  |                                 { mk_fun [] (mk Unit) }

app_opt:
  | %prec no_app { [] }
  | LPAR app_list RPAR { $2 }

ogg_items:
  | ogg_item { [$1] }
  | ogg_item COMMA ogg_items { $1::$3 }
top_level_ogg_item:
  | VORBIS app_opt     { mk_vorbis $2 }
  | VORBIS_CBR app_opt { mk_vorbis_cbr $2 }
  | VORBIS_ABR app_opt { mk_vorbis_abr $2 }
  | THEORA app_opt     { mk_theora $2 }
  | DIRAC app_opt      { mk_dirac $2 }
  | SPEEX app_opt      { mk_speex $2 }
ogg_item:
  | FLAC app_opt   { mk_ogg_flac $2 }
  | top_level_ogg_item { $1 }
