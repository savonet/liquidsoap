/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
      mk (App (mk (Var "time_in_mod"), args))

  let mk_wav params =
    let defaults = { Encoder.WAV.stereo = true } in
    let wav =
      List.fold_left
        (fun f ->
          function
            | ("stereo",{ term = Bool b }) ->
                { Encoder.WAV.stereo = b }
            | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
                { Encoder.WAV.stereo = true }
            | ("",{ term = Var s }) when String.lowercase s = "mono" ->
                { Encoder.WAV.stereo = false }
            | _ -> raise Parsing.Parse_error)
        defaults params
    in
      mk (Encoder (Encoder.WAV wav))

  let mk_mp3 params =
    let defaults =
      { Encoder.MP3.
          stereo = true ;
          samplerate = 44100 ;
          bitrate = 128 ;
          quality = 5 }
    in
    let mp3 =
      List.fold_left
        (fun f ->
          function
            | ("stereo",{ term = Bool b }) ->
                { f with Encoder.MP3.stereo = b }
            | ("samplerate",{ term = Int i }) ->
                { f with Encoder.MP3.samplerate = i }
            | ("bitrate",{ term = Int i }) ->
                { f with Encoder.MP3.bitrate = i }
            | ("quality",{ term = Int q }) ->
                { f with Encoder.MP3.quality = q }

            | ("",{ term = Var s }) when String.lowercase s = "mono" ->
                { f with Encoder.MP3.stereo = false }
            | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
                { f with Encoder.MP3.stereo = true }

            | _ -> raise Parsing.Parse_error)
        defaults params
    in
      mk (Encoder (Encoder.MP3 mp3))

  let mk_vorbis params =
    let defaults =
      { Encoder.Vorbis.
          mode = Encoder.Vorbis.VBR ;
          stereo = true ;
          samplerate = 44100 ;
          quality = 2. ;
          skeleton = false }
    in
    let vorbis =
      List.fold_left
        (fun f ->
          function
            | ("stereo",{ term = Bool b }) ->
                { f with Encoder.Vorbis.stereo = b }
            | ("skeleton",{ term = Bool b }) ->
                { f with Encoder.Vorbis.skeleton = b }
            | ("samplerate",{ term = Int i }) ->
                { f with Encoder.Vorbis.samplerate = i }
            | ("quality",{ term = Float q }) ->
                { f with Encoder.Vorbis.quality = q }
            | ("quality",{ term = Int i }) ->
                { f with Encoder.Vorbis.quality = float_of_int i }

            | ("",{ term = Var "skeleton" }) ->
                { f with Encoder.Vorbis.skeleton = true }
            | ("",{ term = Var s }) when String.lowercase s = "mono" ->
                { f with Encoder.Vorbis.stereo = false }
            | ("",{ term = Var s }) when String.lowercase s = "stereo" ->
                { f with Encoder.Vorbis.stereo = true }

            | ("mode",{ term = Var s })
            | ("",{ term = Var s }) when String.lowercase s = "vbr" ->
                { f with Encoder.Vorbis.mode = Encoder.Vorbis.VBR }
            | ("mode",{ term = Var s })
            | ("",{ term = Var s }) when String.lowercase s = "cbr" ->
                { f with Encoder.Vorbis.mode = Encoder.Vorbis.CBR }
            | ("mode",{ term = Var s })
            | ("",{ term = Var s }) when String.lowercase s = "abr" ->
                { f with Encoder.Vorbis.mode = Encoder.Vorbis.ABR }

            | _ -> raise Parsing.Parse_error)
        defaults params
    in
      Encoder.Ogg.Vorbis vorbis

  let mk_theora params =
    let defaults = { Encoder.Theora.quality = 100 } in
    let theora =
      List.fold_left
        (fun f ->
          function
            | ("quality",{ term = Int i }) ->
                { (* f with *) Encoder.Theora.quality = i }
            | ("",{ term = Int i }) ->
                { (* f with *) Encoder.Theora.quality = i }
            | _ -> raise Parsing.Parse_error)
        defaults params
    in
      Encoder.Ogg.Theora theora

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
%token OGG VORBIS THEORA WAV MP3
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

%left YIELDS
%left BIN0
%left BIN1
%left BIN2 MINUS NOT
%left BIN3
%left unary_minus

%start scheduler
%type <Lang_values.term> scheduler

%%

scheduler: exprs EOF { $1 }

s: | {} | SEQ  {}
g: | {} | GETS {}

/* A sequence of (concatenable) expressions and bindings,
 * separated by optional semicolon */
exprs:
  | cexpr s                  { $1 }
  | cexpr s exprs            { mk (Seq ($1,$3)) }
  | binding s                { let doc,name,def = $1 in
                                 mk (Let (doc,name,def,mk Unit)) }
  | binding s exprs          { let doc,name,def = $1 in
                                 mk (Let (doc,name,def,$3)) }

/* General expressions.
 * The only difference is the ability to start with an unary MINUS.
 * But having two rules, one coercion and one for MINUS expr, would
 * be wrong: we want to parse -3*2 as (-3)*2. */
expr:
  | MINUS expr %prec unary_minus     { mk (App (mk ~pos:(1,1) (Var "~-"),
                                                ["", $2])) }
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk (Int $1) }
  | NOT expr                         { mk (App (mk ~pos:(1,1) (Var "not"),
                                                ["", $2])) }
  | BOOL                             { mk (Bool $1) }
  | FLOAT                            { mk (Float  $1) }
  | STRING                           { mk (String $1) }
  | list                             { mk (List $1) }
  | MP3 app_opt                      { mk_mp3 $2 }
  | WAV app_opt                      { mk_wav $2 }
  | OGG LPAR ogg_items RPAR          { mk (Encoder (Encoder.Ogg $3)) }
  | ogg_item                         { mk (Encoder (Encoder.Ogg [$1])) }
  | LPAR RPAR                        { mk Unit }
  | LPAR expr COMMA expr RPAR        { mk (Product ($2,$4)) }
  | VAR                              { mk (Var $1) }
  | VARLPAR app_list RPAR            { mk (App (mk ~pos:(1,1) (Var $1),$2)) }
  | VARLBRA expr RBRA                { mk (App (mk ~pos:(1,1) (Var "_[_]"),
                                           ["",$2;"",mk ~pos:(1,1) (Var $1)])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { mk (Fun ($3,$6)) }
  | LCUR exprs RCUR                  { mk (Fun ([],$2)) }
  | IF exprs THEN exprs if_elsif END
                                     { let cond = $2 in
                                       let then_b =
                                         mk ~pos:(3,4) (Fun ([],$4))
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
  | expr MINUS expr                { mk (App (mk ~pos:(2,2) (Var "-"),
                                                ["",$1;"",$3])) }
  | INTERVAL                       { mk_time_pred (between (fst $1) (snd $1)) }
  | TIME                           { mk_time_pred (during $1) }

/* An expression,
 * in a restricted form that can be concenated without ambiguity */
cexpr:
  | LPAR expr RPAR                   { $2 }
  | INT                              { mk (Int $1) }
  | NOT expr                         { mk (App (mk ~pos:(1,1) (Var "not"),
                                                ["", $2])) }
  | BOOL                             { mk (Bool $1) }
  | FLOAT                            { mk (Float  $1) }
  | STRING                           { mk (String $1) }
  | list                             { mk (List $1) }
  | MP3 app_opt                      { mk_mp3 $2 }
  | WAV app_opt                      { mk_wav $2 }
  | OGG LPAR ogg_items RPAR          { mk (Encoder (Encoder.Ogg $3)) }
  | ogg_item                         { mk (Encoder (Encoder.Ogg [$1])) }
  | LPAR RPAR                        { mk Unit }
  | LPAR expr COMMA expr RPAR        { mk (Product ($2,$4)) }
  | VAR                              { mk (Var $1) }
  | VARLPAR app_list RPAR            { mk (App (mk ~pos:(1,1) (Var $1),$2)) }
  | VARLBRA expr RBRA                { mk (App (mk ~pos:(1,1) (Var "_[_]"),
                                           ["",$2;"",mk ~pos:(1,1) (Var $1)])) }
  | BEGIN exprs END                  { $2 }
  | FUN LPAR arglist RPAR YIELDS expr
                                     { mk (Fun ($3,$6)) }
  | LCUR exprs RCUR                  { mk (Fun ([],$2)) }
  | IF exprs THEN exprs if_elsif END
                                     { let cond = $2 in
                                       let then_b =
                                         mk ~pos:(3,4) (Fun ([],$4))
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
      let body = mk (Fun (arglist,$6)) in
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
                                        mk ~pos:(3,4) (Fun ([],$4))
                                      in
                                      let else_b = $5 in
                                      let op = mk ~pos:(1,1) (Var "if") in
                                        mk (Fun ([],
                                          mk (App (op,["",cond;
                                                       "else",else_b;
                                                       "then",then_b])))) }
  | ELSE exprs                      { mk ~pos:(1,2) (Fun([],$2)) }
  |                                 { mk (Fun ([],mk Unit)) }

app_opt:
  | { [] }
  | LPAR app_list RPAR { $2 }

ogg_items:
  | ogg_item { [$1] }
  | ogg_item COMMA ogg_items { $1::$3 }
ogg_item:
  | VORBIS app_opt { mk_vorbis $2 }
  | THEORA app_opt { mk_theora $2 }
