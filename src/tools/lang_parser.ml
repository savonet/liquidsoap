type token =
  | ROOT
  | VAR of (string)
  | OP of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | LABEL of (string)
  | EOF
  | COMMA
  | SET
  | LET
  | GETS
  | IN
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | SEP

open Parsing;;
# 3 "tools/lang_parser.mly"

  open Types
  open Lang

  let symbols = Hashtbl.create 10

  let bind var sym = 
    Hashtbl.add symbols var sym

  let resolve var = 
    try
      Hashtbl.find symbols var
    with
      | Not_found -> raise (Lang.Unbound var)

  let nolabelize l =
    let nolabel i =
      Printf.sprintf "arg_%d" i
    in
      fst (List.fold_left (fun (l,i) p -> (nolabel i,p)::l, i+1) ([],1) l)

# 46 "tools/lang_parser.ml"
let yytransl_const = [|
  257 (* ROOT *);
    0 (* EOF *);
  265 (* COMMA *);
  266 (* SET *);
  267 (* LET *);
  268 (* GETS *);
  269 (* IN *);
  270 (* LPAR *);
  271 (* RPAR *);
  272 (* LBRA *);
  273 (* RBRA *);
  274 (* SEP *);
    0|]

let yytransl_block = [|
  258 (* VAR *);
  259 (* OP *);
  260 (* STRING *);
  261 (* INT *);
  262 (* FLOAT *);
  263 (* BOOL *);
  264 (* LABEL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\006\000\006\000\003\000\003\000\003\000\007\000\
\009\000\009\000\010\000\010\000\008\000\008\000\012\000\012\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\004\000\004\000\004\000\004\000\004\000\
\002\000\003\000\001\000\003\000\004\000\002\000\001\000\003\000\
\002\000\003\000\003\000\001\000\003\000\001\000\000\000\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\015\000\
\000\000\000\000\000\000\003\000\000\000\030\000\031\000\028\000\
\025\000\027\000\026\000\000\000\000\000\000\000\014\000\029\000\
\000\000\022\000\000\000\000\000\001\000\004\000\005\000\006\000\
\007\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\024\000\000\000\000\000\000\000\009\000\
\000\000\021\000\032\000\000\000\018\000\000\000\016\000\013\000\
\000\000\010\000\000\000\019\000\012\000\033\000"

let yydgoto = "\002\000\
\004\000\005\000\011\000\006\000\035\000\049\000\028\000\023\000\
\024\000\042\000\025\000\026\000"

let yysindex = "\015\000\
\008\255\000\000\031\255\000\000\060\255\008\255\025\255\000\000\
\024\255\037\255\047\000\000\000\007\255\000\000\000\000\000\000\
\000\000\000\000\000\000\054\255\039\255\003\255\000\000\000\000\
\054\255\000\000\036\255\038\255\000\000\000\000\000\000\000\000\
\000\000\018\255\000\000\024\255\000\000\024\255\034\255\043\255\
\000\000\050\255\051\255\000\000\060\255\060\255\056\255\000\000\
\058\255\000\000\000\000\054\255\000\000\054\255\000\000\000\000\
\072\255\000\000\063\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\062\255\000\000\000\000\000\000\000\000\062\255\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\006\255\057\255\000\000\000\000\
\000\000\000\000\064\255\000\000\000\000\000\000\065\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\048\000\235\255\000\000\000\000\020\000\000\000\043\000\
\000\000\026\000\238\255\058\000"

let yytablesize = 272
let yytable = "\039\000\
\023\000\036\000\040\000\043\000\014\000\015\000\016\000\017\000\
\018\000\019\000\030\000\031\000\032\000\033\000\030\000\001\000\
\021\000\003\000\022\000\041\000\015\000\047\000\034\000\055\000\
\056\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\007\000\059\000\048\000\043\000\013\000\021\000\027\000\022\000\
\037\000\038\000\016\000\017\000\018\000\019\000\029\000\045\000\
\051\000\010\000\046\000\052\000\021\000\012\000\022\000\014\000\
\015\000\016\000\017\000\018\000\019\000\008\000\009\000\002\000\
\002\000\031\000\053\000\021\000\054\000\022\000\010\000\023\000\
\002\000\057\000\058\000\047\000\061\000\062\000\050\000\060\000\
\020\000\011\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\023\000"

let yycheck = "\021\000\
\000\000\020\000\021\000\022\000\002\001\003\001\004\001\005\001\
\006\001\007\001\004\001\005\001\006\001\007\001\009\001\001\000\
\014\001\010\001\016\001\017\001\015\001\004\001\016\001\045\000\
\046\000\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\002\001\052\000\017\001\054\000\012\001\014\001\002\001\016\001\
\002\001\003\001\004\001\005\001\006\001\007\001\000\000\012\001\
\015\001\011\001\013\001\009\001\014\001\006\000\016\001\002\001\
\003\001\004\001\005\001\006\001\007\001\002\001\003\001\002\001\
\003\001\009\001\017\001\014\001\018\001\016\001\011\001\015\001\
\011\001\018\001\017\001\004\001\057\000\015\001\036\000\054\000\
\017\001\017\001\025\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\015\001"

let yynames_const = "\
  ROOT\000\
  EOF\000\
  COMMA\000\
  SET\000\
  LET\000\
  GETS\000\
  IN\000\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  SEP\000\
  "

let yynames_block = "\
  VAR\000\
  OP\000\
  STRING\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  LABEL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'settings) in
    let _2 = (peek_val parser_env 1 : 'source) in
    Obj.repr(
# 45 "tools/lang_parser.mly"
                        ( _2 )
# 230 "tools/lang_parser.ml"
               : Types.source))
; (fun parser_env ->
    Obj.repr(
# 49 "tools/lang_parser.mly"
    ( )
# 236 "tools/lang_parser.ml"
               : 'settings))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'settings_base) in
    let _2 = (peek_val parser_env 0 : 'settings) in
    Obj.repr(
# 50 "tools/lang_parser.mly"
                           ( )
# 244 "tools/lang_parser.ml"
               : 'settings))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 53 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_string _2 _4 )
# 252 "tools/lang_parser.ml"
               : 'settings_base))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 0 : int) in
    Obj.repr(
# 54 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_int _2 _4 )
# 260 "tools/lang_parser.ml"
               : 'settings_base))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 0 : float) in
    Obj.repr(
# 55 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_float _2 _4 )
# 268 "tools/lang_parser.ml"
               : 'settings_base))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 0 : bool) in
    Obj.repr(
# 56 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_bool _2 _4 )
# 276 "tools/lang_parser.ml"
               : 'settings_base))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : string) in
    let _4 = (peek_val parser_env 0 : 'string_list) in
    Obj.repr(
# 57 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_list _2 _4 )
# 284 "tools/lang_parser.ml"
               : 'settings_base))
; (fun parser_env ->
    Obj.repr(
# 60 "tools/lang_parser.mly"
                                     ( [] )
# 290 "tools/lang_parser.ml"
               : 'string_list))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'inner_string_list) in
    Obj.repr(
# 61 "tools/lang_parser.mly"
                                     ( _2 )
# 297 "tools/lang_parser.ml"
               : 'string_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 64 "tools/lang_parser.mly"
                                     ( [_1] )
# 304 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'inner_string_list) in
    Obj.repr(
# 65 "tools/lang_parser.mly"
                                     ( _1::_3 )
# 312 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'binding) in
    let _4 = (peek_val parser_env 0 : 'source) in
    Obj.repr(
# 69 "tools/lang_parser.mly"
                                     ( _4 )
# 320 "tools/lang_parser.ml"
               : 'source))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : string) in
    let _2 = (peek_val parser_env 0 : 'parameters) in
    Obj.repr(
# 70 "tools/lang_parser.mly"
                                     ( match operators#get _1 with
					 | Some f -> ((f _2):>source)
					 | None -> assert false )
# 330 "tools/lang_parser.ml"
               : 'source))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 73 "tools/lang_parser.mly"
                                     ( resolve _1 )
# 337 "tools/lang_parser.ml"
               : 'source))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'source) in
    Obj.repr(
# 76 "tools/lang_parser.mly"
                                     ( bind _1 _3 ; _3#set_name _1 )
# 345 "tools/lang_parser.ml"
               : 'binding))
; (fun parser_env ->
    Obj.repr(
# 80 "tools/lang_parser.mly"
                              ( [] )
# 351 "tools/lang_parser.ml"
               : 'list))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'inner_list) in
    Obj.repr(
# 81 "tools/lang_parser.mly"
                              ( _2 )
# 358 "tools/lang_parser.ml"
               : 'list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'parameter) in
    let _3 = (peek_val parser_env 0 : 'inner_list) in
    Obj.repr(
# 84 "tools/lang_parser.mly"
                              ( _1::_3 )
# 366 "tools/lang_parser.ml"
               : 'inner_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'parameter) in
    Obj.repr(
# 85 "tools/lang_parser.mly"
                              ( [_1] )
# 373 "tools/lang_parser.ml"
               : 'inner_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _2 = (peek_val parser_env 1 : 'parameter) in
    let _3 = (peek_val parser_env 0 : 'parameters) in
    Obj.repr(
# 89 "tools/lang_parser.mly"
                               ( (_1,_2)::_3 )
# 382 "tools/lang_parser.ml"
               : 'parameters))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'parameters_nolabel) in
    Obj.repr(
# 90 "tools/lang_parser.mly"
                               ( nolabelize _1 )
# 389 "tools/lang_parser.ml"
               : 'parameters))
; (fun parser_env ->
    Obj.repr(
# 93 "tools/lang_parser.mly"
                                 ( [] )
# 395 "tools/lang_parser.ml"
               : 'parameters_nolabel))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'parameter) in
    let _2 = (peek_val parser_env 0 : 'parameters_nolabel) in
    Obj.repr(
# 94 "tools/lang_parser.mly"
                                 ( _1::_2 )
# 403 "tools/lang_parser.ml"
               : 'parameters_nolabel))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr(
# 97 "tools/lang_parser.mly"
                     ( Lang.Int _1 )
# 410 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : bool) in
    Obj.repr(
# 98 "tools/lang_parser.mly"
                     ( Lang.Bool _1 )
# 417 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : float) in
    Obj.repr(
# 99 "tools/lang_parser.mly"
                     ( Lang.Float _1 )
# 424 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 100 "tools/lang_parser.mly"
                     ( Lang.String _1 )
# 431 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'list) in
    Obj.repr(
# 101 "tools/lang_parser.mly"
                     ( Lang.List _1 )
# 438 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 102 "tools/lang_parser.mly"
                     ( Lang.Source (resolve _1) )
# 445 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 103 "tools/lang_parser.mly"
                     ( Lang.Source (match operators#get _1 with
				      | Some f -> ((f []):>source)
				      | None -> assert false) )
# 454 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'source) in
    Obj.repr(
# 106 "tools/lang_parser.mly"
                     ( Lang.Source _2 )
# 461 "tools/lang_parser.ml"
               : 'parameter))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'parameter) in
    let _4 = (peek_val parser_env 1 : 'parameter) in
    Obj.repr(
# 107 "tools/lang_parser.mly"
                                        ( Lang.Product (_2,_4) )
# 469 "tools/lang_parser.ml"
               : 'parameter))
(* Entry scheduler *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let scheduler (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Types.source)
