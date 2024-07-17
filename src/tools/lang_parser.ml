type token =
  | VAR of (string)
  | VARLPAR of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | TIME of (string list)
  | EOF
  | SET
  | DEF
  | BEGIN
  | END
  | GETS
  | TILD
  | LPAR
  | RPAR
  | COMMA
  | SEQ
  | LBRA
  | RBRA
  | LCUR
  | RCUR
  | FUNLPAR
  | YIELDS
  | CONCAT
  | MINUS
  | AND
  | OR

open Parsing;;
# 24 "tools/lang_parser.mly"

  open Types
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

  let bindings : (string * value) list ref = ref []

  let init_bindings () =
    bindings := [] ;
    Lang.builtins#iter
      (fun name value -> bindings := (name,value)::!bindings)

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
    let to_int s =
      if s="" then 0 else
        int_of_string (String.sub s 0 (String.length s - 1))
    in
    let rec aux = function
      | ""::tl -> aux tl
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

  let precision d = time_units.(last_index "" 0 d)
  let duration d = time_units.(Array.length time_units - 1 - 
                               last_index "" 0 (List.rev d))

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

# 128 "tools/lang_parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  264 (* SET *);
  265 (* DEF *);
  266 (* BEGIN *);
  267 (* END *);
  268 (* GETS *);
  269 (* TILD *);
  270 (* LPAR *);
  271 (* RPAR *);
  272 (* COMMA *);
  273 (* SEQ *);
  274 (* LBRA *);
  275 (* RBRA *);
  276 (* LCUR *);
  277 (* RCUR *);
  278 (* FUNLPAR *);
  279 (* YIELDS *);
  280 (* CONCAT *);
  281 (* MINUS *);
  282 (* AND *);
  283 (* OR *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* VARLPAR *);
  259 (* STRING *);
  260 (* INT *);
  261 (* FLOAT *);
  262 (* BOOL *);
  263 (* TIME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\006\000\006\000\007\000\007\000\008\000\008\000\
\003\000\003\000\003\000\003\000\011\000\012\000\012\000\012\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\015\000\015\000\013\000\013\000\013\000\010\000\
\010\000\010\000\014\000\014\000\014\000\016\000\016\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\004\000\004\000\004\000\004\000\004\000\
\002\000\003\000\001\000\003\000\000\000\001\000\000\000\001\000\
\002\000\003\000\002\000\003\000\003\000\003\000\001\000\000\000\
\001\000\001\000\001\000\001\000\001\000\002\000\005\000\001\000\
\003\000\003\000\005\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\001\000\000\000\001\000\003\000\003\000\
\005\000\007\000\000\000\001\000\003\000\003\000\002\000\002\000\
\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\058\000\000\000\000\000\000\000\000\000\
\000\000\028\000\025\000\027\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\014\000\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\006\000\007\000\
\000\000\008\000\000\000\000\000\033\000\000\000\041\000\016\000\
\000\000\000\000\034\000\040\000\000\000\000\000\021\000\036\000\
\000\000\055\000\000\000\000\000\000\000\000\000\000\000\038\000\
\018\000\020\000\000\000\009\000\000\000\000\000\047\000\000\000\
\000\000\000\000\022\000\000\000\054\000\000\000\053\000\000\000\
\010\000\049\000\000\000\031\000\000\000\012\000\000\000\050\000"

let yydgoto = "\002\000\
\004\000\005\000\021\000\006\000\058\000\085\000\051\000\065\000\
\022\000\023\000\024\000\040\000\030\000\044\000\031\000\045\000\
\074\000"

let yysindex = "\016\000\
\008\255\000\000\018\255\000\000\064\255\008\255\015\255\022\255\
\159\255\000\000\000\000\000\000\000\000\013\255\010\255\064\255\
\093\255\181\255\064\255\001\255\030\000\009\255\035\255\000\000\
\000\000\019\255\181\255\032\255\030\255\043\255\034\255\065\255\
\068\255\001\255\066\255\000\000\000\000\178\255\063\255\069\255\
\062\255\079\255\091\255\086\255\088\255\000\000\000\000\181\255\
\181\255\181\255\064\255\064\255\000\000\000\000\000\000\000\000\
\002\255\000\000\030\255\181\255\000\000\159\255\000\000\000\000\
\064\255\087\255\000\000\000\000\181\255\181\255\000\000\000\000\
\181\255\000\000\079\255\083\255\001\255\236\254\082\255\000\000\
\000\000\000\000\094\255\000\000\095\255\030\255\000\000\101\255\
\068\255\005\255\000\000\030\255\000\000\181\255\000\000\120\255\
\000\000\000\000\064\255\000\000\030\255\000\000\117\255\000\000"

let yyrindex = "\000\000\
\115\255\000\000\000\000\000\000\000\000\115\255\000\000\028\000\
\111\255\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\112\255\000\000\119\255\000\000\149\000\149\000\000\000\
\000\000\000\000\000\000\182\255\027\255\000\000\121\255\000\000\
\137\255\119\255\000\000\000\000\000\000\000\000\113\255\000\000\
\000\000\033\255\000\000\000\000\130\255\000\000\000\000\000\000\
\000\000\000\000\003\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\127\000\000\000\000\000\111\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\112\255\000\000\000\000\
\000\000\000\000\033\255\000\000\119\255\081\000\055\000\000\000\
\000\000\000\000\129\255\000\000\000\000\047\255\000\000\000\000\
\137\255\000\000\000\000\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\105\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\124\000\250\255\000\000\000\000\054\000\129\000\064\000\
\247\255\000\000\000\000\084\000\094\000\226\255\000\000\000\000\
\083\000"

let yytablesize = 427
let yytable = "\029\000\
\042\000\042\000\017\000\066\000\083\000\049\000\050\000\038\000\
\039\000\035\000\033\000\034\000\041\000\043\000\019\000\003\000\
\001\000\059\000\007\000\100\000\084\000\053\000\054\000\055\000\
\056\000\047\000\026\000\032\000\048\000\046\000\049\000\050\000\
\048\000\027\000\049\000\050\000\057\000\032\000\078\000\079\000\
\080\000\044\000\044\000\060\000\081\000\082\000\095\000\057\000\
\057\000\062\000\086\000\047\000\029\000\048\000\037\000\049\000\
\050\000\061\000\088\000\090\000\039\000\043\000\043\000\092\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\063\000\
\015\000\016\000\056\000\056\000\067\000\017\000\070\000\064\000\
\039\000\018\000\072\000\019\000\101\000\020\000\048\000\071\000\
\049\000\050\000\073\000\075\000\103\000\036\000\009\000\010\000\
\011\000\012\000\013\000\014\000\076\000\089\000\016\000\077\000\
\035\000\094\000\017\000\037\000\050\000\096\000\018\000\098\000\
\019\000\097\000\020\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\083\000\002\000\002\000\045\000\048\000\104\000\
\002\000\025\000\024\000\023\000\002\000\051\000\002\000\046\000\
\002\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\052\000\015\000\015\000\011\000\013\000\102\000\015\000\052\000\
\099\000\091\000\015\000\087\000\015\000\093\000\015\000\028\000\
\009\000\010\000\011\000\012\000\013\000\014\000\000\000\000\000\
\016\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
\018\000\000\000\019\000\000\000\020\000\036\000\009\000\010\000\
\011\000\012\000\013\000\014\000\000\000\000\000\016\000\000\000\
\068\000\069\000\017\000\000\000\032\000\032\000\018\000\000\000\
\019\000\048\000\020\000\049\000\050\000\032\000\000\000\032\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\000\000\042\000\042\000\042\000\000\000\017\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\017\000\
\042\000\019\000\042\000\042\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\019\000\032\000\032\000\032\000\000\000\
\000\000\032\000\000\000\000\000\032\000\032\000\000\000\032\000\
\032\000\032\000\000\000\032\000\000\000\032\000\032\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\000\000\037\000\
\037\000\037\000\000\000\000\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\000\000\037\000\000\000\
\037\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\000\000\039\000\039\000\039\000\000\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\000\000\
\039\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\000\000\035\000\035\000\035\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\000\000\048\000\
\048\000\048\000\000\000\000\000\048\000\000\000\000\000\048\000\
\048\000\000\000\048\000\048\000\048\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\000\000\013\000\013\000\013\000\
\000\000\000\000\013\000\000\000\000\000\000\000\013\000\000\000\
\013\000\013\000\013\000"

let yycheck = "\009\000\
\000\000\001\001\000\000\034\000\003\001\026\001\027\001\017\000\
\018\000\016\000\001\001\002\001\019\000\013\001\000\000\008\001\
\001\000\027\000\001\001\015\001\019\001\003\001\004\001\005\001\
\006\001\017\001\012\001\000\000\024\001\000\000\026\001\027\001\
\024\001\012\001\026\001\027\001\018\001\025\001\048\000\049\000\
\050\000\015\001\016\001\012\001\051\000\052\000\077\000\015\001\
\016\001\016\001\060\000\017\001\062\000\024\001\000\000\026\001\
\027\001\015\001\065\000\069\000\070\000\015\001\016\001\073\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\007\001\
\009\001\010\001\015\001\016\001\011\001\014\001\016\001\012\001\
\000\000\018\001\021\001\020\001\094\000\022\001\024\001\019\001\
\026\001\027\001\012\001\001\001\099\000\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\015\001\015\001\010\001\016\001\
\000\000\023\001\014\001\015\001\027\001\016\001\018\001\011\001\
\020\001\019\001\022\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\003\001\009\001\010\001\015\001\000\000\011\001\
\014\001\006\000\019\001\019\001\018\001\015\001\020\001\015\001\
\022\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\015\001\009\001\010\001\019\001\000\000\096\000\014\001\023\000\
\089\000\070\000\018\001\062\000\020\001\075\000\022\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\255\255\
\010\001\255\255\255\255\255\255\014\001\255\255\255\255\255\255\
\018\001\255\255\020\001\255\255\022\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\255\255\255\255\010\001\255\255\
\015\001\016\001\014\001\255\255\015\001\016\001\018\001\255\255\
\020\001\024\001\022\001\026\001\027\001\024\001\255\255\026\001\
\027\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\255\255\011\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\021\001\
\024\001\011\001\026\001\027\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\021\001\009\001\010\001\011\001\255\255\
\255\255\014\001\255\255\255\255\017\001\018\001\255\255\020\001\
\021\001\022\001\255\255\024\001\255\255\026\001\027\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\255\255\
\026\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\255\255\255\255\014\001\255\255\255\255\017\001\
\018\001\255\255\020\001\021\001\022\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\255\255\009\001\010\001\011\001\
\255\255\255\255\014\001\255\255\255\255\255\255\018\001\255\255\
\020\001\021\001\022\001"

let yynames_const = "\
  EOF\000\
  SET\000\
  DEF\000\
  BEGIN\000\
  END\000\
  GETS\000\
  TILD\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SEQ\000\
  LBRA\000\
  RBRA\000\
  LCUR\000\
  RCUR\000\
  FUNLPAR\000\
  YIELDS\000\
  CONCAT\000\
  MINUS\000\
  AND\000\
  OR\000\
  "

let yynames_block = "\
  VAR\000\
  VARLPAR\000\
  STRING\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  TIME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'settings) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 145 "tools/lang_parser.mly"
                       ( _2 )
# 393 "tools/lang_parser.ml"
               : Lang.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "tools/lang_parser.mly"
    ( (* Once the settings are done, initialize the binding stack. *)
      init_bindings () )
# 400 "tools/lang_parser.ml"
               : 'settings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'settings_base) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'settings) in
    Obj.repr(
# 150 "tools/lang_parser.mly"
                           ( )
# 408 "tools/lang_parser.ml"
               : 'settings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_string _2 _4 )
# 416 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 153 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_int    _2 _4 )
# 424 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 154 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_float  _2 _4 )
# 432 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 155 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_bool   _2 _4 )
# 440 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 156 "tools/lang_parser.mly"
                             ( Dtools.Conf.set_list   _2 _4 )
# 448 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "tools/lang_parser.mly"
                                     ( [] )
# 454 "tools/lang_parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inner_string_list) in
    Obj.repr(
# 160 "tools/lang_parser.mly"
                                     ( _2 )
# 461 "tools/lang_parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 163 "tools/lang_parser.mly"
                                     ( [_1] )
# 468 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inner_string_list) in
    Obj.repr(
# 164 "tools/lang_parser.mly"
                                       ( _1::_3 )
# 476 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 169 "tools/lang_parser.mly"
     ()
# 482 "tools/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 169 "tools/lang_parser.mly"
               ()
# 488 "tools/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "tools/lang_parser.mly"
     ()
# 494 "tools/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "tools/lang_parser.mly"
               ()
# 500 "tools/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 173 "tools/lang_parser.mly"
                             ( _1 )
# 508 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 174 "tools/lang_parser.mly"
                             ( mk (Lang.Seq (_1,_3)) )
# 517 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 175 "tools/lang_parser.mly"
                             ( let name,def = _1 in
                                 pop () ;
                                 mk ~kind:Lang.unit_t
                                   (Lang.Let (name,def,mk Lang.Unit)) )
# 528 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 179 "tools/lang_parser.mly"
                             ( let name,def = _1 in
                                 pop () ;
                                 mk ~kind:_3.kind
                                   (Lang.Let (name,def,_3)) )
# 540 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inner_list) in
    Obj.repr(
# 185 "tools/lang_parser.mly"
                         ( _2 )
# 547 "tools/lang_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inner_list) in
    Obj.repr(
# 187 "tools/lang_parser.mly"
                           ( _1::_3 )
# 555 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "tools/lang_parser.mly"
                           ( [_1] )
# 562 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "tools/lang_parser.mly"
                           ( [] )
# 568 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 192 "tools/lang_parser.mly"
                                     ( mk (Lang.Int _1) )
# 575 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 193 "tools/lang_parser.mly"
                                     ( mk (Lang.Bool _1) )
# 582 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 194 "tools/lang_parser.mly"
                                     ( mk (Lang.Float  _1) )
# 589 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "tools/lang_parser.mly"
                                     ( mk (Lang.String _1) )
# 596 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 196 "tools/lang_parser.mly"
                                     ( mk (Lang.List _1) )
# 603 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "tools/lang_parser.mly"
                                     ( mk Lang.Unit )
# 609 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 198 "tools/lang_parser.mly"
                                     ( mk (Lang.Product (_2,_4)) )
# 617 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "tools/lang_parser.mly"
                                     ( lookup _1 )
# 624 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'app_list) in
    Obj.repr(
# 200 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _1,_2)) )
# 632 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 201 "tools/lang_parser.mly"
                                     ( _2 )
# 639 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'arglist) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "tools/lang_parser.mly"
                                     ( let arglist = _2 in
                                         popn (List.length arglist) ;
                                         mk (Lang.Fun (arglist,_5)) )
# 649 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 205 "tools/lang_parser.mly"
                                     ( mk (Lang.Fun ([],_2)) )
# 656 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 206 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "and",
                                                     ["",_1;"",_3])) )
# 665 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "or",
                                                     ["",_1;"",_3])) )
# 674 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 210 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "^",
                                                     ["",_1;"",_3])) )
# 683 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 212 "tools/lang_parser.mly"
                   ( _2 )
# 690 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 213 "tools/lang_parser.mly"
                                     ( mk_time_pred (between _1 _3) )
# 698 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 214 "tools/lang_parser.mly"
                                     ( mk_time_pred (during _1) )
# 705 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 217 "tools/lang_parser.mly"
                  ( _1,_3 )
# 713 "tools/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 218 "tools/lang_parser.mly"
                  ( "",_1 )
# 720 "tools/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    Obj.repr(
# 220 "tools/lang_parser.mly"
                                   ( [] )
# 726 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_list_elem) in
    Obj.repr(
# 221 "tools/lang_parser.mly"
                                  ( [_1] )
# 733 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'app_list_elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_list) in
    Obj.repr(
# 222 "tools/lang_parser.mly"
                                 ( _1::_3 )
# 741 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 225 "tools/lang_parser.mly"
                  (
       let body = _3 in
         bind ~kind:body.kind _1 ;
         _1,body
    )
# 753 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 230 "tools/lang_parser.mly"
                        (
      let body = _4 in
        bind ~kind:body.kind _2 ;
        _2,body
    )
# 766 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'arglist) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 235 "tools/lang_parser.mly"
                                         (
      let arglist = _3 in
      let body = mk (Lang.Fun (arglist,_6)) in
        popn (List.length arglist) ;
        bind ~kind:body.kind _2 ;
        _2,body
    )
# 782 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    Obj.repr(
# 244 "tools/lang_parser.mly"
                      ( [] )
# 788 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 245 "tools/lang_parser.mly"
                      ( [_1] )
# 795 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arglist) in
    Obj.repr(
# 246 "tools/lang_parser.mly"
                      ( _1::_3 )
# 803 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 248 "tools/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _2 ;
                     _2,_2,kind,_3 )
# 813 "tools/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 251 "tools/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _1 ;
                     "",_1,kind,_2 )
# 823 "tools/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 255 "tools/lang_parser.mly"
              ( Some _2 )
# 830 "tools/lang_parser.ml"
               : 'opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 256 "tools/lang_parser.mly"
              ( None )
# 836 "tools/lang_parser.ml"
               : 'opt))
(* Entry scheduler *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let scheduler (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lang.value)
