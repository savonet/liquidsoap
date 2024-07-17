type token =
  | VAR of (string)
  | VARLPAR of (string)
  | VARLBRA of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | TIME of (int option list)
  | INTERVAL of (int option list * int option list)
  | EOF
  | SET
  | DEF
  | BEGIN
  | END
  | GETS
  | TILD
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | COMMA
  | SEQ
  | LBRA
  | RBRA
  | LCUR
  | RCUR
  | FUN
  | YIELDS
  | BIN0 of (string)
  | BIN1 of (string)
  | BIN2 of (string)
  | BIN3 of (string)
  | MINUS
  | NOT
  | PP_IFDEF
  | PP_ENDIF
  | PP_ENDL

open Parsing;;
# 24 "tools/lang_parser.mly"

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

# 128 "tools/lang_parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  266 (* SET *);
  267 (* DEF *);
  268 (* BEGIN *);
  269 (* END *);
  270 (* GETS *);
  271 (* TILD *);
  272 (* IF *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* LPAR *);
  276 (* RPAR *);
  277 (* COMMA *);
  278 (* SEQ *);
  279 (* LBRA *);
  280 (* RBRA *);
  281 (* LCUR *);
  282 (* RCUR *);
  283 (* FUN *);
  284 (* YIELDS *);
  289 (* MINUS *);
  290 (* NOT *);
  291 (* PP_IFDEF *);
  292 (* PP_ENDIF *);
  293 (* PP_ENDL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* VARLPAR *);
  259 (* VARLBRA *);
  260 (* STRING *);
  261 (* INT *);
  262 (* FLOAT *);
  263 (* BOOL *);
  264 (* TIME *);
  265 (* INTERVAL *);
  285 (* BIN0 *);
  286 (* BIN1 *);
  287 (* BIN2 *);
  288 (* BIN3 *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\004\000\004\000\
\005\000\005\000\006\000\006\000\007\000\007\000\008\000\008\000\
\003\000\003\000\003\000\003\000\011\000\012\000\012\000\012\000\
\013\000\013\000\013\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\016\000\016\000\014\000\014\000\014\000\
\010\000\010\000\010\000\015\000\015\000\015\000\017\000\017\000\
\018\000\018\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\004\000\004\000\004\000\004\000\004\000\
\002\000\003\000\001\000\003\000\000\000\001\000\000\000\001\000\
\002\000\003\000\002\000\003\000\003\000\003\000\001\000\000\000\
\001\000\002\000\002\000\003\000\001\000\002\000\001\000\001\000\
\001\000\001\000\002\000\005\000\001\000\003\000\003\000\003\000\
\006\000\003\000\005\000\007\000\003\000\003\000\003\000\003\000\
\003\000\001\000\001\000\003\000\001\000\000\000\001\000\003\000\
\003\000\005\000\007\000\000\000\001\000\003\000\003\000\002\000\
\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\067\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\029\000\032\000\031\000\051\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\003\000\000\000\000\000\000\000\
\000\000\000\000\053\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\005\000\006\000\007\000\000\000\
\008\000\057\000\000\000\026\000\027\000\038\000\000\000\039\000\
\016\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\028\000\000\000\021\000\000\000\042\000\000\000\000\000\000\000\
\000\000\048\000\000\000\018\000\020\000\000\000\009\000\000\000\
\052\000\056\000\000\000\000\000\064\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\010\000\058\000\065\000\
\063\000\000\000\062\000\043\000\000\000\036\000\000\000\012\000\
\000\000\000\000\041\000\059\000\044\000"

let yydgoto = "\002\000\
\004\000\005\000\025\000\006\000\065\000\096\000\058\000\074\000\
\034\000\027\000\028\000\046\000\035\000\036\000\077\000\037\000\
\078\000\101\000"

let yysindex = "\005\000\
\255\254\000\000\013\255\000\000\149\255\255\254\004\255\047\255\
\183\255\218\255\000\000\000\000\000\000\000\000\000\000\000\000\
\062\255\149\255\149\255\115\255\218\255\149\255\039\255\095\001\
\068\000\001\255\048\255\000\000\000\000\006\255\218\255\069\255\
\061\255\049\255\000\000\064\255\065\255\000\000\063\255\071\255\
\007\255\075\255\073\255\000\000\052\255\068\255\072\255\070\255\
\007\255\066\255\000\000\000\000\095\001\095\001\095\001\095\001\
\095\001\149\255\149\255\000\000\000\000\000\000\000\000\252\254\
\000\000\000\000\218\255\000\000\000\000\000\000\183\255\000\000\
\000\000\149\255\080\255\094\255\077\255\079\255\000\000\149\255\
\000\000\218\255\000\000\218\255\000\000\081\255\009\255\249\254\
\066\255\000\000\066\255\000\000\000\000\083\255\000\000\082\255\
\000\000\000\000\092\255\218\255\000\000\080\255\071\255\007\255\
\044\255\088\255\000\000\084\255\105\255\000\000\000\000\000\000\
\000\000\149\255\000\000\000\000\149\255\000\000\218\255\000\000\
\097\255\100\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\027\001\000\000\000\000\000\000\000\000\027\001\000\000\103\000\
\095\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\101\255\000\000\000\000\000\000\
\000\000\239\000\239\000\000\000\000\000\000\000\000\000\016\255\
\000\000\205\000\000\000\000\000\108\255\000\000\000\000\061\001\
\109\255\000\000\000\000\000\000\000\000\000\000\106\255\000\000\
\109\255\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\250\000\001\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\095\255\000\000\
\000\000\000\000\055\255\000\000\000\000\112\255\000\000\000\000\
\000\000\000\000\000\000\101\255\000\000\000\000\171\000\137\000\
\035\000\000\000\069\000\000\000\000\000\117\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\255\061\001\109\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\127\000\241\255\000\000\000\000\027\000\112\000\040\000\
\253\255\000\000\000\000\060\000\007\000\074\000\212\255\000\000\
\000\000\044\000"

let yytablesize = 641
let yytable = "\094\000\
\030\000\026\000\042\000\043\000\086\000\001\000\048\000\075\000\
\003\000\060\000\061\000\062\000\063\000\007\000\026\000\026\000\
\039\000\030\000\026\000\095\000\050\000\076\000\052\000\055\000\
\056\000\057\000\045\000\047\000\064\000\053\000\054\000\055\000\
\056\000\057\000\047\000\037\000\037\000\066\000\054\000\055\000\
\056\000\057\000\092\000\093\000\037\000\037\000\037\000\037\000\
\037\000\087\000\088\000\089\000\090\000\091\000\026\000\026\000\
\116\000\049\000\099\000\115\000\031\000\117\000\040\000\041\000\
\105\000\068\000\069\000\051\000\049\000\052\000\026\000\081\000\
\082\000\097\000\066\000\066\000\026\000\053\000\054\000\055\000\
\056\000\057\000\067\000\070\000\073\000\071\000\072\000\079\000\
\106\000\080\000\047\000\083\000\084\000\100\000\102\000\085\000\
\103\000\056\000\121\000\104\000\108\000\122\000\037\000\109\000\
\111\000\110\000\112\000\118\000\094\000\124\000\026\000\119\000\
\125\000\026\000\054\000\038\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\024\000\123\000\018\000\055\000\
\060\000\023\000\019\000\061\000\029\000\020\000\044\000\120\000\
\046\000\021\000\059\000\022\000\011\000\023\000\114\000\107\000\
\098\000\113\000\000\000\033\000\024\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\000\000\017\000\
\018\000\000\000\000\000\000\000\019\000\000\000\000\000\020\000\
\000\000\000\000\045\000\021\000\000\000\022\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\032\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\000\000\000\000\018\000\000\000\000\000\000\000\019\000\000\000\
\000\000\020\000\000\000\000\000\025\000\021\000\000\000\022\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\033\000\
\024\000\000\000\038\000\009\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\000\000\000\000\018\000\000\000\000\000\
\000\000\019\000\000\000\000\000\020\000\000\000\013\000\000\000\
\021\000\000\000\022\000\000\000\023\000\000\000\000\000\000\000\
\000\000\017\000\033\000\024\000\000\000\000\000\000\000\000\000\
\019\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\000\000\030\000\030\000\030\000\000\000\000\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\000\000\030\000\030\000\030\000\
\000\000\030\000\030\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\000\000\047\000\047\000\047\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\000\000\047\000\
\047\000\047\000\000\000\047\000\047\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\000\000\049\000\
\049\000\049\000\000\000\000\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
\000\000\049\000\049\000\049\000\000\000\049\000\049\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\000\000\037\000\037\000\037\000\000\000\000\000\037\000\037\000\
\037\000\037\000\000\000\000\000\037\000\037\000\000\000\037\000\
\037\000\037\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\000\000\046\000\046\000\046\000\000\000\000\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\000\000\046\000\046\000\000\000\
\000\000\000\000\046\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\000\000\045\000\045\000\045\000\
\000\000\000\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\000\000\045\000\
\000\000\000\000\000\000\000\000\045\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\000\000\025\000\
\025\000\025\000\000\000\000\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\000\000\013\000\013\000\013\000\000\000\000\000\013\000\013\000\
\013\000\013\000\000\000\000\000\000\000\013\000\017\000\013\000\
\013\000\013\000\017\000\017\000\000\000\019\000\000\000\000\000\
\013\000\019\000\019\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\000\000\002\000\002\000\000\000\
\000\000\000\000\002\000\000\000\000\000\002\000\000\000\000\000\
\000\000\002\000\000\000\002\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\000\000\015\000\
\015\000\000\000\000\000\000\000\015\000\000\000\000\000\015\000\
\000\000\000\000\000\000\015\000\000\000\015\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\038\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\000\000\000\000\018\000\000\000\000\000\000\000\019\000\000\000\
\000\000\020\000\000\000\000\000\000\000\021\000\000\000\022\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000"

let yycheck = "\004\001\
\000\000\005\000\018\000\019\000\049\000\001\000\022\000\001\001\
\010\001\004\001\005\001\006\001\007\001\001\001\018\000\019\000\
\010\000\014\001\022\000\024\001\024\000\015\001\022\001\031\001\
\032\001\033\001\020\000\021\000\023\001\029\001\030\001\031\001\
\032\001\033\001\000\000\020\001\021\001\031\000\030\001\031\001\
\032\001\033\001\058\000\059\000\029\001\030\001\031\001\032\001\
\033\001\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\013\001\019\001\074\000\104\000\014\001\018\001\001\001\002\001\
\080\000\005\001\006\001\000\000\000\000\022\001\074\000\020\001\
\021\001\067\000\020\001\021\001\080\000\029\001\030\001\031\001\
\032\001\033\001\014\001\020\001\014\001\021\001\024\001\013\001\
\082\000\017\001\084\000\024\001\021\001\014\001\001\001\026\001\
\020\001\032\001\114\000\021\001\020\001\117\000\000\000\021\001\
\013\001\024\001\100\000\020\001\004\001\013\001\114\000\028\001\
\013\001\117\000\020\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\024\001\119\000\012\001\020\001\
\020\001\024\001\016\001\020\001\006\000\019\001\020\001\109\000\
\000\000\023\001\027\000\025\001\024\001\027\001\103\000\084\000\
\071\000\102\000\255\255\033\001\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\255\255\255\255\255\255\016\001\255\255\255\255\019\001\
\255\255\255\255\000\000\023\001\255\255\025\001\255\255\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\255\255\012\001\255\255\255\255\255\255\016\001\255\255\
\255\255\019\001\255\255\255\255\000\000\023\001\255\255\025\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\033\001\
\034\001\255\255\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\255\255\255\255\012\001\255\255\255\255\
\255\255\016\001\255\255\255\255\019\001\255\255\000\000\255\255\
\023\001\255\255\025\001\255\255\027\001\255\255\255\255\255\255\
\255\255\000\000\033\001\034\001\255\255\255\255\255\255\255\255\
\000\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\034\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\255\255\011\001\012\001\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\029\001\
\030\001\031\001\255\255\033\001\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\013\001\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\029\001\030\001\031\001\255\255\033\001\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\018\001\019\001\255\255\255\255\022\001\023\001\255\255\025\001\
\026\001\027\001\255\255\029\001\030\001\031\001\032\001\033\001\
\034\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\255\255\
\255\255\255\255\034\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\255\255\011\001\012\001\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\013\001\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\018\001\019\001\255\255\255\255\255\255\023\001\013\001\025\001\
\026\001\027\001\017\001\018\001\255\255\013\001\255\255\255\255\
\034\001\017\001\018\001\026\001\255\255\255\255\255\255\255\255\
\255\255\255\255\026\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\255\255\011\001\012\001\255\255\
\255\255\255\255\016\001\255\255\255\255\019\001\255\255\255\255\
\255\255\023\001\255\255\025\001\255\255\027\001\255\255\255\255\
\255\255\255\255\255\255\255\255\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\255\255\011\001\
\012\001\255\255\255\255\255\255\016\001\255\255\255\255\019\001\
\255\255\255\255\255\255\023\001\255\255\025\001\255\255\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\255\255\012\001\255\255\255\255\255\255\016\001\255\255\
\255\255\019\001\255\255\255\255\255\255\023\001\255\255\025\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\034\001"

let yynames_const = "\
  EOF\000\
  SET\000\
  DEF\000\
  BEGIN\000\
  END\000\
  GETS\000\
  TILD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SEQ\000\
  LBRA\000\
  RBRA\000\
  LCUR\000\
  RCUR\000\
  FUN\000\
  YIELDS\000\
  MINUS\000\
  NOT\000\
  PP_IFDEF\000\
  PP_ENDIF\000\
  PP_ENDL\000\
  "

let yynames_block = "\
  VAR\000\
  VARLPAR\000\
  VARLBRA\000\
  STRING\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  TIME\000\
  INTERVAL\000\
  BIN0\000\
  BIN1\000\
  BIN2\000\
  BIN3\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'settings) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 145 "tools/lang_parser.mly"
                       ( _2 )
# 478 "tools/lang_parser.ml"
               : Lang.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "tools/lang_parser.mly"
    ( (* Make sure that the builtins are in the binding stack before
       * parsing the expression. *)
      init_bindings () )
# 486 "tools/lang_parser.ml"
               : 'settings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'settings_base) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'settings) in
    Obj.repr(
# 151 "tools/lang_parser.mly"
                           ( )
# 494 "tools/lang_parser.ml"
               : 'settings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "tools/lang_parser.mly"
                             ( Dtools.Var.set_string _2 _4 )
# 502 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 154 "tools/lang_parser.mly"
                             ( Dtools.Var.set_int    _2 _4 )
# 510 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 155 "tools/lang_parser.mly"
                             ( Dtools.Var.set_float  _2 _4 )
# 518 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 156 "tools/lang_parser.mly"
                             ( Dtools.Var.set_bool   _2 _4 )
# 526 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 157 "tools/lang_parser.mly"
                             ( Dtools.Var.set_list   _2 _4 )
# 534 "tools/lang_parser.ml"
               : 'settings_base))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "tools/lang_parser.mly"
                                     ( [] )
# 540 "tools/lang_parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inner_string_list) in
    Obj.repr(
# 161 "tools/lang_parser.mly"
                                     ( _2 )
# 547 "tools/lang_parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 164 "tools/lang_parser.mly"
                                     ( [_1] )
# 554 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inner_string_list) in
    Obj.repr(
# 165 "tools/lang_parser.mly"
                                     ( _1::_3 )
# 562 "tools/lang_parser.ml"
               : 'inner_string_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "tools/lang_parser.mly"
     ()
# 568 "tools/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "tools/lang_parser.mly"
               ()
# 574 "tools/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "tools/lang_parser.mly"
     ()
# 580 "tools/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "tools/lang_parser.mly"
               ()
# 586 "tools/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 174 "tools/lang_parser.mly"
                             ( _1 )
# 594 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 175 "tools/lang_parser.mly"
                             ( mk (Lang.Seq (_1,_3)) )
# 603 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 176 "tools/lang_parser.mly"
                             ( let name,def = _1 in
                                 pop () ;
                                 mk ~kind:Lang.unit_t
                                   (Lang.Let (name,def,mk Lang.Unit)) )
# 614 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 180 "tools/lang_parser.mly"
                             ( let name,def = _1 in
                                 pop () ;
                                 mk ~kind:_3.kind
                                   (Lang.Let (name,def,_3)) )
# 626 "tools/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inner_list) in
    Obj.repr(
# 186 "tools/lang_parser.mly"
                         ( _2 )
# 633 "tools/lang_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inner_list) in
    Obj.repr(
# 188 "tools/lang_parser.mly"
                           ( _1::_3 )
# 641 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "tools/lang_parser.mly"
                           ( [_1] )
# 648 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "tools/lang_parser.mly"
                           ( [] )
# 654 "tools/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 193 "tools/lang_parser.mly"
                                     ( _1 )
# 661 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 194 "tools/lang_parser.mly"
                                     ( mk (Lang.Int (~- _2)) )
# 668 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 195 "tools/lang_parser.mly"
                                     ( mk (Lang.Float (~-. _2)) )
# 675 "tools/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 197 "tools/lang_parser.mly"
                                     ( _2 )
# 682 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 198 "tools/lang_parser.mly"
                                     ( mk (Lang.Int _1) )
# 689 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 199 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "not", ["", _2])) )
# 696 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 200 "tools/lang_parser.mly"
                                     ( mk (Lang.Bool _1) )
# 703 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 201 "tools/lang_parser.mly"
                                     ( mk (Lang.Float  _1) )
# 710 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 202 "tools/lang_parser.mly"
                                     ( mk (Lang.String _1) )
# 717 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 203 "tools/lang_parser.mly"
                                     ( mk (Lang.List _1) )
# 724 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 204 "tools/lang_parser.mly"
                                     ( mk Lang.Unit )
# 730 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 205 "tools/lang_parser.mly"
                                     ( mk (Lang.Product (_2,_4)) )
# 738 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 206 "tools/lang_parser.mly"
                                     ( lookup _1 )
# 745 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'app_list) in
    Obj.repr(
# 207 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _1,_2)) )
# 753 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 208 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "_[_]",
                                           ["",_2;"",lookup _1])) )
# 762 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 210 "tools/lang_parser.mly"
                                     ( _2 )
# 769 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arglist) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 212 "tools/lang_parser.mly"
                                     ( let arglist = _3 in
                                         popn (List.length arglist) ;
                                         mk (Lang.Fun (arglist,_6)) )
# 779 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 215 "tools/lang_parser.mly"
                                     ( mk (Lang.Fun ([],_2)) )
# 786 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'exprs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 216 "tools/lang_parser.mly"
                                     ( let test = _2 in
                                       let block = mk (Lang.Fun ([],_4)) in
                                       let op = lookup "if" in
                                         mk (Lang.App (op,["",test;
                                                           "then",block])) )
# 798 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'exprs) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'exprs) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 222 "tools/lang_parser.mly"
                                     ( let test = _2 in
                                       let blockt = mk (Lang.Fun ([],_4)) in
                                       let blocke = mk (Lang.Fun ([],_6)) in
                                       let op = lookup "if" in
                                         mk (Lang.App (op,["",test;
                                                           "then",blockt;
                                                           "else",blocke])) )
# 813 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 229 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _2,
                                                     ["",_1;"",_3])) )
# 823 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 231 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _2,
                                                     ["",_1;"",_3])) )
# 833 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 233 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _2,
                                                     ["",_1;"",_3])) )
# 843 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 235 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup _2,
                                                     ["",_1;"",_3])) )
# 853 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 237 "tools/lang_parser.mly"
                                     ( mk (Lang.App (lookup "-",
                                                     ["",_1;"",_3])) )
# 862 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option list * int option list) in
    Obj.repr(
# 239 "tools/lang_parser.mly"
                                   ( mk_time_pred (between (fst _1) (snd _1)) )
# 869 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option list) in
    Obj.repr(
# 240 "tools/lang_parser.mly"
                                   ( mk_time_pred (during _1) )
# 876 "tools/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 243 "tools/lang_parser.mly"
                  ( _1,_3 )
# 884 "tools/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 244 "tools/lang_parser.mly"
                  ( "",_1 )
# 891 "tools/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    Obj.repr(
# 246 "tools/lang_parser.mly"
                                   ( [] )
# 897 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_list_elem) in
    Obj.repr(
# 247 "tools/lang_parser.mly"
                                  ( [_1] )
# 904 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'app_list_elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_list) in
    Obj.repr(
# 248 "tools/lang_parser.mly"
                                 ( _1::_3 )
# 912 "tools/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 251 "tools/lang_parser.mly"
                  (
       let body = _3 in
         bind ~kind:body.kind _1 ;
         _1,body
    )
# 924 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 256 "tools/lang_parser.mly"
                        (
      let body = _4 in
        bind ~kind:body.kind _2 ;
        _2,body
    )
# 937 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'arglist) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 261 "tools/lang_parser.mly"
                                         (
      let arglist = _3 in
      let body = mk (Lang.Fun (arglist,_6)) in
        popn (List.length arglist) ;
        bind ~kind:body.kind _2 ;
        _2,body
    )
# 953 "tools/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    Obj.repr(
# 270 "tools/lang_parser.mly"
                      ( [] )
# 959 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 271 "tools/lang_parser.mly"
                      ( [_1] )
# 966 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arglist) in
    Obj.repr(
# 272 "tools/lang_parser.mly"
                      ( _1::_3 )
# 974 "tools/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 274 "tools/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _2 ;
                     _2,_2,kind,_3 )
# 984 "tools/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 277 "tools/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _1 ;
                     "",_1,kind,_2 )
# 994 "tools/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 281 "tools/lang_parser.mly"
              ( Some _2 )
# 1001 "tools/lang_parser.ml"
               : 'opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 282 "tools/lang_parser.mly"
              ( None )
# 1007 "tools/lang_parser.ml"
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
