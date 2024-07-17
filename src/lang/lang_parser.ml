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
  | BEGIN
  | END
  | GETS
  | TILD
  | DEF of (Doc.item * (string*string) list)
  | IF
  | THEN
  | ELSE
  | ELSIF
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
  | PP_INCLUDE
  | PP_DEF
  | PP_COMMENT of (string list)

open Parsing;;
# 24 "lang/lang_parser.mly"

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

# 139 "lang/lang_parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  266 (* BEGIN *);
  267 (* END *);
  268 (* GETS *);
  269 (* TILD *);
  271 (* IF *);
  272 (* THEN *);
  273 (* ELSE *);
  274 (* ELSIF *);
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
  294 (* PP_INCLUDE *);
  295 (* PP_DEF *);
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
  270 (* DEF *);
  285 (* BIN0 *);
  286 (* BIN1 *);
  287 (* BIN2 *);
  288 (* BIN3 *);
  296 (* PP_COMMENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\004\000\004\000\005\000\005\000\003\000\003\000\
\003\000\003\000\008\000\009\000\009\000\009\000\010\000\010\000\
\010\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\014\000\014\000\011\000\011\000\011\000\007\000\007\000\007\000\
\012\000\012\000\012\000\015\000\015\000\016\000\016\000\013\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\003\000\000\000\000\000\001\000\000\000\001\000\002\000\003\000\
\002\000\003\000\003\000\003\000\001\000\000\000\001\000\002\000\
\002\000\003\000\001\000\002\000\001\000\001\000\001\000\001\000\
\002\000\005\000\001\000\003\000\003\000\003\000\006\000\003\000\
\006\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\003\000\001\000\000\000\001\000\003\000\003\000\005\000\007\000\
\000\000\001\000\003\000\003\000\002\000\002\000\000\000\005\000\
\002\000\000\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\059\000\000\000\000\000\000\000\000\000\023\000\
\019\000\022\000\021\000\040\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\000\
\016\000\017\000\028\000\000\000\029\000\030\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\011\000\
\000\000\032\000\000\000\000\000\000\000\000\000\037\000\000\000\
\008\000\010\000\041\000\045\000\000\000\000\000\053\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\047\000\054\000\
\052\000\000\000\051\000\000\000\000\000\000\000\026\000\000\000\
\000\000\057\000\000\000\033\000\031\000\048\000\000\000\000\000\
\056\000"

let yydgoto = "\002\000\
\003\000\004\000\022\000\053\000\064\000\023\000\024\000\025\000\
\041\000\030\000\031\000\067\000\102\000\032\000\068\000\087\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\140\255\017\255\174\255\208\255\000\000\
\000\000\000\000\000\000\000\000\000\000\140\255\006\255\140\255\
\106\255\208\255\140\255\024\255\073\001\038\000\032\255\022\255\
\000\000\208\255\039\255\005\255\047\255\000\000\036\255\038\255\
\000\000\042\255\049\255\055\255\011\255\056\255\000\000\252\254\
\050\255\060\255\045\255\011\255\053\255\000\000\000\000\073\001\
\073\001\073\001\073\001\073\001\140\255\140\255\000\000\208\255\
\000\000\000\000\000\000\174\255\000\000\000\000\000\000\140\255\
\070\255\092\255\074\255\075\255\140\255\000\000\208\255\000\000\
\208\255\000\000\078\255\059\255\250\254\053\255\000\000\053\255\
\000\000\000\000\000\000\000\000\084\255\208\255\000\000\070\255\
\055\255\011\255\019\255\079\255\000\000\072\255\000\000\000\000\
\000\000\140\255\000\000\140\255\140\255\091\255\000\000\208\255\
\093\255\000\000\089\255\000\000\000\000\000\000\140\255\019\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\103\000\097\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\094\255\000\000\000\000\000\000\000\000\239\000\239\000\
\000\000\000\000\001\255\000\000\205\000\000\000\000\000\099\255\
\000\000\000\000\000\000\039\001\100\255\000\000\000\000\000\000\
\000\000\098\255\000\000\100\255\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\252\000\013\001\000\000\000\000\
\000\000\000\000\000\000\097\255\000\000\000\000\000\000\000\000\
\021\255\000\000\000\000\103\255\000\000\000\000\000\000\000\000\
\094\255\000\000\000\000\171\000\137\000\035\000\000\000\069\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\255\
\039\001\100\255\113\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\113\255\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\242\255\104\000\041\000\253\255\000\000\000\000\
\054\000\002\000\072\000\218\255\022\000\000\000\000\000\047\000"

let yytablesize = 619
let yytable = "\035\000\
\020\000\038\000\029\000\029\000\043\000\075\000\036\000\037\000\
\034\000\057\000\058\000\065\000\001\000\029\000\029\000\070\000\
\071\000\045\000\040\000\042\000\027\000\027\000\029\000\066\000\
\050\000\051\000\052\000\055\000\026\000\027\000\027\000\027\000\
\027\000\027\000\036\000\100\000\101\000\046\000\081\000\082\000\
\055\000\055\000\044\000\047\000\076\000\077\000\078\000\079\000\
\080\000\085\000\056\000\099\000\029\000\047\000\091\000\059\000\
\029\000\083\000\060\000\062\000\048\000\049\000\050\000\051\000\
\052\000\061\000\063\000\029\000\038\000\029\000\074\000\069\000\
\092\000\072\000\042\000\048\000\049\000\050\000\051\000\052\000\
\073\000\086\000\029\000\105\000\051\000\106\000\107\000\096\000\
\049\000\050\000\051\000\052\000\088\000\089\000\095\000\090\000\
\112\000\094\000\103\000\104\000\029\000\108\000\027\000\110\000\
\111\000\109\000\033\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\043\000\014\000\044\000\049\000\
\016\000\013\000\050\000\058\000\017\000\039\000\093\000\054\000\
\018\000\098\000\019\000\084\000\020\000\113\000\097\000\000\000\
\035\000\000\000\028\000\021\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\000\000\000\000\
\000\000\015\000\016\000\000\000\000\000\000\000\017\000\000\000\
\000\000\000\000\018\000\000\000\019\000\000\000\020\000\000\000\
\000\000\000\000\034\000\000\000\000\000\021\000\027\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\018\000\000\000\019\000\000\000\
\020\000\000\000\000\000\000\000\015\000\000\000\028\000\021\000\
\033\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\018\000\000\000\
\019\000\000\000\020\000\000\000\000\000\000\000\003\000\000\000\
\028\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\009\000\000\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\000\000\020\000\020\000\020\000\
\000\000\020\000\020\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\000\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\000\000\036\000\
\036\000\036\000\000\000\036\000\036\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\000\000\000\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\000\000\038\000\038\000\038\000\000\000\038\000\038\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\000\000\027\000\
\027\000\027\000\000\000\027\000\027\000\027\000\027\000\027\000\
\027\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\000\000\035\000\035\000\000\000\
\000\000\000\000\035\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\000\000\034\000\
\000\000\000\000\000\000\000\000\034\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\000\000\000\000\003\000\003\000\003\000\003\000\
\003\000\003\000\000\000\000\000\000\000\003\000\007\000\003\000\
\003\000\003\000\000\000\007\000\007\000\007\000\000\000\000\000\
\003\000\000\000\000\000\000\000\000\000\007\000\000\000\009\000\
\000\000\000\000\000\000\000\000\009\000\009\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\000\000\000\000\000\000\005\000\005\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\005\000\000\000\005\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\033\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\014\000\000\000\000\000\000\000\000\000\016\000\
\000\000\000\000\000\000\017\000\000\000\000\000\000\000\018\000\
\000\000\019\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000"

let yycheck = "\014\000\
\000\000\016\000\006\000\007\000\019\000\044\000\001\001\002\001\
\007\000\005\001\006\001\001\001\001\000\017\000\018\000\020\001\
\021\001\021\000\017\000\018\000\020\001\021\001\026\000\013\001\
\031\001\032\001\033\001\026\000\012\001\029\001\030\001\031\001\
\032\001\033\001\000\000\017\001\018\001\000\000\053\000\054\000\
\020\001\021\001\019\001\022\001\048\000\049\000\050\000\051\000\
\052\000\064\000\012\001\090\000\056\000\022\001\069\000\020\001\
\060\000\056\000\021\001\011\001\029\001\030\001\031\001\032\001\
\033\001\024\001\012\001\071\000\000\000\073\000\026\001\016\001\
\071\000\024\001\073\000\029\001\030\001\031\001\032\001\033\001\
\021\001\012\001\086\000\098\000\032\001\100\000\101\000\086\000\
\030\001\031\001\032\001\033\001\001\001\020\001\011\001\021\001\
\111\000\020\001\020\001\028\001\104\000\011\001\000\000\011\001\
\016\001\104\000\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\020\001\024\001\020\001\020\001\
\015\001\024\001\020\001\011\001\019\001\020\001\073\000\024\000\
\023\001\089\000\025\001\060\000\027\001\112\000\088\000\255\255\
\000\000\255\255\033\001\034\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\014\001\015\001\255\255\255\255\255\255\019\001\255\255\
\255\255\255\255\023\001\255\255\025\001\255\255\027\001\255\255\
\255\255\255\255\000\000\255\255\255\255\034\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\255\255\255\255\255\255\
\019\001\255\255\255\255\255\255\023\001\255\255\025\001\255\255\
\027\001\255\255\255\255\255\255\000\000\255\255\033\001\034\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\015\001\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\023\001\255\255\
\025\001\255\255\027\001\255\255\255\255\255\255\000\000\255\255\
\033\001\034\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\000\000\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\031\001\
\255\255\033\001\034\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\029\001\
\030\001\031\001\255\255\033\001\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\029\001\030\001\031\001\255\255\033\001\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\022\001\023\001\255\255\025\001\
\026\001\027\001\255\255\029\001\030\001\031\001\032\001\033\001\
\034\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\030\001\255\255\
\255\255\255\255\034\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\029\001\
\255\255\255\255\255\255\255\255\034\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\255\255\255\255\255\255\034\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\255\255\023\001\011\001\025\001\
\026\001\027\001\255\255\016\001\017\001\018\001\255\255\255\255\
\034\001\255\255\255\255\255\255\255\255\026\001\255\255\011\001\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\026\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\014\001\015\001\255\255\255\255\
\255\255\019\001\255\255\255\255\255\255\023\001\255\255\025\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\034\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\015\001\
\255\255\255\255\255\255\019\001\255\255\255\255\255\255\023\001\
\255\255\025\001\255\255\027\001\255\255\255\255\255\255\255\255\
\255\255\255\255\034\001"

let yynames_const = "\
  EOF\000\
  BEGIN\000\
  END\000\
  GETS\000\
  TILD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSIF\000\
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
  PP_INCLUDE\000\
  PP_DEF\000\
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
  DEF\000\
  BIN0\000\
  BIN1\000\
  BIN2\000\
  BIN3\000\
  PP_COMMENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'init) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 155 "lang/lang_parser.mly"
                   ( _2 )
# 482 "lang/lang_parser.ml"
               : Lang_values.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "lang/lang_parser.mly"
    ( (* Make sure that the builtins are in the binding stack before
       * parsing the expression. *)
      init_bindings () )
# 490 "lang/lang_parser.ml"
               : 'init))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "lang/lang_parser.mly"
     ()
# 496 "lang/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "lang/lang_parser.mly"
               ()
# 502 "lang/lang_parser.ml"
               : 's))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "lang/lang_parser.mly"
     ()
# 508 "lang/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "lang/lang_parser.mly"
               ()
# 514 "lang/lang_parser.ml"
               : 'g))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 166 "lang/lang_parser.mly"
                             ( _1 )
# 522 "lang/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 167 "lang/lang_parser.mly"
                             ( mk (Seq (_1,_3)) )
# 531 "lang/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 168 "lang/lang_parser.mly"
                             ( let doc,name,def = _1 in
                                 pop () ;
                                 mk (Let (doc,name,def,mk Unit)) )
# 541 "lang/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binding) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 171 "lang/lang_parser.mly"
                             ( let doc,name,def = _1 in
                                 pop () ;
                                 mk (Let (doc,name,def,_3)) )
# 552 "lang/lang_parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inner_list) in
    Obj.repr(
# 176 "lang/lang_parser.mly"
                         ( _2 )
# 559 "lang/lang_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inner_list) in
    Obj.repr(
# 178 "lang/lang_parser.mly"
                           ( _1::_3 )
# 567 "lang/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "lang/lang_parser.mly"
                           ( [_1] )
# 574 "lang/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "lang/lang_parser.mly"
                           ( [] )
# 580 "lang/lang_parser.ml"
               : 'inner_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 183 "lang/lang_parser.mly"
                                     ( _1 )
# 587 "lang/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 184 "lang/lang_parser.mly"
                                     ( mk (Int (~- _2)) )
# 594 "lang/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 185 "lang/lang_parser.mly"
                                     ( mk (Float (~-. _2)) )
# 601 "lang/lang_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 187 "lang/lang_parser.mly"
                                     ( _2 )
# 608 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 188 "lang/lang_parser.mly"
                                     ( mk (Int _1) )
# 615 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 189 "lang/lang_parser.mly"
                                     ( mk (App (lookup "not", ["", _2])) )
# 622 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 190 "lang/lang_parser.mly"
                                     ( mk (Bool _1) )
# 629 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 191 "lang/lang_parser.mly"
                                     ( mk (Float  _1) )
# 636 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 192 "lang/lang_parser.mly"
                                     ( mk (String _1) )
# 643 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 193 "lang/lang_parser.mly"
                                     ( mk (List _1) )
# 650 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 194 "lang/lang_parser.mly"
                                     ( mk Unit )
# 656 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 195 "lang/lang_parser.mly"
                                     ( mk (Product (_2,_4)) )
# 664 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 196 "lang/lang_parser.mly"
                                     ( lookup _1 )
# 671 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'app_list) in
    Obj.repr(
# 197 "lang/lang_parser.mly"
                                     ( mk (App (lookup ~pos:(1,1) _1,_2)) )
# 679 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 198 "lang/lang_parser.mly"
                                     ( mk (App (lookup "_[_]",
                                           ["",_2;"",lookup ~pos:(1,1) _1])) )
# 688 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 200 "lang/lang_parser.mly"
                                     ( _2 )
# 695 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arglist) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "lang/lang_parser.mly"
                                     ( let arglist = _3 in
                                         popn (List.length arglist) ;
                                         mk (Fun (arglist,_6)) )
# 705 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 205 "lang/lang_parser.mly"
                                     ( mk (Fun ([],_2)) )
# 712 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exprs) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'if_elsif) in
    Obj.repr(
# 207 "lang/lang_parser.mly"
                                     ( let cond = _2 in
                                       let then_b =
                                         mk ~pos:(3,4) (Fun ([],_4))
                                       in
                                       let else_b = _5 in
                                       let op = lookup ~pos:(1,1) "if" in
                                         mk (App (op,["",cond;
                                                      "else",else_b;
                                                      "then",then_b])) )
# 729 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 216 "lang/lang_parser.mly"
                                     ( mk (App (lookup _2,
                                                     ["",_1;"",_3])) )
# 739 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 218 "lang/lang_parser.mly"
                                     ( mk (App (lookup _2,
                                                     ["",_1;"",_3])) )
# 749 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 220 "lang/lang_parser.mly"
                                     ( mk (App (lookup _2,
                                                     ["",_1;"",_3])) )
# 759 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 222 "lang/lang_parser.mly"
                                     ( mk (App (lookup _2,
                                                     ["",_1;"",_3])) )
# 769 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cexpr) in
    Obj.repr(
# 224 "lang/lang_parser.mly"
                                     ( mk (App (lookup "-",
                                                     ["",_1;"",_3])) )
# 778 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option list * int option list) in
    Obj.repr(
# 226 "lang/lang_parser.mly"
                                   ( mk_time_pred (between (fst _1) (snd _1)) )
# 785 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option list) in
    Obj.repr(
# 227 "lang/lang_parser.mly"
                                   ( mk_time_pred (during _1) )
# 792 "lang/lang_parser.ml"
               : 'cexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 230 "lang/lang_parser.mly"
                  ( _1,_3 )
# 800 "lang/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 231 "lang/lang_parser.mly"
                  ( "",_1 )
# 807 "lang/lang_parser.ml"
               : 'app_list_elem))
; (fun __caml_parser_env ->
    Obj.repr(
# 233 "lang/lang_parser.mly"
                                   ( [] )
# 813 "lang/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_list_elem) in
    Obj.repr(
# 234 "lang/lang_parser.mly"
                                  ( [_1] )
# 820 "lang/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'app_list_elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_list) in
    Obj.repr(
# 235 "lang/lang_parser.mly"
                                 ( _1::_3 )
# 828 "lang/lang_parser.ml"
               : 'app_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 238 "lang/lang_parser.mly"
                  (
       let body = _3 in
         bind ~kind:body.t _1 ;
         (Doc.none (),[]),_1,body
    )
# 840 "lang/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Doc.item * (string*string) list) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 243 "lang/lang_parser.mly"
                        (
      let body = _4 in
        bind ~kind:body.t _2 ;
        _1,_2,body
    )
# 854 "lang/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Doc.item * (string*string) list) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'arglist) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'g) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 248 "lang/lang_parser.mly"
                                         (
      let arglist = _3 in
      let body = mk (Fun (arglist,_6)) in
        popn (List.length arglist) ;
        bind ~kind:body.t _2 ;
        _1,_2,body
    )
# 871 "lang/lang_parser.ml"
               : 'binding))
; (fun __caml_parser_env ->
    Obj.repr(
# 257 "lang/lang_parser.mly"
                      ( [] )
# 877 "lang/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 258 "lang/lang_parser.mly"
                      ( [_1] )
# 884 "lang/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arglist) in
    Obj.repr(
# 259 "lang/lang_parser.mly"
                      ( _1::_3 )
# 892 "lang/lang_parser.ml"
               : 'arglist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 261 "lang/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _2 ;
                     _2,_2,kind,_3 )
# 902 "lang/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt) in
    Obj.repr(
# 264 "lang/lang_parser.mly"
                 ( let kind = fresh_kindvar () in
                     bind ~kind _1 ;
                     "",_1,kind,_2 )
# 912 "lang/lang_parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 268 "lang/lang_parser.mly"
              ( Some _2 )
# 919 "lang/lang_parser.ml"
               : 'opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 269 "lang/lang_parser.mly"
              ( None )
# 925 "lang/lang_parser.ml"
               : 'opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'exprs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'if_elsif) in
    Obj.repr(
# 272 "lang/lang_parser.mly"
                                    ( let cond = _2 in
                                      let then_b =
                                        mk ~pos:(3,4) (Fun ([],_4))
                                      in
                                      let else_b = _5 in
                                      let op = lookup ~pos:(1,1) "if" in
                                        mk (Fun ([],
                                          mk (App (op,["",cond;
                                                       "else",else_b;
                                                       "then",then_b])))) )
# 943 "lang/lang_parser.ml"
               : 'if_elsif))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 282 "lang/lang_parser.mly"
                                    ( mk ~pos:(1,2) (Fun([],_2)) )
# 950 "lang/lang_parser.ml"
               : 'if_elsif))
; (fun __caml_parser_env ->
    Obj.repr(
# 283 "lang/lang_parser.mly"
                                    ( mk (Fun ([],mk Unit)) )
# 956 "lang/lang_parser.ml"
               : 'if_elsif))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lang_values.value)
