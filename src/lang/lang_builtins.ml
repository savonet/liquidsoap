(*****************************************************************************

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

 *****************************************************************************)

open Lang

type category = Sys | Math | String | List | Bool | Liq | Control
              | Interaction | Other

let string_of_category = function
  | Sys     -> "System"
  | Math    -> "Math"
  | String  -> "String"
  | List    -> "List"
  | Bool    -> "Bool"
  | Liq     -> "Liquidsoap"
  | Control -> "Control"
  | Interaction -> "Interaction"
  | Other   -> "Other"

let mkdoc ~cat ?(flags=[]) s k =
  let i = new Doc.item s in
  let category = string_of_category cat in
    i#add_subsection "category" (Doc.trivial category) ;
    i#add_subsection "type" (Doc.trivial (Lang.print_kind k)) ;
    List.iter
      (fun f -> i#add_subsection "flag" (Doc.trivial (Lang.string_of_flag f)))
      flags;
    i

(** Liquidsoap stuff *)

let log = Dtools.Log.make ["lang"]

let () =
  let set cast path v =
    try
      (cast (Configure.conf#path (Dtools.Conf.path_of_string path)))#set v
    with
      | Dtools.Conf.Unbound (t, s) ->
          log#f 2 "WARNING: there is no configuration key named %S!" path
  in
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t;
       false,"",Lang.univ_t ~constraints:[Lang_types.Dtools] 1]
      Lang.unit_t
  in
    Lang.builtins#register "set"
      ~doc:(mkdoc ~cat:Liq "Change some setting." kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       let path = Lang.to_string (Lang.assoc "" 1 p) in
                         begin match (Lang.assoc "" 2 p).value with
                           | Lang.String s -> set Dtools.Conf.as_string path s
                           | Lang.Int    s -> set Dtools.Conf.as_int    path s
                           | Lang.Bool   s -> set Dtools.Conf.as_bool   path s
                           | Lang.Float  s -> set Dtools.Conf.as_float  path s
                           | Lang.List   l ->
                               let l = List.map Lang.to_string l in
                                 set Dtools.Conf.as_list path l
                           | _ -> assert false
                         end ;
                         Unit) }

let () =
  let get cast path v =
    try
      (cast (Configure.conf#path (Dtools.Conf.path_of_string path)))#get
    with
      | Dtools.Conf.Unbound (t, s) ->
          log#f 2 "WARNING: there is no configuration key named %S!" path ;
          v
  in
  let univ = Lang.univ_t ~constraints:[Lang_types.Dtools] 1 in
  let kind = Lang.fun_t [false,"default",univ;false,"",string_t] univ in
    Lang.builtins#register "get"
      ~doc:(mkdoc ~cat:Liq "Get a setting's value." kind)
      { t = kind ;
        value = FFI (["default",None;"",None],[],
                     fun p ->
                       let path = Lang.to_string (List.assoc "" p) in
                       let v = List.assoc "default" p in
                         match v.value with
                           | Lang.String s ->
                               Lang.String (get Dtools.Conf.as_string path s)
                           | Lang.Int s ->
                               Lang.Int (get Dtools.Conf.as_int path s)
                           | Lang.Bool s ->
                               Lang.Bool (get Dtools.Conf.as_bool path s)
                           | Lang.Float s ->
                               Lang.Float (get Dtools.Conf.as_float path s)
                           | Lang.List l ->
                               let l = List.map Lang.to_string l in
                                 Lang.List
                                   (List.map
                                      Lang.string
                                      (get Dtools.Conf.as_list path l))
                           | _ -> assert false) }

let () =
  let kind = Lang.fun_t [true,"indicators",Lang.list_t Lang.string_t;
                         true,"persistent",Lang.bool_t;
                         false,"",Lang.string_t] Lang.request_t in
    Lang.builtins#register "request"
      ~doc:(mkdoc ~cat:Liq "Create a request." kind)
      { t = kind ;
        value = FFI (["indicators",Some (Lang.list []);
                      "persistent",Some (Lang.bool false);
                      "",None],[],
                     fun p ->
                       let indicators = List.assoc "indicators" p in
                       let persistent =
                         Lang.to_bool (List.assoc "persistent" p)
                       in
                       let initial = Lang.to_string (List.assoc "" p) in
                       let l = String.length initial in
                       let initial =
                         (* Remove trailing newline *)
                         if l > 0 && initial.[l - 1] = '\n' then
                           String.sub initial 0 (l - 1)
                         else
                           initial
                       in
                       let indicators =
                         List.map Lang.to_string (Lang.to_list indicators)
                       in
                       let indicators =
                         List.map Request.indicator indicators
                       in
                         Request
                           (Request.create ~persistent ~indicators initial)) }

let () =
  let kind = Lang.fun_t [] Lang.unit_t in
    Lang.builtins#register "shutdown"
      ~doc:(mkdoc ~cat:Liq "Shutdown the application." kind)
      { t = kind ;
        value = FFI ([],[],fun p -> Tutils.shutdown () ; Unit) }

let () =
  let protocol_t =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.float_t]
      (Lang.list_t Lang.string_t)
  in
  let kind =
    Lang.fun_t [false,"",Lang.string_t ; false,"",protocol_t ] Lang.unit_t
  in
    Lang.builtins#register "add_protocol"
      ~doc:(mkdoc ~cat:Liq "Register a new protocol." kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       let name = Lang.to_string (Lang.assoc "" 1 p) in
                       let f = Lang.assoc "" 2 p in
                         Request.protocols#register name
                           { Request.static = false ;
                             Request.resolve =
                               fun arg ~log timeout ->
                                 let l =
                                   Lang.apply f ["",Lang.string arg;
                                                 "",Lang.float timeout]
                                 in
                                   List.map
                                     (fun s ->
                                        Request.indicator (Lang.to_string s))
                                     (Lang.to_list l) } ;
                         Lang.Unit) }

let () =
  let kind = let t = false,"",Lang.int_t in Lang.fun_t [ t;t;t ] Lang.bool_t in
  Lang.builtins#register "time_in_mod"
    ~doc:(mkdoc ("INTERNAL: time_in_mod(a,b,c) checks that the unix time T "^
                 "satisfies a <= T mod c < b")
    ~cat:Other ~flags:[Lang.Hidden] kind)
    { t = kind ;
      value = FFI (["",None;"",None;"",None],[],
                   fun p ->
                     match List.map (fun (_,x) -> Lang.to_int x) p with
                       | [a;b;c] ->
                           let t = Unix.localtime (Unix.time ()) in
                           let t =
                             t.Unix.tm_sec +
                             t.Unix.tm_min * 60 +
                             t.Unix.tm_hour * 60 * 60 +
                             t.Unix.tm_wday * 24 * 60 * 60
                           in
                           let t = t mod c in
                             if a <= b then
                               Lang.Bool (a <= t && t < b)
                             else
                               Lang.Bool (not (b <= t && t < a))
                       | _ -> assert false) }

(** Math *)

let () =
  let kind =
    let t = Lang.univ_t ~constraints:[Lang_types.Num] 1 in
      Lang.fun_t [ false,"",t ] t
  in
    Lang.builtins#register "abs"
      ~doc:(mkdoc ~cat:Math "Absolute value." kind)
      { t = kind ;
        value = FFI (["",None],[],
                     fun p ->
                       match snd (List.hd p) with
                         | {value=Int i}   -> Int (abs i)
                         | {value=Float i} -> Float (abs_float i)
                         | _ -> assert false) }

let () =
  let kind =
    let t = Lang.univ_t ~constraints:[Lang_types.Num] 1 in
      Lang.fun_t [ false,"",t ; false,"",t ] t
  in
  let register_op doc name op_int op_float =
    Lang.builtins#register name
      ~doc:(mkdoc ~cat:Math (Printf.sprintf "%s of numbers." doc) kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match p with
                         | ["",{value=Int a};"",{value=Int b}] ->
                             Int (op_int a b)
                         | ["",{value=Float a};"",{value=Float b}] ->
                             Float (op_float a b)
                         | _ -> assert false) }
  in
    register_op "Multiplication" "*" ( * ) ( *. ) ;
    register_op "Division" "/" (/) (/.) ;
    register_op "Addition" "+" (+) (+.) ;
    register_op "Substraction " "-" (-) (-.) ;
    register_op "Exponentiation" "pow"
      (fun a b -> int_of_float ((float_of_int a) ** float_of_int b)) ( ** )

let () =
  let kind =
    let t = float_t in
      Lang.fun_t
        [true,"min",t; true,"max",t]
        t
  in
    Lang.builtins#register "random.float"
      ~doc:(mkdoc ~cat:Math "Generate a random value." kind)
      {
        t = kind ;
        (* TODO: better default values *)
        value =
          FFI (["min",Some (Lang.float (-1000000.));
                "max", Some (Lang.float 1000000.)],[],
               fun p ->
                 let min = Lang.to_float (List.assoc "min" p) in
                 let max = Lang.to_float (List.assoc "max" p) in
                   Float (Random.float (max -. min) +. min)
        )
      }

let () =
  let kind = fun_t [] bool_t in
    Lang.builtins#register "random.bool"
      ~doc:(mkdoc ~cat:Bool "Generate a random value." kind)
      {
        t = kind ;
        (* TODO: better default values *)
        value =
          FFI ([],[],
               fun p ->
                 Lang.Bool (Random.bool ()))
      }

(** Comparison and boolean connectives *)

let compare_value a b =
  let rec aux = function
    | Lang.Float  a, Lang.Float b   -> compare a b
    | Lang.Int    a, Lang.Int b     -> compare a b
    | Lang.String a, Lang.String b  -> compare a b
    | Lang.Bool   a, Lang.Bool b    -> compare a b
    | Lang.Unit    , Lang.Unit      -> 0
    | Lang.Product (a1,a2), Lang.Product (b1,b2) ->
        let c = aux (a1.value,b1.value) in
          if c=0 then aux (a2.value,b2.value) else c
    | Lang.List l1, Lang.List l2 ->
        let rec cmp = function
          | [],[] -> 0
          | [],_  -> -1
          | _,[]  -> 1
          | h1::l1,h2::l2 ->
              let c = aux (h1.value,h2.value) in
                if c=0 then cmp (l1,l2) else c
        in
          cmp (l1,l2)
    | _ -> assert false
  in
    aux (a.value,b.value)

let () =
  let kind =
    let t = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
      Lang.fun_t [ false,"",t ; false,"",t] Lang.bool_t
  in
  let register_op name op =
    Lang.builtins#register name
      ~doc:(mkdoc ~cat:Bool "Comparison of comparable values." kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     function
                         | ["",a;"",b] -> Lang.Bool (op (compare_value a b))
                         | _ -> assert false) }
  in
    register_op "==" (fun c -> c = 0) ;
    register_op "!=" (fun c -> c <> 0) ;
    register_op "<"  (fun c -> c = -1) ;
    register_op "<=" (fun c -> c <> 1) ;
    register_op ">=" (fun c -> c <> -1) ;
    register_op ">"  (fun c -> c = 1)

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.bool_t ; false,"",Lang.bool_t] Lang.bool_t
  in
    Lang.builtins#register "and"
      ~doc:(mkdoc ~cat:Bool "Return the conjunction of its arguments" kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_bool x) p with
                         | [a;b] -> Lang.Bool (a && b)
                         | _ -> assert false) } ;
    Lang.builtins#register "or"
      ~doc:(mkdoc ~cat:Bool "Return the disjunction of its arguments" kind)
      { t = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_bool x) p with
                         | [a;b] -> Lang.Bool (a || b)
                         | _ -> assert false) }

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.bool_t ] Lang.bool_t
  in
  Lang.builtins#register "not"
    ~doc:(mkdoc ~cat:Bool "Returns the negation of its argument." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     Lang.Bool (not (Lang.to_bool (List.assoc "" p)))) }

(** Operations on strings *)

let () =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.string_t]
      Lang.string_t
  in
  Lang.builtins#register "^"
    ~doc:(mkdoc ~cat:String "Concatenate strings." kind)
    { t = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let s1 = Lang.to_string (Lang.assoc "" 1 p) in
                     let s2 = Lang.to_string (Lang.assoc "" 2 p) in
                       Lang.String (s1 ^ s2)) }

let () =
  let kind =
    Lang.fun_t [ true, "separator", Lang.string_t ;
                 false, "", Lang.list_t Lang.string_t ]
               Lang.string_t
  in
    Lang.builtins#register "string.concat"
      ~doc:(mkdoc ~cat:String "Concatenate strings." kind )
    { t = kind ;
      value = FFI (["separator", Some (Lang.string "") ; "", None], [],
                   fun p ->
                     let sep = Lang.to_string (List.assoc "separator" p) in
                     let l = Lang.to_list (List.assoc "" p) in
                     let l = List.map Lang.to_string l in
                       Lang.String (String.concat sep l)) }

let () =
  let kind =
    Lang.fun_t [ false, "separator", Lang.string_t ;
                 false, "", Lang.string_t ]
               (Lang.list_t Lang.string_t)
  in
    Lang.builtins#register "string.split"
      ~doc:(mkdoc ~cat:String "Split a string at 'separator'." kind )
    { t = kind ;
      value = FFI (["separator", None ; "", None], [],
                   fun p ->
                     let sep = Lang.to_string (List.assoc "separator" p) in
                     let string = Lang.to_string (List.assoc "" p) in
                       Lang.List (List.map Lang.string 
                                    (Pcre.split ~pat:sep string))) }

let () =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ;
       false,"",Lang.list_t (Lang.product_t
                               Lang.string_t Lang.string_t)]
      Lang.string_t
  in
  Lang.builtins#register "%"
    ~doc:(mkdoc ~cat:String
            ("(pattern % [...,(k,v),...]) replaces in pattern occurences of:\n"^
             " - \'$(k)\' into \"v\";\n"^
             " - \'$(if $(k2),\"a\",\"b\")\' into \"a\" if k2 is found in "^
             "the list, \"b\" otherwise.") kind)
    { t = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let s = Lang.to_string (Lang.assoc "" 1 p) in
                     let l =
                       List.map
                         (fun p ->
                            let a,b = Lang.to_product p in
                              Lang.to_string a, Lang.to_string b)
                         (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.String
                         (Utils.interpolate (fun k -> List.assoc k l) s)) }

let () =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
  Lang.builtins#register "quote"
    ~doc:(mkdoc ~cat:String "Escape shell metacharacters." kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let s = Lang.to_string (List.assoc "" p) in
                       Lang.String (Filename.quote s)) }

(** Operations on lists. *)

let () =
  (* TODO It would be good to generalize this one but we'd need a way to handle
   *      errors. *)
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ;
       false,"",Lang.list_t (Lang.product_t
                               Lang.string_t Lang.string_t)]
      Lang.string_t
  in
  Lang.builtins#register "_[_]"
    ~doc:(mkdoc ~cat:List
          "l[k] returns v for the first item (k,v) in l." kind)
    { t = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let k = Lang.to_string (Lang.assoc "" 1 p) in
                     let l = List.map
                               (fun p ->
                                  let (a,b) = Lang.to_product p in
                                    Lang.to_string a, Lang.to_string b)
                               (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.String (try List.assoc k l with _ -> "")) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.fun_t [false, "", Lang.univ_t 1] Lang.unit_t ;
                 false, "", (Lang.list_t (Lang.univ_t 1)) ]
               Lang.unit_t
  in
    Lang.builtins#register "list.iter"
      ~doc:(mkdoc ~cat:List
              "Execute a function on every element of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let f,l = match p with
                         | (_,f)::(_,l)::_ -> f,l
                         | _ -> assert false
                     in
                     let l = Lang.to_list l in
                       List.iter (fun c -> 
                         ignore (Lang.apply f ["",c])) l;
                     Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "",
                 Lang.fun_t [false, "", Lang.univ_t 1] (Lang.univ_t 2) ;
                 false, "", (Lang.list_t (Lang.univ_t 1)) ]
               (Lang.list_t (Lang.univ_t 2))
  in
    Lang.builtins#register "list.map"
      ~doc:(mkdoc ~cat:List
              "Map a function on every element of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let f,l = match p with
                         | (_,f)::(_,l)::_ -> f,l
                         | _ -> assert false
                     in
                     let l = Lang.to_list l in
                     let l =
                       List.map (fun c -> (Lang.apply f ["",c])) l
                     in
                       Lang.List l) }

let () =
  let kind =
    Lang.fun_t
      [
        false, "",
        Lang.fun_t
          [false, "", Lang.univ_t 1; false, "", Lang.univ_t 2]
          (Lang.univ_t 1) ;
        false, "", Lang.univ_t 1;
        false, "", (Lang.list_t (Lang.univ_t 2))
      ]
      (Lang.univ_t 1)
  in
    Lang.builtins#register "list.fold"
      ~doc:(mkdoc ~cat:List
              "Fold a function on every element of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let f,x,l = match p with
                         | (_,f)::(_,x)::(_,l)::_ -> f,x,l
                         | _ -> assert false
                     in
                     let l = Lang.to_list l in
                     let v =
                       List.fold_left
                         (fun x y -> Lang.apply f ["",x; "",y])
                         x l
                     in
                       v.Lang.value
      ) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.univ_t 1) ;
                 false, "", Lang.int_t ]
               (Lang.univ_t 1)
  in
  Lang.builtins#register "list.nth"
    ~doc:(mkdoc ~cat:List "Returns the nth element of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let k = Lang.to_int (Lang.assoc "" 2 p) in
                     let l = List.map
                               (fun p -> ("", Lang.to_string p))
                               (Lang.to_list (Lang.assoc "" 1 p))
                     in
                       Lang.String (try Lang.assoc "" k l with _ -> "")) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.string_t) ]
               Lang.string_t
  in
  Lang.builtins#register "list.hd"
    ~doc:(mkdoc ~cat:List "Returns the head (first element) of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     let l = List.map
                               Lang.to_string
                               (Lang.to_list (Lang.assoc "" 1 p))
                     in
                       try
                         Lang.String (List.hd l)
                       with
                         Failure "hd" -> Lang.String "") }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.univ_t 1) ]
               (Lang.list_t (Lang.univ_t 1))
  in
  Lang.builtins#register "list.tl"
    ~doc:(mkdoc ~cat:List "Returns the list without its first element." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     let l = Lang.to_list (Lang.assoc "" 1 p) in
                       match l with
                         | [] -> Lang.List []
                         | _::tl -> Lang.List tl) }

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
  let kind =
    Lang.fun_t [ false, "", t ;
                 false, "", Lang.list_t t ]
               Lang.bool_t
  in
  Lang.builtins#register "list.mem"
    ~doc:(mkdoc ~cat:List
            "Checks if an element is present within a list." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let e = Lang.assoc "" 1 p in
                     let l = Lang.to_list (Lang.assoc "" 2 p) in
                       Lang.Bool
                         (List.exists (fun e' -> compare_value e e' = 0) l)) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.univ_t 1) ]
               Lang.int_t
  in
  Lang.builtins#register "list.length"
    ~doc:(mkdoc ~cat:List
            "Returns the length (number of elements) of a list." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     let l = Lang.to_list (Lang.assoc "" 1 p)
                     in
                       Lang.Int (List.length l)) }

(** Misc control/system functions. *)

let () =
  let kind =
    Lang.fun_t [ true,"name",Lang.string_t ;
                 false,"",Lang.float_t ;
                 false,"",Lang.fun_t [] Lang.float_t ] Lang.unit_t
  in
    Lang.builtins#register "add_timeout"
      ~doc:(mkdoc ("Call a function every N seconds. "^
                   "If the output of the function is a positive float "^
                   "it will be used as the new delay. "^
                   "The name of the created thread can be chosen.")
              ~cat:Control kind)
      { t = kind ;
        value = FFI (["name",Some (Lang.string "add_timeout");
                      "",None;"",None],[],
                     fun p ->
                       let name = Lang.to_string (List.assoc "name" p) in
                       let d,f = match p with
                         | (_,d)::(_,f)::_ -> d,f
                         | _ -> assert false
                       in
                       let d = ref (Lang.to_float d) in
                         ignore (Tutils.create
                                   (fun () ->
                                      while true do
                                        Thread.delay !d ;
                                        let out =
                                          Lang.to_float (Lang.apply f [])
                                        in
                                          if out >= 0. then d := out
                                      done)
                                   ()
                                   name) ;
                         Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.string_t ;
                 true, "", Lang.string_t ]
               (Lang.list_t Lang.string_t)
  in
  Lang.builtins#register "execute"
    ~doc:(mkdoc ~cat:Sys "Executes a command." kind)
    { t = kind ;
      value = FFI ([ "", None ; "", Some (Lang.string "") ], [],
                   fun p ->
                     let c = Lang.to_string (Lang.assoc "" 1 p) in
                     let a = Lang.to_string (Lang.assoc "" 2 p) in
                       let r = try Server.exec (c ^ " " ^ a)
                               with Not_found -> "Command not found!"
                       in
                         Lang.List
                           (List.map Lang.string 
                             (Pcre.split ~pat:"\n" r))) }

let () =
  let t = Lang.univ_t 1 in
  let kind =
    Lang.fun_t [ false, "", Lang.bool_t ;
                 false, "then", Lang.fun_t [] t ;
                 false, "else", Lang.fun_t [] t ]
      t
  in
    Lang.builtins#register "if"
      ~doc:(mkdoc ~cat:Control ~flags:[Lang.Hidden]
              "Execute a function conditionally." kind)
    { t = kind ;
      value = FFI ([ "", None ; "then", None ; "else", None ], [],
                   fun p ->
                     let c = (List.assoc "" p) in
                     let fy = (List.assoc "then" p) in
                     let fn = (List.assoc "else" p) in
                     let c = Lang.to_bool c in
                       (Lang.apply (if c then fy else fn) []).value) }

let () =
  let cb_kind = Lang.fun_t [] Lang.unit_t in
  let kind = Lang.fun_t [false,"",cb_kind] Lang.unit_t in
  Lang.builtins#register "on_shutdown"
    ~doc:(mkdoc ~cat:Sys "Run a callback when Liquidsoap shuts down." kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let f = (List.assoc "" p) in
                     let wrap_f = fun () -> ignore (Lang.apply f []) in
                       ignore (Dtools.Init.at_stop wrap_f) ;
                       Lang.Unit) }

let () =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.unit_t in
  Lang.builtins#register "system"
    ~doc:(mkdoc ~cat:Sys "Shell command call." kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     ignore (Unix.system (Lang.to_string (List.assoc "" p))) ;
                     Lang.Unit) }

let () =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
  Lang.builtins#register "get_process_output"
    ~doc:(mkdoc ~cat:Sys
            "Perform a shell call and return its output."
            kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let chan =
                       Unix.open_process_in
                         (Lang.to_string (List.assoc "" p))
                     in
                     let rec aux s =
                       let more = String.make 128 '?' in
                       let n = input chan more 0 128 in
                         if n = 0 then s else
                           aux (s^(String.sub more 0 n))
                     in
                     let s = aux "" in
                       ignore (Unix.close_process_in chan) ;
                       Lang.String s) }

let () =
  let kind = Lang.fun_t [false,"",Lang.string_t] (Lang.list_t Lang.string_t) in
  Lang.builtins#register "get_process_lines"
    ~doc:(mkdoc ~cat:Sys
            "Perform a shell call and return the list of its output lines."
            kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let chan =
                       Unix.open_process_in
                         (Lang.to_string (List.assoc "" p))
                     in
                     let rec aux () =
                       match
                         try Some (input_line chan) with End_of_file -> None
                       with
                         | None -> []
                         | Some s -> s::(aux ())
                     in
                     let l = aux () in
                         ignore (Unix.close_process_in chan) ;
                         Lang.List (List.map Lang.string l)) }

let () =
  let kind =
    Lang.fun_t
      [true,"label",string_t;true,"level",int_t;false,"",Lang.string_t]
      Lang.unit_t
  in
  Lang.builtins#register "log"
    ~doc:(mkdoc ~cat:Sys "Log a message." kind)
    { t = kind ;
      value = FFI (["label",Some (string "lang");
                    "level",Some (int 3);
                    "",None],[],
                   fun p ->
                     let msg = Lang.to_string (List.assoc "" p) in
                     let label = Lang.to_string (List.assoc "label" p) in
                     let level = Lang.to_int (List.assoc "level" p) in
                       (Dtools.Log.make [label])#f level "%s" msg ;
                       Lang.Unit ) }

let () =
  let argv = Shebang.argv in
  let offset = ref 0 in
  let offset () =
    if !offset > 0 then !offset else begin
      while !offset < Array.length argv && argv.(!offset) <> "--" do
        incr offset
      done ;
      !offset
    end
  in
  let kind =
    Lang.fun_t
      [true,"default",Lang.string_t;
       false,"",Lang.int_t]
      Lang.string_t
  in
    Lang.builtins#register "argv"
      ~doc:(mkdoc ~cat:Sys "Get command-line parameters." kind)
      { t = kind ;
        value = FFI (["default", Some (Lang.string "") ;
                      "", None],
                     [],
                     fun p ->
                       let default = Lang.to_string (List.assoc "default" p) in
                       let i = Lang.to_int (List.assoc "" p) in
                       let i = offset () + i in
                         if i < Array.length argv then
                           Lang.String argv.(i)
                         else
                           Lang.String default) }

let () =
  let kind = Lang.fun_t [] Lang.unit_t in
    Lang.builtins#register "shutdown"
      ~doc:(mkdoc ~cat:Sys "Terminates the whole liquidsoap process." kind)
      { t = kind ;
        value = FFI ([],[],
                     fun p ->
                       assert false) }

(** Data conversions. *)

let () =
  (** Register as [name] the function which composes [in_value],[func] and
    * [out_value], and returns [default] in exceptional cases -- which MUST not
    * occur when default is not supplied. *)
  let register_tt doc name cat
        func ?default in_type in_value out_value out_type =
    let kind =
      Lang.fun_t
        (let p = [false,"",in_type] in
           match default with
             | None -> p
             | Some d -> (true,"default",out_type)::p)
        out_type
    in
      Lang.builtins#register name
        ~doc:(mkdoc ~cat:cat ("Convert " ^ doc ^ ".") kind)
      { t = kind ;
        value = FFI ((let p = ["",None] in
                        match default with
                          | None -> p
                          | Some d -> ("default",Some d)::p),
                     [],
                     fun p ->
                       try
                         out_value (func (in_value (List.assoc "" p)))
                       with _ -> (List.assoc "default" p).value) }
  in
  let register_tts name func ~default out_value out_type =
    register_tt ("a string to a " ^ name) (name ^ "_of_string") String
      func ~default Lang.string_t Lang.to_string out_value out_type
  in
  let register_tti name func out_value out_type =
    register_tt ("an int to a " ^ name) (name ^ "_of_int") Math
      func Lang.int_t Lang.to_int out_value out_type
  in
  let register_ttf name func out_value out_type =
    register_tt ("a float to a " ^ name) (name ^ "_of_float") Math
      func Lang.float_t Lang.to_float out_value out_type
  in
    register_tts
      "int" int_of_string ~default:(Lang.int 0)
      (fun v -> Lang.Int v) Lang.int_t ;
    register_tts
      "float" float_of_string ~default:(Lang.float 0.)
      (fun v -> Lang.Float v) Lang.float_t ;
    register_tts
      "bool" bool_of_string ~default:(Lang.bool false)
      (fun v -> Lang.Bool v) Lang.bool_t ;
    register_tti "float" float_of_int (fun v -> Lang.Float v) Lang.float_t ;
    register_tti "bool" (fun v -> v = 1) (fun v -> Lang.Bool v) Lang.bool_t ;
    register_ttf "int" int_of_float (fun v -> Lang.Int v) Lang.int_t ;
    register_ttf "bool" (fun v -> v = 1.) (fun v -> Lang.Bool v) Lang.bool_t

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.univ_t 1 ] Lang.string_t
  in
    Lang.builtins#register "string_of"
      ~doc:(mkdoc ~cat:String "Convert a value into a string." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     match List.assoc "" p with
                       | {Lang.value=Lang.String s} -> Lang.String s
                       | v -> Lang.String (Lang.print_value v)) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.univ_t 1 ] Lang.unit_t
  in
    Lang.builtins#register "ignore"
      ~doc:(mkdoc ~cat:Control
              "Convert anything to unit, preventing warnings." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun _ -> Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.source_t ] Lang.unit_t
  in
    Lang.builtins#register "source.skip"
      ~doc:(mkdoc ~cat:Liq
              "Skip source's current song." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p -> let s = Lang.to_source (List.assoc "" p) in
		            log#f 3 "Performing source.skip on %s" s#id;
		            s#abort_track; Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.source_t ] Lang.string_t
  in
    Lang.builtins#register "source.id"
      ~doc:(mkdoc ~cat:Liq
              "Get source's id." kind)
    { t = kind ;
      value = FFI ([ "", None ], [],
                   fun p -> let s = Lang.to_source (List.assoc "" p) in
                            Lang.String s#id) }


(** Sound utils. *)

let () =
  let kind =
    Lang.fun_t
      [false,"",Lang.float_t]
      Lang.float_t
  in
  Lang.builtins#register "dB_of_lin"
    ~doc:(mkdoc ~cat:Math "Convert linear scale into decibels." kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let x = Lang.to_float (Lang.assoc "" 1 p) in
                       Lang.Float (Sutils.dB_of_lin x)) } ;
  Lang.builtins#register "lin_of_dB"
    ~doc:(mkdoc ~cat:Math "Convert decibels into linear scale." kind)
    { t = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let x = Lang.to_float (Lang.assoc "" 1 p) in
                       Lang.Float (Sutils.lin_of_dB x)) }

(** Interactive parameters. *)

module Var =
struct
  exception Invalid_value of string

  type variable =
      {
        name : string;
        kind : Lang.kind;
        get : unit -> string;
        set : string -> unit;
        validate : string -> unit;
      }

  let variables = ref []

  let ns = Server.register ["var"] "interactive variables"

  let () =
    let usage = "list" in
      Server.add ~ns ~usage "list"
        (fun s ->
           String.concat "\n"
             (List.map
                (fun (_,v) ->
                   Printf.sprintf "%s : %s" v.name (Lang.print_kind v.kind))
                (List.sort (fun (m,_) (n,_) -> compare m n) !variables)))

  let () =
    let usage = "set <variable> = <value>" in
      Server.add ~ns ~usage "set"
        (fun s ->
           let pat = "^([a-zA-Z_][a-zA-Z0-9_.]*) *= *([^ ]+)" in
           try
             let sub = Pcre.exec ~pat s in
             let name = Pcre.get_substring sub 1 in
             let v = Pcre.get_substring sub 2 in
               try
                 let var = List.assoc name !variables in
                 let oldval = var.get () in
                   var.validate v;
                   var.set v;
                   Printf.sprintf "Variable %s set (was %s)." name oldval
               with
                 | Not_found ->
                     Printf.sprintf "Variable %s is not defined." name
                 | Invalid_value s ->
                     Printf.sprintf "Invalid value: %s." s
           with
             | Not_found -> "Usage: var." ^ usage)

  let () =
    let usage = "get <variable>" in
      Server.add ~ns ~usage "get"
        (fun name ->
           try
             let var = List.assoc name !variables in
               Printf.sprintf "%s" (var.get ())
           with
             | Not_found ->
                 Printf.sprintf "Variable %s is not defined." name)

  let add name t ~get ~set ~validate =
    let var =
      {
        name = name;
        kind = t;
        get = get;
        set = set;
        validate = validate;
      }
    in
      variables := (name,var)::!variables
end

let () =
  let kind =
    Lang.fun_t
      [
        false,"",Lang.string_t;
        false,"",Lang.float_t
      ]
      (Lang.fun_t [] Lang.float_t)
  in
    Lang.builtins#register "interactive_float"
      ~doc:(mkdoc ~cat:Interaction
              "Read a float from an interactive input." kind)
      {
        t = kind;
        value =
          FFI
            (["",None;"",None],[],
             fun p ->
               let name = Lang.to_string (Lang.assoc "" 1 p) in
               let v = Lang.to_float (Lang.assoc "" 2 p) in
               let v = ref v in
                 Var.add
                   name
                   Lang.float_t
                   ~get:(fun () -> Printf.sprintf "%.04f" !v)
                   ~set:(fun s -> v := float_of_string s)
                   ~validate:(fun s ->
                                try
                                  ignore (float_of_string s)
                                with _ ->
                                  raise (Var.Invalid_value
                                           (s ^ " is not a float")));
                 FFI ([],[], fun p -> Lang.Float !v)
            )
      }

let () =
  let kind =
    Lang.fun_t
      [true,"newline",Lang.bool_t;
       false,"",Lang.univ_t 1]
      Lang.unit_t
  in
    Lang.builtins#register "print"
      ~doc:(mkdoc ~cat:Interaction "Print on standard output." kind)
      {
        t = kind ;
        value =
          FFI
            (["newline",Some (Lang.bool true);"",None],[],
             fun p ->
               let nl = Lang.to_bool (List.assoc "newline" p) in
               let v = List.assoc "" p in
               let v = match v.value with
                 | Lang.String s -> s
                 | _ -> Lang.print_value v
               in
               let v = if nl then v^"\n" else v in
                 print_string v ; flush stdout ;
                 Unit)
      }
