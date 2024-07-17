(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

let add_builtin ~cat ~descr ?flags name proto ret_t f =
  Lang.add_builtin ~category:(string_of_category cat)
    ~descr ?flags name proto ret_t f

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
    add_builtin ~cat:Liq "set"
      ~descr:"Change some setting. \
              Use <code>liquidsoap --conf-descr</code> and \
              <code>liquidsoap --conf-descr-key KEY</code> on \
              the command-line to get some information \
                                    about available settings."
      ["",Lang.string_t,None,None;
       "",Lang.univ_t ~constraints:[Lang_types.Dtools] 1,None,None]
      Lang.unit_t
      (fun p ->
         let s = Lang.assoc "" 1 p in
         let path = Lang.to_string s in
           try begin match (Lang.assoc "" 2 p).Lang.value with
               | Lang.String s -> set Dtools.Conf.as_string path s
               | Lang.Int    s -> set Dtools.Conf.as_int    path s
               | Lang.Bool   s -> set Dtools.Conf.as_bool   path s
               | Lang.Float  s -> set Dtools.Conf.as_float  path s
               | Lang.List   l ->
                   let l = List.map Lang.to_string l in
                     set Dtools.Conf.as_list path l
               | _ -> assert false
             end ;
             Lang.unit
           with Dtools.Conf.Mismatch t ->
             let t =
               Configure.conf#path
                 (Dtools.Conf.path_of_string path)
             in
             let kind =
               match t#kind with
                 | Some "unit" -> "of type unit"
                 | Some "int" -> "of type int"
                 | Some "float" -> "of type float"
                 | Some "bool" -> "of type bool"
                 | Some "string" -> "of type string"
                 | Some "list" -> "of type [string]"
                 | _ -> "untyped"
             in
             let msg =
               Printf.sprintf
                 "key %S is %s, thus cannot be set to %s"
                 (Lang.to_string s)
                 kind (Lang.print_value (Lang.assoc "" 2 p))
             in
               raise (Lang.Invalid_value (s,msg)))

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
    add_builtin "get" ~cat:Liq ~descr:"Get a setting's value."
      ["default",univ,None,None;
       "",Lang.string_t,None,None]
      univ
      (fun p ->
         let path = Lang.to_string (List.assoc "" p) in
         let v = List.assoc "default" p in
           match v.Lang.value with
             | Lang.String s ->
                 Lang.string (get Dtools.Conf.as_string path s)
             | Lang.Int s ->
                 Lang.int (get Dtools.Conf.as_int path s)
             | Lang.Bool s ->
                 Lang.bool (get Dtools.Conf.as_bool path s)
             | Lang.Float s ->
                 Lang.float (get Dtools.Conf.as_float path s)
             | Lang.List l ->
                 let l = List.map Lang.to_string l in
                   Lang.list
                     (List.map
                        Lang.string
                        (get Dtools.Conf.as_list path l))
             | _ -> assert false)

let () =
  add_builtin "shutdown" ~cat:Liq ~descr:"Shutdown the application."
    [] Lang.unit_t
    (fun p -> 
      Duppy.stop Tutils.scheduler ;
      Tutils.shutdown () ; 
      Lang.unit)

let () =
  let protocol_t =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.float_t]
      (Lang.list_t Lang.string_t)
  in
    add_builtin "add_protocol" ~cat:Liq ~descr:"Register a new protocol."
      ["",Lang.string_t,None,None ;
       "",protocol_t,None,None ]
      Lang.unit_t
      (fun p ->
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
           Lang.unit)

let () =
  let t = "",Lang.int_t,None,None in
    add_builtin "time_in_mod" ~cat:Other ~flags:[Lang.Hidden]
      ~descr:("INTERNAL: time_in_mod(a,b,c) checks that the unix time T "^
              "satisfies a <= T mod c < b")
     [t;t;t] Lang.bool_t
     (fun p ->
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
                  Lang.bool (a <= t && t < b)
                else
                  Lang.bool (not (b <= t && t < a))
          | _ -> assert false)

(** Math *)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Num] 1 in
    add_builtin "abs" ~cat:Math ~descr:"Absolute value."
      [ "",t,None,None ] t
      (fun p ->
         match (snd (List.hd p)).Lang.value with
           | Lang.Int i   -> Lang.int (abs i)
           | Lang.Float i -> Lang.float (abs_float i)
           | _ -> assert false)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Num] 1 in
  let register_op doc name op_int op_float =
    add_builtin name ~cat:Math ~descr:(Printf.sprintf "%s of numbers." doc)
      ["",t,None,None;"",t,None,None] t
      (fun p ->
         match p with
           | ["",{Lang.value=Lang.Int a};"",{Lang.value=Lang.Int b}] ->
               Lang.int (op_int a b)
           | ["",{Lang.value=Lang.Float a};"",{Lang.value=Lang.Float b}] ->
               Lang.float (op_float a b)
           | _ -> assert false)
  in
    register_op "Multiplication" "*" ( * ) ( *. ) ;
    register_op "Division" "/" (/) (/.) ;
    register_op "Addition" "+" (+) (+.) ;
    register_op "Substraction " "-" (-) (-.) ;
    register_op "Exponentiation" "pow"
      (fun a b -> int_of_float ((float_of_int a) ** float_of_int b)) ( ** )

let () =
  add_builtin "random.float" ~cat:Math ~descr:"Generate a random value."
    (* TODO better default values *)
    ["min",Lang.float_t,Some (Lang.float (-1000000.)),None;
     "max",Lang.float_t,Some (Lang.float ( 1000000.)),None]
    Lang.float_t
    (fun p ->
       let min = Lang.to_float (List.assoc "min" p) in
       let max = Lang.to_float (List.assoc "max" p) in
         Lang.float (Random.float (max -. min) +. min))

let () =
  add_builtin "random.bool" ~cat:Bool ~descr:"Generate a random value."
    [] Lang.bool_t
    (fun p -> Lang.bool (Random.bool ()))

(** Comparison and boolean connectives *)

let compare_value a b =
  let rec aux = function
    | Lang.Float  a, Lang.Float b   -> compare a b
    | Lang.Int    a, Lang.Int b     -> compare a b
    | Lang.String a, Lang.String b  -> compare a b
    | Lang.Bool   a, Lang.Bool b    -> compare a b
    | Lang.Unit    , Lang.Unit      -> 0
    | Lang.Product (a1,a2), Lang.Product (b1,b2) ->
        let c = aux (a1.Lang.value,b1.Lang.value) in
          if c = 0 then aux (a2.Lang.value,b2.Lang.value) else c
    | Lang.List l1, Lang.List l2 ->
        let rec cmp = function
          | [],[] -> 0
          | [],_  -> -1
          | _,[]  -> 1
          | h1::l1,h2::l2 ->
              let c = aux (h1.Lang.value,h2.Lang.value) in
                if c=0 then cmp (l1,l2) else c
        in
          cmp (l1,l2)
    | _ -> assert false
  in
    aux (a.Lang.value,b.Lang.value)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
  let register_op name op =
    add_builtin name ~cat:Bool ~descr:"Comparison of comparable values."
      ["",t,None,None;"",t,None,None] Lang.bool_t
      (function
         | ["",a;"",b] -> Lang.bool (op (compare_value a b))
         | _ -> assert false)
  in
    register_op "==" (fun c -> c = 0) ;
    register_op "!=" (fun c -> c <> 0) ;
    register_op "<"  (fun c -> c = -1) ;
    register_op "<=" (fun c -> c <> 1) ;
    register_op ">=" (fun c -> c <> -1) ;
    register_op ">"  (fun c -> c = 1)

let () =
  add_builtin "and" ~cat:Bool
    ~descr:"Return the conjunction of its arguments"
    ["",Lang.bool_t,None,None;"",Lang.bool_t,None,None] Lang.bool_t
    (fun p ->
       match List.map (fun (_,x) -> Lang.to_bool x) p with
         | [a;b] -> Lang.bool (a && b)
         | _ -> assert false) ;
  add_builtin "or" ~cat:Bool
    ~descr:"Return the disjunction of its arguments"
    ["",Lang.bool_t,None,None;"",Lang.bool_t,None,None] Lang.bool_t
    (fun p ->
       match List.map (fun (_,x) -> Lang.to_bool x) p with
         | [a;b] -> Lang.bool (a || b)
         | _ -> assert false)

let () =
  add_builtin "not" ~cat:Bool ~descr:"Returns the negation of its argument."
    ["",Lang.bool_t,None,None] Lang.bool_t
    (fun p -> Lang.bool (not (Lang.to_bool (List.assoc "" p))))

(** Operations on strings *)

let () =
  add_builtin "string.ref" ~cat:String
    ~descr:"Returns a pair (get,set) \
            where get is a function of type <code>unit -> string</code>, \
            to get current value, \
            and set a function of type <code>string -> unit</code>, \
            to set a new value. \n\
            This is a workaround, and it shall be removed \
            when variable references are implemented."
    ["",Lang.string_t,None,Some "Initial value"]
    (Lang.product_t (Lang.fun_t [] Lang.string_t)
       (Lang.fun_t [false,"",Lang.string_t] Lang.unit_t))
    (fun p ->
       let i = ref (Lang.to_string (List.assoc "" p)) in
       let get  =
         Lang.val_fun []
         (fun p -> Lang.string !i)
       in
       let set =
         Lang.val_fun ["","",None]
         (fun p ->
             let v = List.assoc "" p in
             i := Lang.to_string v ;
             Lang.unit)
       in
       Lang.product get set)

let () =
  add_builtin "^" ~cat:String ~descr:"Concatenate strings."
    ["",Lang.string_t,None,None ; "",Lang.string_t,None,None]
    Lang.string_t
    (fun p ->
       let s1 = Lang.to_string (Lang.assoc "" 1 p) in
       let s2 = Lang.to_string (Lang.assoc "" 2 p) in
         Lang.string (s1 ^ s2))

let () =
  add_builtin "string.concat" ~cat:String ~descr:"Concatenate strings."
    [ "separator", Lang.string_t, Some (Lang.string ""), None ;
      "", Lang.list_t Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let sep = Lang.to_string (List.assoc "separator" p) in
       let l = Lang.to_list (List.assoc "" p) in
       let l = List.map Lang.to_string l in
         Lang.string (String.concat sep l))

let () =
  add_builtin "string.split" ~cat:String
    ~descr:"Split a string at 'separator'. \n\
            Perl compatible regular expressions \
	    are recognized. Hence, special characters \
	    should be escaped."
    [ "separator", Lang.string_t, None, None ;
      "", Lang.string_t, None, None ]
    (Lang.list_t Lang.string_t)
    (fun p ->
       let sep = Lang.to_string (List.assoc "separator" p) in
       let string = Lang.to_string (List.assoc "" p) in
       let rex = Pcre.regexp sep in
         Lang.list (List.map Lang.string
                      (Pcre.split ~rex string)))

let () =
  add_builtin "string.extract" ~cat:String
    ~descr:"Extract substrings from a string. \n\
            Perl compatible regular expressions \
            are recognized. Hence, special characters \
            should be escaped. \n\
            Returns a list of (index,value).\n\
            If the list does not have a pair associated to some index, \
            it means that the corresponding pattern was not found."
    [ "pattern", Lang.string_t, None, None ;
      "", Lang.string_t, None, None ]
    (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t))
    (fun p ->
       let pattern = Lang.to_string (List.assoc "pattern" p) in
       let string = Lang.to_string (List.assoc "" p) in
       let rex = Pcre.regexp pattern in
       try
         let sub = Pcre.exec ~rex string in
         let n = Pcre.num_of_subs sub in
         let rec extract l i =
           if i < n then
             try
               extract (l @ [(string_of_int i,Pcre.get_substring sub i)]) 
                       (i+1)
             with
               | Not_found -> extract l (i+1)
           else
             l
         in
         let l = extract [] 1 in
         Lang.list 
             (List.map (fun (x,y) -> 
                          Lang.product (Lang.string x) (Lang.string y))
                        l)
       with
         | Not_found -> Lang.list [])

let () =
  add_builtin "string.match" ~cat:String
    ~descr:"Match a string with an expression. \n\
            Perl compatible regular expressions \
            are recognized. Hence, special characters \
            should be escaped."
    [ "pattern", Lang.string_t, None, None ;
      "", Lang.string_t, None, None ]
    Lang.bool_t
    (fun p ->
       let pattern = Lang.to_string (List.assoc "pattern" p) in
       let string = Lang.to_string (List.assoc "" p) in
       let rex = Pcre.regexp pattern in
       Lang.bool (Pcre.pmatch ~rex string))

let () =
  add_builtin "string.replace" ~cat:String
    ~descr:"Replace substrings in a string. \n\
            Will replace all substrings matched \
	    in the pattern by the string returned \
	    by the replace function."
    [ "pattern", Lang.string_t, None, None ;
      "", Lang.fun_t [false,"",Lang.string_t] Lang.string_t,
      None, None ;
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let pattern = Lang.to_string (List.assoc "pattern" p) in
       let string = Lang.to_string (Lang.assoc "" 2 p) in
       let subst = Lang.assoc "" 1 p in
       let subst s = 
         let ret = 
           Lang.apply subst [("",Lang.string s)] 
         in 
         Lang.to_string ret
       in
       let rex = Pcre.regexp pattern in
       Lang.string (Pcre.substitute ~rex ~subst string))

let () =
  add_builtin "%" ~cat:String
    ~descr:"<code>pattern % [...,(k,v),...]</code> \
            changes in the pattern occurences of:\n\
             \ - <code>$(k)</code> into <code>v</code>;\n\
             \ - <code>$(if $(k2),\"a\",\"b\")</code> into \
                 \"a\" if k2 is found in the list, \"b\" otherwise."
    ["",Lang.string_t,None,None ;
     "",Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),None,None]
    Lang.string_t
    (fun p ->
                     let s = Lang.to_string (Lang.assoc "" 1 p) in
                     let l =
                       List.map
                         (fun p ->
                            let a,b = Lang.to_product p in
                              Lang.to_string a, Lang.to_string b)
                         (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.string
                         (Utils.interpolate (fun k -> List.assoc k l) s))

let () =
  add_builtin "quote" ~cat:String ~descr:"Escape shell metacharacters."
    ["",Lang.string_t,None,None] Lang.string_t
    (fun p ->
       let s = Lang.to_string (List.assoc "" p) in
         Lang.string (Filename.quote s))

(** Operations on lists. *)

let () =
  (* TODO It would be good to generalize this one but we'd need a way to handle
   *      errors. *)
  add_builtin "_[_]" ~cat:List
    ~descr:"<code>l[k]</code> returns the first <code>v</code> such that \
            <code>(k,v)</code> is in the list <code>l</code>."
    ["",Lang.string_t,None,None ;
     "",Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),None,None]
    Lang.string_t
    (fun p ->
       let k = Lang.to_string (Lang.assoc "" 1 p) in
       let l =
         List.map
           (fun p ->
              let (a,b) = Lang.to_product p in
                Lang.to_string a, Lang.to_string b)
           (Lang.to_list (Lang.assoc "" 2 p))
       in
         Lang.string (try List.assoc k l with _ -> ""))

let () =
  add_builtin "list.iter" ~cat:List
    ~descr:"Call a function on every element of a list."
    [ "", Lang.fun_t [false, "", Lang.univ_t 1] Lang.unit_t, None, None ;
      "", (Lang.list_t (Lang.univ_t 1)), None, None ]
    Lang.unit_t
    (fun p ->
       let f,l =
         match p with
           | (_,f)::(_,l)::_ -> f,l
           | _ -> assert false
       in
       let l = Lang.to_list l in
         List.iter (fun c -> ignore (Lang.apply f ["",c])) l ;
         Lang.unit)

let () =
  add_builtin "list.map" ~cat:List
    ~descr:"Map a function on every element of a list."
    [ "",Lang.fun_t [false, "", Lang.univ_t 1] (Lang.univ_t 2),None,None ;
      "", (Lang.list_t (Lang.univ_t 1)), None, None ]
    (Lang.list_t (Lang.univ_t 2))
    (fun p ->
       let f,l =
         match p with
           | (_,f)::(_,l)::_ -> f,l
           | _ -> assert false
       in
       let l = Lang.to_list l in
       let l = List.map (fun c -> (Lang.apply f ["",c])) l in
         Lang.list l)

let () =
  add_builtin "list.fold" ~cat:List
    ~descr:"Fold a function on every element of a list."
    [ "",
      Lang.fun_t
        [false, "", Lang.univ_t 1; false, "", Lang.univ_t 2]
        (Lang.univ_t 1),
      None, None ;
      "", Lang.univ_t 1, None, None ;
      "", Lang.list_t (Lang.univ_t 2), None, None
    ]
    (Lang.univ_t 1)
    (fun p ->
       let f,x,l =
         match p with
           | (_,f)::(_,x)::(_,l)::_ -> f,x,l
           | _ -> assert false
       in
       let l = Lang.to_list l in
         List.fold_left (fun x y -> Lang.apply f ["",x; "",y]) x l)

let () =
  add_builtin "list.nth" ~cat:List ~descr:"Get the n-th element of a list."
    [ "",Lang.list_t (Lang.univ_t 1),None,None ;
      "",Lang.int_t,None,None ]
    (Lang.univ_t 1)
    (fun p ->
       List.nth
         (Lang.to_list (Lang.assoc "" 1 p))
         (Lang.to_int (Lang.assoc "" 2 p)))

let () =
  add_builtin "list.hd" ~cat:List
    ~descr:"Return the head of a list, i.e. its first element."
    ["",Lang.list_t Lang.string_t,None,None] Lang.string_t
    (fun p ->
       try
         List.hd (Lang.to_list (Lang.assoc "" 1 p))
       with
         | Failure "hd" -> Lang.string "")

let () =
  add_builtin "list.sort" ~cat:List
    ~descr:"Sort a list according to a comparison function."
    ["",Lang.fun_t [false,"",Lang.univ_t 1;false,"",Lang.univ_t 1] Lang.int_t,None,None ;
     "",Lang.list_t (Lang.univ_t 1),None,None] (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let f = Lang.assoc "" 1 p in
       let sort x y = 
         Lang.to_int (Lang.apply f ["",x;"",y])
       in
       Lang.list 
        (List.sort sort (Lang.to_list (Lang.assoc "" 2 p))))

let () =
  add_builtin "list.tl" ~cat:List
    ~descr:"Return the list without its first element."
    ["",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.to_list (Lang.assoc "" 1 p) in
         match l with
           | [] -> Lang.list []
           | _::tl -> Lang.list tl)

let () =
  add_builtin "list.append" ~cat:List
    ~descr:"Catenate two lists."
    ["",Lang.list_t (Lang.univ_t 1),None,None;
     "",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.to_list (Lang.assoc "" 1 p) in
       let l' = Lang.to_list (Lang.assoc "" 2 p) in
       Lang.list (l@l'))

let () =
  add_builtin "list.remove" ~cat:List
    ~descr:"Remove a value from a list."
    ["",Lang.univ_t 1,None,None;
     "",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let a = Lang.assoc "" 1 p in
       let l = Lang.to_list (Lang.assoc "" 2 p) in
       let rec remove a l l' = 
         match l with
           | x :: l'' when x = a -> l' @ l''
           | x :: l'' -> remove a l'' (l' @ [x])
           | [] -> l'
       in
       Lang.list (remove a l []))

let () =
  add_builtin "list.rev" ~cat:List
    ~descr:"Revert list order."
    ["",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.to_list (Lang.assoc "" 1 p) in
       Lang.list (List.rev l))

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Ord] 1 in
    add_builtin "list.mem" ~cat:List
      ~descr:"Check if an element belongs to a list."
      ["",t,None,None ; "",Lang.list_t t,None,None] Lang.bool_t
      (fun p ->
         let e = Lang.assoc "" 1 p in
         let l = Lang.to_list (Lang.assoc "" 2 p) in
           Lang.bool
             (List.exists (fun e' -> compare_value e e' = 0) l))

let () =
  add_builtin "list.length" ~cat:List
    ~descr:"Get the length of a list, i.e. its number of elements."
    ["",Lang.list_t (Lang.univ_t 1),None,None] Lang.int_t
    (fun p ->
       let l = Lang.to_list (Lang.assoc "" 1 p) in
         Lang.int (List.length l))

(** Operations on products. *)

let () =
  add_builtin "fst" ~cat:List (* TODO wrong category *)
    ~descr:"Get the first component of a pair."
    ["",Lang.product_t (Lang.univ_t 1) (Lang.univ_t 2),None,None]
    (Lang.univ_t 1)
    (fun p -> fst (Lang.to_product (Lang.assoc "" 1 p))) ;
  add_builtin "snd" ~cat:List (* TODO wrong category *)
    ~descr:"Get the second component of a pair."
    ["",Lang.product_t (Lang.univ_t 1) (Lang.univ_t 2),None,None]
    (Lang.univ_t 2)
    (fun p -> snd (Lang.to_product (Lang.assoc "" 1 p)))

(** Misc control/system functions. *)

let () =
  add_builtin "add_timeout" ~cat:Control
    [ "fast", Lang.bool_t, Some (Lang.bool true),
      Some
        "Set to <code>false</code> if the execution of the code can take long \
         in order to lower its priority below that of request resolutions and \
         fast timeouts. \
         This is only effective if you set a dedicated queue for fast tasks, \
         see the \"scheduler\" settings for more details." ;
      "",Lang.float_t,None,None ;
      "",Lang.fun_t [] Lang.float_t,None,None ]
    Lang.unit_t
    ~descr:"Call a function in N seconds. \
        If the result of the function is a positive or null integer, \
        the task will be scheduled after this amount of time (in seconds.)"
    (fun p ->
       let d = Lang.to_float (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
       let priority =
         if Lang.to_bool (List.assoc "fast" p) then
           Tutils.Maybe_blocking
         else
           Tutils.Blocking
       in
       let rec t =
         { Duppy.Task.
             priority = priority ;
             events   = [`Delay d] ;
             handler  =
               fun _ ->
                 if Lang.to_float (Lang.apply f []) >= 0. then [t] else [] }
       in
         Duppy.Task.add Tutils.scheduler t ;
         Lang.unit)

let () =
  let descr = "Execute a liquidsoap server command." in
  let cat = Liq in
  let params = 
    [ "", Lang.string_t, None, None ;
      "", Lang.string_t, Some (Lang.string ""), None ]
  in
  let return_t = (Lang.list_t Lang.string_t) in
  let execute p =
    let c = Lang.to_string (Lang.assoc "" 1 p) in
    let a = Lang.to_string (Lang.assoc "" 2 p) in
    let s =
      match a with
        | "" -> c
        | _ -> c ^ " " ^ a
    in
    let r = try Server.exec (s) with Not_found -> "Command not found!" in
      Lang.list (List.map Lang.string (Pcre.split ~pat:"\n" r))
  in
  add_builtin "server.execute" 
    ~cat ~descr params return_t execute ;
  add_builtin "execute" ~cat ~flags:[Lang.Deprecated]
    ~descr:(descr ^
            "\nThis operator is deprecated, in favor of 'server.execute'.")
    params return_t 
    (fun p ->
      log#f 1 "WARNING: 'execute' is DEPRECATED and will be \
               removed in future releases. You can use \
               'server.execute' instead." ;
       execute p) 

let () =
  add_builtin "if"
    ~cat:Control ~descr:"The basic conditional." ~flags:[Lang.Hidden]
    [ "",Lang.bool_t,None,None ;
      "then", Lang.fun_t [] (Lang.univ_t 1), None,None ;
      "else", Lang.fun_t [] (Lang.univ_t 1), None,None ]
    (Lang.univ_t 1)
    (fun p ->
       let c = List.assoc "" p in
       let fy = List.assoc "then" p in
       let fn = List.assoc "else" p in
       let c = Lang.to_bool c in
         Lang.apply (if c then fy else fn) [])

let () =
  add_builtin "on_shutdown" ~cat:Sys
    [ "", Lang.fun_t [] Lang.unit_t, None, None ]
    Lang.unit_t
    ~descr:"Register a function to be called when Liquidsoap shuts down."
    (fun p ->
       let f = List.assoc "" p in
       let wrap_f = fun () -> ignore (Lang.apply f []) in
         ignore (Dtools.Init.at_stop wrap_f) ;
         Lang.unit)

let () =
  add_builtin "system" ~cat:Sys
    ["",Lang.string_t,None,None]
    Lang.unit_t
    ~descr:"Shell command call. \
            Set verbose to true to log process' output and errors."
    (fun p ->
       ignore (Unix.system (Lang.to_string (List.assoc "" p))) ;
       Lang.unit)

let () =
  add_builtin "getpid" ~cat:Sys
    []
    Lang.int_t
    ~descr:"Get the process' pid."
    (fun p ->
       Lang.int (Unix.getpid()))

let () =
  add_builtin "get_process_output" ~cat:Sys
    ~descr:"Perform a shell call and return its output."
    [ "",Lang.string_t,None,None] Lang.string_t
    (fun p ->
       let chan =
         Unix.open_process_in (Lang.to_string (List.assoc "" p))
       in
       let rec aux s =
         let more = String.make 128 '?' in
         let n = input chan more 0 128 in
           if n = 0 then s else
             aux (s^(String.sub more 0 n))
       in
       let s = aux "" in
         ignore (Unix.close_process_in chan) ;
         Lang.string s)

let () =
  add_builtin "get_process_lines" ~cat:Sys
    ~descr:"Perform a shell call and return the list of its output lines."
    [ "",Lang.string_t,None,None] (Lang.list_t Lang.string_t)
    (fun p ->
       let chan = Unix.open_process_in (Lang.to_string (List.assoc "" p)) in
       let rec aux () =
         match
           try Some (input_line chan) with End_of_file -> None
         with
           | None -> []
           | Some s -> s::(aux ())
       in
       let l = aux () in
         ignore (Unix.close_process_in chan) ;
         Lang.list (List.map Lang.string l))

let () =
  add_builtin "log" ~cat:Sys ~descr:"Log a message."
    [ "label",Lang.string_t,Some (Lang.string "lang"),None ;
      "level",Lang.int_t,Some (Lang.int 3),None ;
      "",Lang.string_t,None,None ]
    Lang.unit_t
    (fun p ->
       let msg = Lang.to_string (List.assoc "" p) in
       let label = Lang.to_string (List.assoc "label" p) in
       let level = Lang.to_int (List.assoc "level" p) in
         (Dtools.Log.make [label])#f level "%s" msg ;
         Lang.unit)

let () =
  (** Cheap implementation of "getopt" which does not really deserve its name as
    * it has little to do with the standards that getopt(3) implements.
    * A complete rework of argv() and getopt() should eventually be done. *)
  let argv = Shebang.argv in
  let offset =
    (** Index of the last non-script parameter on the command-line. *)
    let rec find i =
      if i >= Array.length argv || argv.(i) = "--" then i else find (i+1)
    in
      find 0
  in
  let opts =
    ref (Array.to_list (Array.sub argv offset (Array.length argv - offset)))
  in
    add_builtin "getopt" ~cat:Sys
      ["default",Lang.string_t,Some (Lang.string ""),None;
       "",Lang.string_t,None,None]
      Lang.string_t
      ~descr:"\
   Parse command line options:\n\
   <code>getopt(\"-o\")</code> returns \"1\" if \"-o\" was passed \
   without any parameter, \"0\" otherwise.\n\
   <code>getopt(default=\"X\",\"-o\")</code> returns \"Y\" if \"-o Y\" \
   was passed, \"X\" otherwise.\n\
   The result is removed from the list of arguments, affecting subsequent\n\
   calls to <code>argv()</code> and <code>getopt()</code>."
      (fun p ->
         let default = Lang.to_string (List.assoc "default" p) in
         let name = Lang.to_string (List.assoc "" p) in
         let argv = !opts in
           if default = "" then
             try
               ignore (List.find (fun x -> x = name) argv) ;
               opts := List.filter (fun x -> x <> name) argv ;
               Lang.string "1"
             with
               | Not_found -> Lang.string "0"
           else
             begin
               let rec find l l' =
                 match l with
                   | [] ->
                       default, List.rev l'
                   | e :: v :: l when e = name ->
                       v, List.rev_append l' l
                   | e :: l ->
                       find l (e::l')
               in
               let v,l = find argv [] in
                 opts := l ;
                 Lang.string v
             end) ;

  add_builtin "argv" ~cat:Sys ~descr:"Get command-line parameters."
    ["default",Lang.string_t,Some (Lang.string ""),None;
     "",Lang.int_t,None,None]
    Lang.string_t
    (fun p ->
       let default = Lang.to_string (List.assoc "default" p) in
       let i = Lang.to_int (List.assoc "" p) in
       let opts = !opts in
         if i < List.length opts then
           Lang.string (List.nth opts i)
         else
           Lang.string default)

let () =
  add_builtin "server.register" ~cat:Sys
    ~descr:"Register a command. You can then execute this function \
            through the server, either telnet or socket."
    [ "namespace",Lang.string_t,Some (Lang.string ""),None ;
      "description",Lang.string_t,Some (Lang.string "No documentation available."),
      None ;
      "usage",Lang.string_t,Some (Lang.string ""),None ;
      "",Lang.string_t,None,None ;
      "",Lang.fun_t [false,"",Lang.string_t] Lang.string_t,None,None ]
    Lang.unit_t
    (fun p ->
       let namespace = Lang.to_string (List.assoc "namespace" p) in
       let descr = Lang.to_string (List.assoc "description" p) in
       let usage = Lang.to_string (List.assoc "usage" p) in
       let command = Lang.to_string (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
       let f x = Lang.to_string (Lang.apply f ["",Lang.string x]) in
       let ns =
         Server.register (Pcre.split ~pat:"\\." namespace) "custom commands"
       in
       let usage = if usage = "" then command ^ " <variable>" else usage in
           Server.add ~ns ~usage ~descr command f ;
         Lang.unit)

(** Data conversions. *)

let () =
  (** Register as [name] the function which composes [in_value],[func] and
    * [out_value], and returns [default] in exceptional cases -- which MUST not
    * occur when default is not supplied. *)
  let register_tt doc name cat
        func ?default in_type in_value out_value out_type =
    add_builtin name ~cat ~descr:("Convert "^doc^".")
      (let p = ["",in_type,None,None] in
         match default with
           | None -> p
           | Some d -> ("default",out_type,Some d,None)::p)
      out_type
      (fun p ->
         try
           out_value (func (in_value (List.assoc "" p)))
         with _ -> List.assoc "default" p)
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
      (fun v -> Lang.int v) Lang.int_t ;
    register_tts
      "float" float_of_string ~default:(Lang.float 0.)
      (fun v -> Lang.float v) Lang.float_t ;
    register_tts
      "bool" bool_of_string ~default:(Lang.bool false)
      (fun v -> Lang.bool v) Lang.bool_t ;
    register_tti "float" float_of_int (fun v -> Lang.float v) Lang.float_t ;
    register_tti "bool" (fun v -> v = 1) (fun v -> Lang.bool v) Lang.bool_t ;
    register_ttf "int" int_of_float (fun v -> Lang.int v) Lang.int_t ;
    register_ttf "bool" (fun v -> v = 1.) (fun v -> Lang.bool v) Lang.bool_t

let () =
  add_builtin "string_of" ~cat:String
    ~descr:"Return the representation of a value."
    ["",Lang.univ_t 1,None,None] Lang.string_t
    (fun p ->
       match List.assoc "" p with
         | {Lang.value=Lang.String s} -> Lang.string s
         | v -> Lang.string (Lang.print_value v))

let () =
  add_builtin "ignore" ~descr:"Convert anything to unit, preventing warnings."
    ~cat:Control
    ["",Lang.univ_t 1,None,None] Lang.unit_t
    (fun _ -> Lang.unit)

(* More liquidsoap stuff: sources and requests *)

let () =
  add_builtin "source.skip" ~cat:Liq ~descr:"Skip to the next track."
    [ "",Lang.source_t,None,None ] Lang.unit_t
    (fun p -> (Lang.to_source (List.assoc "" p))#abort_track ; Lang.unit)

let () =
  add_builtin "source.id" ~cat:Liq ~descr:"Get one source's identifier."
    [ "",Lang.source_t,None,None ] Lang.string_t
    (fun p -> Lang.string (Lang.to_source (List.assoc "" p))#id)

let () =
  add_builtin "request.create" ~cat:Liq
    ~descr:"Create a request. Creation may fail if there is no available RID, \
            which cannot be detected currently: in that case one will obtain \
            a request that will fail to be resolved."
    ["indicators",Lang.list_t Lang.string_t,Some (Lang.list []),None;
     "persistent",Lang.bool_t,Some (Lang.bool false),None;
     "audio",Lang.bool_t,Some (Lang.bool true),
     Some "If set, resolving includes checking that the resulting file \
           can be decoded as audio." ;
     "",Lang.string_t,None,None]
    Lang.request_t
    (fun p ->
       let indicators = List.assoc "indicators" p in
       let audio = Lang.to_bool (List.assoc "audio" p) in
       let persistent = Lang.to_bool (List.assoc "persistent" p) in
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
         Lang.request
           (if not audio then
              Request.create_raw ~persistent ~indicators initial
           else
              match Request.create ~persistent ~indicators initial with
                | Some r -> Some (Request.to_raw r)
                | None -> None))

let () =
  add_builtin "request.resolve" ~cat:Liq
    ["timeout",Lang.float_t,Some (Lang.float 30.),
     Some "Limit in seconds to the duration of the resolving." ;
     "",Lang.request_t,None,None]
    Lang.bool_t
    ~descr:"Resolve a request, i.e. attempt to get a valid local file. \
            The operation can take some time. Return true if the resolving \
            was successful, false otherwise (timeout or invalid URI)."
    (fun p ->
       let timeout = Lang.to_float (List.assoc "timeout" p) in
          match Lang.to_request_raw (List.assoc "" p) with
            | Some r ->
                Lang.bool (Request.Resolved = Request.resolve r timeout)
            | _ -> Lang.bool false)

let () =
  add_builtin "request.ready" ~cat:Liq
    ~descr:"Check if a request is ready, i.e. is associated to a valid \
            local file. Unless the initial URI was such a file, a request \
            has to be resolved before being ready."
    ["", Lang.request_t,None,None] Lang.bool_t
    (fun p ->
       match Lang.to_request_raw (List.assoc "" p) with
         | Some e -> Lang.bool (Request.is_ready e)
         | _ -> Lang.bool false)

let () =
  add_builtin "request.filename" ~cat:Liq
    ~descr:"Return a valid local filename if the request is ready, \
            and the empty string otherwise."
    [ "",Lang.request_t,None,None ] Lang.string_t
    (fun p ->
       match Lang.to_request_raw (List.assoc "" p) with
         | None -> Lang.string ""
         | Some r ->
             Lang.string (match Request.get_filename r with
                            | Some f -> f | None -> ""))

let () =
  add_builtin "request.destroy" ~cat:Liq
    ~descr:"Destroying a request causes any temporary associated file \
            to be deleted, and releases its RID. Persistent requests resist \
            to destroying, unless forced."
    ["force",Lang.bool_t,Some (Lang.bool false),
     Some "Destroy the request even if it is persistent." ;
     "",Lang.request_t,None,None ]
    Lang.unit_t
    (fun p ->
       let force = Lang.to_bool (List.assoc "force" p) in
         begin match Lang.to_request_raw (List.assoc "" p) with
            | Some e -> Request.destroy ~force e
            | None -> ()
         end ;
         Lang.unit)

let () =
  add_builtin "file.duration" ~cat:Liq
    ["",Lang.string_t,None,None] Lang.float_t
    ~descr:"Compute the duration in seconds of audio data contained in \
            a file. The computation may be expensive. \
            Returns -1. if computation failed, typically if the file was \
            not recognized as valid audio."
    (fun p ->
       let f = Lang.to_string (List.assoc "" p) in
         Lang.float (try Request.duration f with Not_found -> -1.))

let () =
  add_builtin "playlist.parse" ~cat:Liq
    ["", Lang.string_t,None,None]
    (Lang.list_t
       (Lang.product_t
          (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t))
          Lang.string_t))
    ~descr:"Try to parse a local playlist. \
            Return a list of (metadata,URI) items, where metadata is a list \
            of (key,value) bindings."
    (fun p ->
       let f = Lang.to_string (List.assoc "" p) in
       let f = Utils.home_unrelate f in
       let channel = open_in f in
       let length = in_channel_length channel in
       let content = String.create length in
         really_input channel content 0 length ;
         close_in channel ;
         try
           let _,l = Playlist_parser.search_valid content in
           let process m =
             let f (n,v) =
               Lang.product (Lang.string n) (Lang.string v)
             in
               Lang.list (List.map f m)
           in
           let process (m,uri) =
             Lang.product (process m) (Lang.string uri)
           in
             Lang.list (List.map process l)
         with
           | _ -> Lang.list [])

(** Sound utils. *)

let () =
  add_builtin "dB_of_lin" ~cat:Math ["",Lang.float_t,None,None] Lang.float_t
    ~descr:"Convert linear scale into decibels."
    (fun p ->
       let x = Lang.to_float (Lang.assoc "" 1 p) in
         Lang.float (Sutils.dB_of_lin x)) ;
  add_builtin "lin_of_dB" ~cat:Math ["",Lang.float_t,None,None] Lang.float_t
    ~descr:"Convert decibels into linear scale."
    (fun p ->
       let x = Lang.to_float (Lang.assoc "" 1 p) in
         Lang.float (Sutils.lin_of_dB x))

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
                 ~descr:"List available interactive variables."
        (fun s ->
           String.concat "\n"
             (List.map
                (fun (_,v) ->
                   Printf.sprintf "%s : %s" v.name (Lang_types.print v.kind))
                (List.sort (fun (m,_) (n,_) -> compare m n) !variables)))

  let () =
    let usage = "set <variable> = <value>" in
      Server.add ~ns ~usage "set" ~descr:"Set a variable's value."
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
      Server.add ~ns ~usage "get" ~descr:"Get a variable's value."
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
  add_builtin "interactive_float" ~cat:Interaction
    ~descr:"Read a float from an interactive input."
    ["",Lang.string_t,None,None; "",Lang.float_t,None,None ]
    (Lang.fun_t [] Lang.float_t)
    (fun p ->
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
         Lang.val_fun [] (fun p -> Lang.float !v))

let () = (* TODO do not print when daemonized *)
  add_builtin "print" ~cat:Interaction ~descr:"Print on standard output."
    ["newline",Lang.bool_t,Some (Lang.bool true),
     Some "If true, a newline is added after displaying the value." ;
     "",Lang.univ_t 1,None,None]
    Lang.unit_t
    (fun p ->
       let nl = Lang.to_bool (List.assoc "newline" p) in
       let v = List.assoc "" p in
       let v =
         match v.Lang.value with Lang.String s -> s | _ -> Lang.print_value v
       in
       let v = if nl then v^"\n" else v in
         print_string v ; flush stdout ;
         Lang.unit)
