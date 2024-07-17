(*****************************************************************************

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

 *****************************************************************************)

open Lang

type category = Sys | Math | String | List | Bool | Liq | Control | Other

let string_of_category = function
  | Sys     -> "System"
  | Math    -> "Math"
  | String  -> "String"
  | List    -> "List"
  | Bool    -> "Bool"
  | Liq     -> "Liquidsoap"
  | Control -> "Control"
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

let () =
  let kind =
    Lang.fun_t
      [true,"label",string_t;true,"level",int_t;false,"",Lang.string_t]
      Lang.unit_t
  in
  Lang.builtins#register "log"
    ~doc:(mkdoc ~cat:Sys "Log a message." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["label",Some (string "lang");
                    "level",Some (int 3);
                    "",None],[],
                   fun p ->
                     let msg = Lang.to_string (List.assoc "" p) in
                     let label = Lang.to_string (List.assoc "label" p) in
                     let level = Lang.to_int (List.assoc "level" p) in
                       Dtools.Log.log ~label level msg ;
                       Lang.Unit ) }

let () =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.string_t]
      Lang.string_t
  in
  Lang.builtins#register "^"
    ~doc:(mkdoc ~cat:String "Concatenate strings." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let s1 = Lang.to_string (Lang.assoc "" 1 p) in
                     let s2 = Lang.to_string (Lang.assoc "" 2 p) in
                       Lang.String (s1 ^ s2)) }

let () =
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
    { pos = dummy_pos ;
      kind = kind ;
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
    { pos = dummy_pos ;
      kind = kind ;
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
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.unit_t in
  Lang.builtins#register "system"
    ~doc:(mkdoc ~cat:Sys "Shell command call." kind)
    { pos = dummy_pos ;
      kind = kind ;
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
    { pos = dummy_pos ;
      kind = kind ;
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
    { pos = dummy_pos ;
      kind = kind ;
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
  let kind = Lang.fun_t [true,"indicators",Lang.list_t Lang.string_t;
                         false,"",Lang.string_t] Lang.request_t in
    Lang.builtins#register "request"
      ~doc:(mkdoc ~cat:Liq "Create a request." kind)
      { pos = dummy_pos ;
        kind = kind ;
        value = FFI (["indicators",Some (Lang.list []); "",None],[],
                     fun p ->
                       let indicators = List.assoc "indicators" p in
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
                         Request (Request.create ~indicators initial)) }

let () =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
  Lang.builtins#register "quote"
    ~doc:(mkdoc ~cat:String "Escape shell metacharacters." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let s = Lang.to_string (List.assoc "" p) in
                       Lang.String (Filename.quote s)) }

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
      { pos = dummy_pos ;
        kind = kind ;
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
    { pos = dummy_pos ;
      kind = kind ;
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

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.int_t ] Lang.int_t
  in
    Lang.builtins#register "abs"
      ~doc:(mkdoc ~cat:Math "Absolute value." kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None],[],
                     fun p ->
                       Int (abs (Lang.to_int (snd (List.hd p))))) }

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.int_t ; false,"",Lang.int_t] Lang.int_t
  in
  let register_op doc name op =
    Lang.builtins#register name
      ~doc:(mkdoc ~cat:Math (Printf.sprintf "%s of integers." doc) kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_int x) p with
                         | [a;b] -> Int (op a b)
                         | _ -> assert false) }
  in
    register_op "Multiplication" "*" ( * ) ;
    register_op "Division" "/" (/) ;
    register_op "Addition" "+" (+) ;
    register_op "Substraction " "-" (-)

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.float_t ; false,"",Lang.float_t] Lang.float_t
  in
  let register_op doc name op =
    Lang.builtins#register name
      ~doc:(mkdoc ~cat:Math (Printf.sprintf "%s of floats." doc) kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_float x) p with
                         | [a;b] -> Float (op a b)
                         | _ -> assert false) }
  in
    register_op "Multiplication" "*." ( *. ) ;
    register_op "Division" "/." (/.) ;
    register_op "Addition" "+." (+.) ;
    register_op "Substraction " "-." (-.)

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.int_t ; false,"",Lang.int_t] Lang.bool_t
  in
  let register_op name op =
    Lang.builtins#register name
      ~doc:(mkdoc ~cat:Bool "Comparison of integers." kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_int x) p with
                         | [a;b] -> Lang.Bool (op a b)
                         | _ -> assert false) }
  in
    register_op "==" (=) ;
    register_op "<" (<) ;
    register_op "<=" (<=) ;
    register_op ">=" (>=) ;
    register_op ">" (>)

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.bool_t ; false,"",Lang.bool_t] Lang.bool_t
  in
    Lang.builtins#register "and"
      ~doc:(mkdoc ~cat:Bool "Return the conjunction of its arguments" kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_bool x) p with
                         | [a;b] -> Lang.Bool (a && b)
                         | _ -> assert false) } ;
    Lang.builtins#register "or"
      ~doc:(mkdoc ~cat:Bool "Return the disjunction of its arguments" kind)
      { pos = dummy_pos ; kind = kind ;
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
    { pos = dummy_pos ; kind = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     Lang.Bool (not (Lang.to_bool (List.assoc "" p)))) }

let () =
  let kind =
    Lang.fun_t [ false,"",Lang.float_t ;
                 false,"",Lang.fun_t [] Lang.string_t ] Lang.unit_t
  in
    Lang.builtins#register "add_timeout"
      ~doc:(mkdoc ("Call a function every N seconds. "^
                   "If the output of the function is a well-formed float "^
                   "it will be used as the new delay.") ~cat:Control kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       let d,f = match p with
                         | (_,d)::(_,f)::_ -> d,f
                         | _ -> assert false
                       in
                       let d = ref (Lang.to_float d) in
                         ignore (Tutils.create
                                   (fun () ->
                                      while true do
                                        Thread.delay !d ;
                                        let out = Lang.apply f [] in
                                        let out = Lang.to_string out in
                                          try d := float_of_string out with
                                            | _ -> ()
                                      done)
                                   ()
                                   "add_timeout") ;
                         Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "separator", Lang.string_t ;
                 false, "", Lang.string_t ]
               (Lang.list_t Lang.string_t)
  in
    Lang.builtins#register "str.split"
      ~doc:(mkdoc ~cat:String "Split a string at 'separator'." kind )
    { pos = dummy_pos ; kind = kind ;
      value = FFI (["separator", None ; "", None], [],
                   fun p ->
                     let sep = Lang.to_string (List.assoc "separator" p) in
                     let string = Lang.to_string (List.assoc "" p) in
                       Lang.List (List.map Lang.string 
                         (Str.split (Str.regexp_string sep) string))) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.fun_t [false, "", Lang.string_t] Lang.unit_t ;
                 false, "", (Lang.list_t Lang.string_t) ]
               Lang.unit_t
  in
    Lang.builtins#register "list.iter"
      ~doc:(mkdoc ~cat:List
              "Execute a function on every element of a list." kind)
    { pos = dummy_pos ; kind = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let f,l = match p with
                         | (_,f)::(_,l)::_ -> f,l
                         | _ -> assert false
                     in
                     let l = Lang.to_list l in
                       List.iter (fun c -> 
                         ignore (Lang.eval (Lang.apply f ["",c]))) l;
                     Lang.Unit) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.string_t) ;
                 false, "", Lang.int_t ]
               Lang.string_t
  in
  Lang.builtins#register "list.nth"
    ~doc:(mkdoc ~cat:List "Returns the nth element of a list." kind)
    { pos = dummy_pos ;
      kind = kind ;
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
    { pos = dummy_pos ;
      kind = kind ;
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
    Lang.fun_t [ false, "", Lang.list_t (Lang.string_t) ]
               (Lang.list_t Lang.string_t)
  in
  Lang.builtins#register "list.tl"
    ~doc:(mkdoc ~cat:List "Returns the list without its first element." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     let l = List.map
                               Lang.to_string
                               (Lang.to_list (Lang.assoc "" 1 p))
                     in
                       try
                         Lang.List (List.map Lang.string (List.tl l))
                       with
                         Failure "tl" -> Lang.List []) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.string_t ;
                 false, "", Lang.list_t (Lang.string_t) ]
               Lang.bool_t
  in
  Lang.builtins#register "list.mem"
    ~doc:(mkdoc ~cat:List
            "Checks if an element is present within a list." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI ([ "", None ; "", None ], [],
                   fun p ->
                     let e = Lang.to_string (Lang.assoc "" 1 p) in
                     let l = List.map
                               Lang.to_string
                               (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.Bool (List.mem e l)) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.list_t (Lang.string_t) ]
               Lang.int_t
  in
  Lang.builtins#register "list.length"
    ~doc:(mkdoc ~cat:List
            "Returns the length (number of elements) of a list." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI ([ "", None ], [],
                   fun p ->
                     let l = Lang.to_list (Lang.assoc "" 1 p)
                     in
                       Lang.Int (List.length l)) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.string_t ;
                 true, "", Lang.string_t ]
               (Lang.list_t Lang.string_t)
  in
  Lang.builtins#register "execute"
    ~doc:(mkdoc ~cat:Sys "Executes a command." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI ([ "", None ; "", Some (Lang.string "") ], [],
                   fun p ->
                     let c = Lang.to_string (Lang.assoc "" 1 p) in
                     let a = Lang.to_string (Lang.assoc "" 2 p) in
                       let r = try Server.exec (c ^ " " ^ a)
                               with Not_found -> "Command not found!"
                       in
                         Lang.List
                           (List.map Lang.string 
                             (Str.split (Str.regexp_string "\n") r))) }

let () =
  let kind =
    Lang.fun_t [ false, "", Lang.bool_t ;
                 false, "then", Lang.fun_t [] Lang.unit_t ;
                 true , "else", Lang.fun_t [] Lang.unit_t ]
               Lang.unit_t
  in
    Lang.builtins#register "if"
      ~doc:(mkdoc ~cat:Control ~flags:[Lang.Hidden]
              "Execute a function conditionally." kind)
    { pos = dummy_pos ; kind = kind ;
      value = FFI ([ "", None ; "then", None ;
                     "else", Some (Lang.val_fun [] Lang.unit) ], [],
                   fun p ->
                     let c = (List.assoc "" p) in
                     let fy = (List.assoc "then" p) in
                     let fn = (List.assoc "else" p) in
                     let c = Lang.to_bool c in
                       if c
                       then
                         ignore (Lang.eval (Lang.apply fy []))
                       else
                         ignore (Lang.eval (Lang.apply fn [])) ;
                     Lang.Unit) }

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
      { pos = dummy_pos ; kind = kind ;
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
    register_tti "string" string_of_int (fun v -> Lang.String v) Lang.string_t ;
    register_tti "float" float_of_int (fun v -> Lang.Float v) Lang.float_t ;
    register_tti "bool" (fun v -> v = 1) (fun v -> Lang.Bool v) Lang.bool_t ;
    register_ttf "string"
      string_of_float (fun v -> Lang.String v) Lang.string_t ;
    register_ttf "int" int_of_float (fun v -> Lang.Int v) Lang.int_t ;
    register_ttf "bool" (fun v -> v = 1.) (fun v -> Lang.Bool v) Lang.bool_t
