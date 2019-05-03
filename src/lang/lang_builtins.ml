(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Extralib

type category = Sys | Math | String | List | Bool | Pair
              | Liq | Control | Interaction | Other

let string_of_category = function
  | Sys     -> "System"
  | Math    -> "Math"
  | String  -> "String"
  | List    -> "List"
  | Pair    -> "Pair"
  | Bool    -> "Bool"
  | Liq     -> "Liquidsoap"
  | Control -> "Control"
  | Interaction -> "Interaction"
  | Other   -> "Other"

let add_builtin ~cat ~descr ?flags name proto ret_t f =
  Lang.add_builtin ~category:(string_of_category cat)
    ~descr ?flags name proto ret_t (fun p _ -> f p)

let () =
  Lang.add_builtin_base
    ~category:(string_of_category Liq)
    ~descr:"Liquidsoap version string."
    "liquidsoap.version"
    (Lang.String Configure.version)
    Lang.string_t ;
  List.iter
    (fun (name,kind,str) ->
       Lang.add_builtin_base
         ~category:(string_of_category Liq)
         ~descr:(Printf.sprintf "Liquidsoap's %s." kind)
         ("configure."^name)
         (Lang.String str)
         Lang.string_t)
    [ ("libdir", "library directory", Configure.libs_dir) ;
      ("rundir", "PID file directory", Configure.rundir) ;
      ("logdir", "logging directory", Configure.logdir) ;
      ("default_font", "default font file", Configure.default_font) ]

let () =
  Lang.add_builtin_base
    ~category:(string_of_category Sys)
    ~descr:"Type of OS running liquidsoap."
    "os.type"
    (Lang.String Sys.os_type)
    Lang.string_t

let () =
  Lang.add_builtin_base
    ~category:(string_of_category Sys)
    ~descr:"Executable file extension."
    "exe_ext"
    (Lang.String Configure.exe_ext)
    Lang.string_t

(** Liquidsoap stuff *)

let log = Lang.log

let add_getters name get_t type_t to_get to_val =
  add_builtin ~cat:Liq ("to_" ^ name ^ "_getter")
    ~descr:("Return a function from a " ^ name ^ " getter")
    ["",get_t 1,None,None]
    (Lang.fun_t [] type_t)
    (fun p ->
      let getter =
        to_get
          (Lang.assoc "" 1 p)
      in
      Lang.val_fun [] ~ret_t:type_t (fun _ _ ->
        to_val (getter ())));
  add_builtin ~cat:Liq (name ^ "_getter")
    ~descr:("Create a " ^ name ^ " getter")
    ["",get_t 1,None,None]
    (get_t 2)
    (fun p -> List.assoc "" p)

let () =
  add_getters "string" Lang.string_getter_t Lang.string_t Lang.to_string_getter Lang.string;
  add_getters "float" Lang.float_getter_t Lang.float_t Lang.to_float_getter Lang.float;
  add_getters "int" Lang.int_getter_t Lang.int_t Lang.to_int_getter Lang.int;
  add_getters "bool" Lang.bool_getter_t Lang.bool_t Lang.to_bool_getter Lang.bool

let () =
  add_builtin ~cat:Liq "eval"
    ~descr:"Evaluate a string as an expression in the toplevel environment."
    ~flags:[Lang.Hidden]
    ["",Lang.string_t,None,None]
    Lang.string_t
    (fun p ->
       let s = Lang.to_string (Lang.assoc "" 1 p) in
         match Lang.eval s with
           | None -> Lang.string ""
           | Some v -> Lang.string (Lang.print_value v))

let () =
  add_builtin "clock.assign_new" ~cat:Liq
    ~descr:"Create a new clock and assign it to a list of sources."
    [ ("id", Lang.string_t, Some (Lang.string ""),
       Some "Identifier for the new clock. The default empty string means \
             that the identifier of the first source will be used.") ;
      ("sync", Lang.bool_t, Some (Lang.bool true),
        Some "Do not synchronize the clock on regular wallclock time, \
              but try to run as fast as possible (CPU burning mode).") ;
      ("", Lang.list_t (Lang.source_t (Lang.univ_t 1)), None,
       Some "List of sources to which the new clock will be assigned") ]
    Lang.unit_t
    (fun p ->
       match Lang.to_list (List.assoc "" p) with
         | [] -> Lang.unit
         | (hd::_) as sources ->
             let sync = Lang.to_bool (List.assoc "sync" p) in
             let id = Lang.to_string (List.assoc "id" p) in
             let id =  if id = "" then (Lang.to_source hd)#id else id in
             let clock = new Clock.wallclock ~sync id in
               List.iter
                 (fun s ->
                    try
                      let s = Lang.to_source s in
                        Clock.unify s#clock (Clock.create_known clock)
                    with
                      | Source.Clock_conflict (a,b) ->
                          raise (Lang.Clock_conflict
                                   (s.Lang.t.Lang_types.pos,a,b))
                      | Source.Clock_loop (a,b) ->
                          raise (Lang.Clock_loop
                                   (s.Lang.t.Lang_types.pos,a,b)))
                 sources ;
               Lang.unit)

let () =
  add_builtin "clock.unify" ~cat:Liq
    ~descr:"Enforce that a list of sources all belong to the same clock."
    [ ("", Lang.list_t (Lang.source_t (Lang.univ_t 1)), None, None) ]
    Lang.unit_t
    (fun p ->
       let l = List.assoc "" p in
         try
           match Lang.to_source_list l with
             | [] -> Lang.unit
             | hd::tl ->
                 List.iter (fun s -> Clock.unify hd#clock s#clock) tl ;
                 Lang.unit
         with
           | Source.Clock_conflict (a,b) ->
               raise (Lang.Clock_conflict
                        (l.Lang.t.Lang_types.pos,a,b))
           | Source.Clock_loop (a,b) ->
               raise (Lang.Clock_loop
                        (l.Lang.t.Lang_types.pos,a,b)))

let () =
  let t = Lang.product_t Lang.string_t Lang.int_t in
    add_builtin "get_clock_status" ~cat:Liq
      ~descr:"Get the current time for all allocated clocks."
      []
      (Lang.list_t t)
      (fun _ ->
         let l =
           Clock.fold
             (fun clock l ->
                Lang.product
                  (Lang.string clock#id)
                  (Lang.int clock#get_tick)
                :: l)
             []
         in
         let l =
           Lang.product
             (Lang.string "uptime")
             (Lang.int
                (int_of_float
                   (Utils.uptime () /. Lazy.force Frame.duration)))
           :: l
         in
           Lang.list ~t l)

let () =
  if Sys.os_type <> "Win32" then
   begin
   (** The type of the test function for external decoders.
     * Return is one of:
     * . 0: no audio
     * . -1: audio with unknown number of channels.
     * . x >= 1: audio with a fixed number (x) of channels. *)
   let test_file_t = Lang.fun_t [false,"",Lang.string_t] Lang.int_t in
   let test_arg =
     "test",test_file_t,None,
     Some "Function used to determine if a file should \
           be decoded by the decoder. Returned values are: \
           0: no decodable audio, -1: decodable audio but \
           number of audio channels unknown, x: fixed number of decodable \
           audio channels."
   in
   let test_f f =
     (fun file ->
        Lang.to_int (Lang.apply f ~t:Lang.int_t ["",Lang.string file]))
   in
    add_builtin "add_decoder" ~cat:Liq
      ~descr:"Register an external decoder. \
              The encoder should output in WAV format \
              to his standard output (stdout) and read \
              data from its standard input (stdin)."
      ["name",Lang.string_t,None,Some "Format/decoder's name." ;
       "description",Lang.string_t,None,Some "Description of the decoder.";
       "mimes",Lang.list_t (Lang.string_t),
       Some (Lang.list ~t:Lang.string_t []),
       Some "List of mime types supported by this decoder \
             for decoding streams."; 
       test_arg;
       "",Lang.string_t,None,Some "Process to start."]
      Lang.unit_t
      (fun p ->
         let process = Lang.to_string (Lang.assoc "" 1 p) in
         let name = Lang.to_string (List.assoc "name" p) in
         let descr = Lang.to_string (List.assoc "description" p) in
         let mimes =
           List.map Lang.to_string (Lang.to_list (List.assoc "mimes" p))
         in
         let test = List.assoc "test" p in
         External_decoder.register_stdin name descr mimes (test_f test) process;
         Lang.unit) ;

    let process_t = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
    add_builtin "add_oblivious_decoder" ~cat:Liq
      ~descr:"Register an external file decoder. \
              The encoder should output in WAV format \
              to his standard output (stdout) and read \
              data from the file it receives. The estimated \
              remaining duration for this decoder will be \
              unknown until the @buffer@ last seconds of the file. \
              If possible, it is recommended \
              to decode from stdin and use @add_decoder@."
      ["name",Lang.string_t,None,Some "Format/decoder's name." ;
       "description",Lang.string_t,None,Some "Description of the decoder.";
       test_arg;
       "buffer", Lang.float_t, Some (Lang.float 5.), None;
       "",process_t,None,Some "Process to start. The function \
                           takes the filename as argument and \
                           returns the process to start." ]
      Lang.unit_t
      (fun p ->
         let f = Lang.assoc "" 1 p in
         let name = Lang.to_string (List.assoc "name" p) in
         let descr = Lang.to_string (List.assoc "description" p) in
         let prebuf = Lang.to_float (List.assoc "buffer" p) in
         let process file =
           Lang.to_string (Lang.apply f ~t:Lang.string_t ["",Lang.string file])
         in
         let test = List.assoc "test" p in
         External_decoder.register_oblivious
           name descr
           (test_f test) process prebuf ;
         Lang.unit)
   end

let () =
  add_builtin "metadata.export" ~cat:Liq
   ~descr:"Filter-out internal metadata."
   ["",Lang.metadata_t,None,None] Lang.metadata_t
   (fun p ->
     Lang.metadata
      (Meta_format.to_metadata
        (Meta_format.export_metadata
          (Lang.to_metadata
            (List.assoc "" p)))))

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

(* Special characters that must be escaped *)
let special_chars =
  (* Control chars between 0x00 and 0x1f *)
  let rec escaped p l =
    if p <= 0x1f then
      escaped (p+1) ((Char.chr p)::l)
    else
      List.rev l
  in
  (* We also add 0x7F (DEL) and the
   * usual '"' and '\' *)
  escaped 0 ['"'; '\\'; '\x7F']

let register_escape_fun ~name ~descr ~escape
                        ~escape_char =
  let escape ~special_char ~escape_char s =
    let b = Buffer.create (String.length s) in
    let f = Format.formatter_of_buffer b in
    escape ~special_char ~escape_char f s ;
    Format.pp_print_flush f () ;
    Buffer.contents b
  in
  let special_chars =
    Lang.list ~t:Lang.string_t
     (List.map Lang.string
      (List.map (String.make 1)
        special_chars))
  in
  let escape_char p _ =
    let v = List.assoc "" p in
    Lang.string
     (escape_char
       (Lang.to_string v).[0])
  in
  let escape_char =
    Lang.val_fun
     ["","",Lang.string_t,None]
     ~ret_t:Lang.string_t
     escape_char
  in
  add_builtin name ~cat:String ~descr
    [ "special_chars", Lang.list_t Lang.string_t,
      Some (special_chars),
      Some ("List of characters that should be escaped. The first \
             character of each element in the list is considered.") ;
      "escape_char",
      Lang.fun_t [false,"",Lang.string_t] Lang.string_t,
      Some escape_char,
      Some "Function used to escape a character." ;
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let s = Lang.to_string (List.assoc "" p) in
       let special_chars =
         List.map
          (fun s -> s.[0])
            (List.map Lang.to_string
              (Lang.to_list (List.assoc "special_chars" p)))
       in
       let special_char c = List.mem c special_chars in
       let f = List.assoc "escape_char" p in
       let escape_char c =
         Lang.to_string
          (Lang.apply f ~t:Lang.string_t
             ["",Lang.string (String.make 1 c)])
       in
       Lang.string (escape ~special_char ~escape_char s))

let () =
  let escape ~special_char ~escape_char f s =
    Utils.escape ~special_char ~escape_char f s
  in
  register_escape_fun ~name:"string.escape"
                      ~descr:"Escape special charaters in a \
                              string. String is parsed char by char. \
                              See @string.utf8.escape@ for an UTF8-aware \
                              parsing function."
                      ~escape ~escape_char:Utils.escape_char ;
  let escape ~special_char ~escape_char f s =
    Utils.escape_utf8 ~special_char ~escape_char f s
  in
  register_escape_fun ~name:"string.utf8.escape"
                      ~descr:"Escape special charaters in an UTF8 \
                              string."
                      ~escape ~escape_char:Utils.escape_utf8_char

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
         Lang.list ~t:Lang.string_t
           (List.map Lang.string (Pcre.split ~rex string)))

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
    Lang.metadata_t
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
           ~t:(Lang.product_t Lang.string_t Lang.string_t)
           (List.map
              (fun (x,y) ->
                 Lang.product (Lang.string x) (Lang.string y))
              l)
       with
         | Not_found ->
             Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) [])

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
  add_builtin "string.recode" ~cat:String
    ~descr:"Convert a string. Effective only if Camomile \
            is enabled."
    [ "in_enc", Lang.string_t, Some (Lang.string ""),
      Some "Input encoding. Autodetected if empty." ;
      "out_enc", Lang.string_t, Some (Lang.string "UTF-8"),
      Some "Output encoding." ;
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let in_enc =
         match Lang.to_string (List.assoc "in_enc" p) with
           | "" -> None
           | s  -> Some s
       in
       let out_enc =
         Lang.to_string (List.assoc "out_enc" p)
       in
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string (Configure.recode_tag ?in_enc ~out_enc string))

let () =
  add_builtin "string.length" ~cat:String
    ~descr:"Get the length of a string."
    [ "", Lang.string_t, None, None ]
    Lang.int_t
    (fun p ->
       let string = Lang.to_string (List.assoc "" p) in
       Lang.int (String.length string))

let () =
  add_builtin "string.sub" ~cat:String
    ~descr:"Get a substring of a string. Returns \"\" if \
            no such substring exists."
    [ "", Lang.string_t, None, None;
      "start", Lang.int_t, None, Some "Return a sub string \
         starting at this position. First position is 0.";
      "length", Lang.int_t, None, Some "Return a sub string \
         of @length@ characters." ]
    Lang.string_t
    (fun p ->
       let start = Lang.to_int (List.assoc "start" p) in
       let len = Lang.to_int (List.assoc "length" p) in
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string
         (try
           String.sub string start len
          with Invalid_argument _ -> ""))

let () =
  add_builtin "string.case" ~cat:String
    ~descr:"Convert a string to lower or upper case."
    [ "lower", Lang.bool_t, Some (Lang.bool true),
      Some "Convert to lower case if true and uppercase otherwise.";
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let lower = Lang.to_bool (List.assoc "lower" p) in
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string
         (if lower then
           String.lowercase_ascii string
          else
           String.uppercase_ascii string))

let () =
  add_builtin "string.trim" ~cat:String
    ~descr:"Return a string without leading and trailing whitespace."
    ["", Lang.string_t, None, None] Lang.string_t
    (fun p ->
       Lang.string (String.trim
         (Lang.to_string (List.assoc "" p))))

let () =
  add_builtin "string.capitalize" ~cat:String
    ~descr:"Return a string with the first character set to upper case \
            (capitalize), or to lower case (uncapitalize)."
    [ "capitalize", Lang.bool_t, Some (Lang.bool true),
        Some "Capitalize if true, uncapitalize otherwise";
      "space_sensitive", Lang.bool_t, Some (Lang.bool true),
        Some "Capitalize each space separated sub-string.";
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let cap = Lang.to_bool (List.assoc "capitalize" p) in
       let space_sensitive = Lang.to_bool (List.assoc "space_sensitive" p) in
       let string = Lang.to_string (List.assoc "" p) in
       let f s =
           if cap then
             String.capitalize_ascii s
           else
             String.uncapitalize_ascii s
      in
      Lang.string
      (if space_sensitive then
        let l = Pcre.split ~pat:" " string in
	let l = List.map f l in
	String.concat " " l
      else
        f string))

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
           Lang.apply ~t:Lang.string_t subst [("",Lang.string s)]
         in
         Lang.to_string ret
       in
       let rex = Pcre.regexp pattern in
       Lang.string (Pcre.substitute ~rex ~subst string))

let () =
  add_builtin "base64.decode" ~cat:String
    ~descr:"Decode a Base64 encoded string."
    [ "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string (Utils.decode64 string))

let () =
  add_builtin "base64.encode" ~cat:String
    ~descr:"Encode a string in Base64."
    [ "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string (Utils.encode64 string))

let () =
  add_builtin "url.decode" ~cat:String
    ~descr:"Decode an encoded url (e.g. \"%20\" becomes \" \")."
    [ "plus", Lang.bool_t, Some (Lang.bool true),None;
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let plus = Lang.to_bool (List.assoc "plus" p) in
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string (Http.url_decode ~plus string))

let () =
  add_builtin "url.encode" ~cat:String
    ~descr:"Encode an url (e.g. \" \" becomes \"%20\")."
    [ "plus", Lang.bool_t, Some (Lang.bool true),None;
      "", Lang.string_t, None, None ]
    Lang.string_t
    (fun p ->
       let plus = Lang.to_bool (List.assoc "plus" p) in
       let string = Lang.to_string (List.assoc "" p) in
       Lang.string (Http.url_encode ~plus string))

let () =
  add_builtin "%" ~cat:String
    ~descr:"<code>pattern % [...,(k,v),...]</code> \
            changes in the pattern occurences of:\n\
             \ - <code>$(k)</code> into <code>v</code>;\n\
             \ - <code>$(if $(k2),\"a\",\"b\")</code> into \
                 \"a\" if k2 is found in the list, \"b\" otherwise."
    ["",Lang.string_t,None,None ;
     "",Lang.metadata_t,None,None]
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
         Lang.string (Utils.quote s))

(** Operations on lists. *)

let () =
  (* TODO It would be good to generalize this one but we'd need a way to handle
   *      errors. *)
  add_builtin "_[_]" ~cat:List
    ~descr:"l[k] returns the first v such that \
            (k,v) is in the list l (or \"\" if no such v exists)."
    ["",Lang.string_t,None,None ;
     "",Lang.metadata_t,None,None]
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

exception Found_assoc of (Lang.value*Lang.value)

let assoc_value k l =
  try
    List.iter (fun ((k',_) as el) ->
      if compare_value k k' == 0 then
        raise (Found_assoc el)) l;
    raise Not_found
  with Found_assoc v -> v

let () =
  let t1 = Lang.univ_t 1 in
  let t2 = Lang.univ_t 2 in
  let lt =
    Lang.list_t (Lang.product_t t1 t2)
  in
  add_builtin "list.assoc" ~cat:List
    ~descr:"Generalized l[k] with default value."
    ["default",t2,None,Some "Default value if key does not exist";
     "",t1,None,None ;
     "",lt,None,None] t2
    (fun p ->
      let default = List.assoc "default" p in
       let k = Lang.assoc "" 1 p in
       let l =
         List.map Lang.to_product
           (Lang.to_list (Lang.assoc "" 2 p))
       in
       try
         snd(assoc_value k l)
       with Not_found -> default)

let () =
  let t1 = Lang.univ_t 1 in
  let t2 = Lang.univ_t 2 in
  let lt =
    Lang.list_t (Lang.product_t t1 t2)
  in
  add_builtin "list.remove_assoc" ~cat:List
    ~descr:"Remove the first pair from an associative list."
    ["",t1,None,Some "Key of pair to be removed";
     "",lt,None,Some "List of pairs (key,value)"] lt
    (fun p ->
       let k = Lang.assoc "" 1 p in
       let l = Lang.assoc "" 2 p in
       let t = Lang.of_list_t l.Lang.t in
       let l =
         List.map Lang.to_product
           (Lang.to_list (Lang.assoc "" 2 p))
       in
       let l =
         try
           let k = fst(assoc_value k l) in
           List.remove_assoc k l
         with Not_found -> l
       in
       Lang.list ~t (List.map (fun (x,y) ->
         Lang.product x y) l))

let () =
  Lang.add_builtin "list.add"
    ~category:(string_of_category List)
    ~descr:"Add an element at the top of a list."
    ["",Lang.univ_t 1,None,None;
     "",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p t ->
      let t = Lang.of_list_t t in
      let x,l =
        match p with
        | ["",x;"",l] -> x,l
        | _ -> assert false
      in
      let l = Lang.to_list l in
      Lang.list ~t (x::l))

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
         List.iter (fun c -> ignore (Lang.apply ~t:Lang.unit_t f ["",c])) l ;
         Lang.unit)

let () =
  Lang.add_builtin "list.map"
    ~category:(string_of_category List)
    ~descr:"Map a function on every element of a list."
    [ "",Lang.fun_t [false, "", Lang.univ_t 1] (Lang.univ_t 2),None,None ;
      "", (Lang.list_t (Lang.univ_t 1)), None, None ]
    (Lang.list_t (Lang.univ_t 2))
    (fun p t ->
       let f,l =
         match p with
           | [("",f);("",l)] -> f,l
           | _ -> assert false
       in
       let t = Lang.of_list_t t in
       let l = Lang.to_list l in
       let l = List.map (fun c -> (Lang.apply ~t f ["",c])) l in
         Lang.list ~t l)

let () =
  Lang.add_builtin "list.mapi"
    ~category:(string_of_category List)
    ~descr:"Map a function on every element of a list, along with its index."
    [ "",Lang.fun_t [false, "", Lang.int_t;false, "", Lang.univ_t 1]
                    (Lang.univ_t 2),None,None ;
      "", (Lang.list_t (Lang.univ_t 1)), None, None ]
    (Lang.list_t (Lang.univ_t 2))
    (fun p t ->
       let f,l =
         match p with
           | [("",f);("",l)] -> f,l
           | _ -> assert false
       in
       let t = Lang.of_list_t t in
       let l = Lang.to_list l in
       let l = List.mapi (fun i -> fun c -> (Lang.apply ~t f ["",Lang.int i;"",c])) l in
         Lang.list ~t l)

let () =
  let t = Lang.list_t (Lang.univ_t 1) in
  Lang.add_builtin "list.randomize"
    ~category:(string_of_category List)
    ~descr:"Shuffle the content of a list."
    ["", t, None, None ] t
    (fun p t ->
       let t = Lang.of_list_t t in
       let l = Array.of_list (Lang.to_list (List.assoc "" p)) in
       Utils.randomize l;
       Lang.list ~t (Array.to_list l))

let () =
  add_builtin "list.fold" ~cat:List
    ~descr:"Fold a function on every element of a list: \
            list.fold(f,x1,[e1,..,en]) is f(...f(f(x1,e1),e2)...,en)."
    [ "",
      Lang.fun_t
        [false, "", Lang.univ_t 1; false, "", Lang.univ_t 2]
        (Lang.univ_t 1),
      None,
      Some "Function f for which \
            f(x,e) which will be called on every element e with \
            the current value of x, returning the new value of x.";
      "", Lang.univ_t 1, None,
      Some "Initial value x1, \
            to be updated by successive calls of f(x,e)." ;
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
         List.fold_left (fun x y -> Lang.apply ~t:x.Lang.t f ["",x; "",y]) x l)

let () =
  let t = Lang.univ_t 1 in
  add_builtin "list.nth" ~cat:List
    ~descr:"Get the n-th element of a list \
            (the first element is at position 0), or\
            'default' if element does not exist."
    [ "default",t,None,Some "Default value if key does not exist";
      "",Lang.list_t t,None,None ;
      "",Lang.int_t,None,None ]
    t
    (fun p ->
       let default = List.assoc "default" p in
       try
         List.nth
           (Lang.to_list (Lang.assoc "" 1 p))
           (Lang.to_int (Lang.assoc "" 2 p))
       with _ -> default)

let () =
  let t = Lang.univ_t 1 in 
  add_builtin "list.hd" ~cat:List
    ~descr:"Return the head (first element) of a list, \
            or 'default' if the list is empty."
    [ "default",t,None,Some "Default value if key does not exist";
      "",Lang.list_t t,None,None ]
    t
    (fun p ->
       let default = List.assoc "default" p in
       try
         List.hd (Lang.to_list (Lang.assoc "" 1 p))
       with
         | _ -> default)

let () =
  add_builtin "list.sort" ~cat:List
    ~descr:"Sort a list according to a comparison function."
    ["",
     Lang.fun_t [false,"",Lang.univ_t 1;false,"",Lang.univ_t 1] Lang.int_t,
     None, None ;
     "",Lang.list_t (Lang.univ_t 1),None,None] (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let f = Lang.assoc "" 1 p in
       let sort x y =
         Lang.to_int (Lang.apply ~t:Lang.int_t f ["",x;"",y])
       in
       let l = Lang.assoc "" 2 p in
       Lang.list
         ~t:(Lang.of_list_t l.Lang.t)
         (List.sort sort (Lang.to_list l)))

let () =
  add_builtin "list.filter" ~cat:List
    ~descr:"Filter a list according to a filtering function."
    ["",
     Lang.fun_t [false,"",Lang.univ_t 1] Lang.bool_t,
     None, None ;
     "",Lang.list_t (Lang.univ_t 1),None,None] (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let f = Lang.assoc "" 1 p in
       let filter x =
         Lang.to_bool (Lang.apply ~t:Lang.bool_t f ["",x])
       in
       let l = Lang.assoc "" 2 p in
       Lang.list
         ~t:(Lang.of_list_t l.Lang.t)
         (List.filter filter (Lang.to_list l)))

let () =
  add_builtin "list.tl" ~cat:List
    ~descr:"Return the list without its first element."
    ["",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.assoc "" 1 p in
       let t = Lang.of_list_t l.Lang.t in
       let l = Lang.to_list l in
         match l with
           | [] -> Lang.list ~t []
           | _::tl -> Lang.list ~t tl)

let () =
  add_builtin "list.append" ~cat:List
    ~descr:"Catenate two lists."
    ["",Lang.list_t (Lang.univ_t 1),None,None;
     "",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.assoc "" 1 p in
       let t = Lang.of_list_t l.Lang.t in
       let l = Lang.to_list l in
       let l' = Lang.to_list (Lang.assoc "" 2 p) in
       Lang.list ~t (l@l'))

let () =
  add_builtin "list.remove" ~cat:List
    ~descr:"Remove a value from a list."
    ["",Lang.univ_t 1,None,None;
     "",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let a = Lang.assoc "" 1 p in
       let l = Lang.assoc "" 2 p in
       let t = Lang.of_list_t l.Lang.t in
       let l = Lang.to_list l in
       let l = List.fold_left (fun l' x ->
         if compare_value x a = 0 then l' else x::l') [] l
       in
         Lang.list ~t l)

let () =
  add_builtin "list.rev" ~cat:List
    ~descr:"Revert list order."
    ["",Lang.list_t (Lang.univ_t 1),None,None]
    (Lang.list_t (Lang.univ_t 1))
    (fun p ->
       let l = Lang.assoc "" 1 p in
       let t = Lang.of_list_t l.Lang.t in
       let l = Lang.to_list l in
         Lang.list ~t (List.rev l))

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
  add_builtin "fst" ~cat:Pair
    ~descr:"Get the first component of a pair."
    ["",Lang.product_t (Lang.univ_t 1) (Lang.univ_t 2),None,None]
    (Lang.univ_t 1)
    (fun p -> fst (Lang.to_product (Lang.assoc "" 1 p))) ;
  add_builtin "snd" ~cat:Pair
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
        If the result of the function is positive or null, the \
        task will be scheduled again after this amount of time (in seconds)."
    (fun p ->
       let d = Lang.to_float (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
       let priority =
         if Lang.to_bool (List.assoc "fast" p) then
           Tutils.Maybe_blocking
         else
           Tutils.Blocking
       in
       let rec t d =
         { Duppy.Task.
             priority = priority ;
             events   = [`Delay d] ;
             handler  =
               fun _ ->
	         let d = Lang.to_float (Lang.apply ~t:Lang.float_t f []) in
                 if d >= 0. then [t d] else [] }
       in
         Duppy.Task.add Tutils.scheduler (t d);
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
      Lang.list ~t:Lang.string_t (List.map Lang.string (Pcre.split ~pat:"\n" r))
  in
  add_builtin "server.execute"
    ~cat ~descr params return_t execute

let () =
  Lang.add_builtin "if"
    ~category:(string_of_category Control)
    ~descr:"The basic conditional."
    ~flags:[Lang.Hidden]
    [ "",Lang.bool_t,None,None ;
      "then", Lang.fun_t [] (Lang.univ_t 1), None,None ;
      "else", Lang.fun_t [] (Lang.univ_t 1), None,None ]
    (Lang.univ_t 1)
    (fun p t ->
       let c = List.assoc "" p in
       let fy = List.assoc "then" p in
       let fn = List.assoc "else" p in
       let c = Lang.to_bool c in
         Lang.apply ~t (if c then fy else fn) [])

let () =
  add_builtin "shutdown" ~cat:Sys ~descr:"Shutdown the application."
    [] Lang.unit_t
    (fun _ ->
      Configure.restart := false ;
      Tutils.shutdown () ;
      Lang.unit) ;
  add_builtin "restart" ~cat:Sys ~descr:"Restart the application."
    [] Lang.unit_t
    (fun _ ->
      Configure.restart := true ;
      Tutils.shutdown () ;
      Lang.unit);
  add_builtin "exit" ~cat:Sys
    ~descr:"Immediately stop the application. This should only be used in extreme cases \
            or to specify an exit value. The recommended way of stopping Liquidsoap is \
            to use shutdown."
    ["", Lang.int_t, None, Some "Exit value."] Lang.unit_t
    (fun p ->
      let n = Lang.to_int (List.assoc "" p) in
      exit n)


let () =
  let reopen name descr f =
    add_builtin name ~cat:Sys ~descr
      ["", Lang.string_t, None, None] Lang.unit_t
      (fun p ->
        let file = Lang.to_string (List.assoc "" p) in
        f file ;
        Lang.unit)
  in
  reopen "reopen.stdin" "Reopen standard input on the given file"
         (Utils.reopen_in stdin) ;
  reopen "reopen.stdout" "Reopen standard output on the given file"
         (Utils.reopen_out stdout) ;
  reopen "reopen.stderr" "Reopen standard error on the given file"
         (Utils.reopen_out stderr)

let () =
  add_builtin "garbage_collect" ~cat:Liq
    ~descr:"Trigger full major garbage collection."
    [] Lang.unit_t
    (fun _ ->
      Gc.full_major () ;
      Lang.unit)

let () =
  let t = Lang.univ_t 1 in
  add_builtin "mutexify" ~cat:Liq
    ~descr:"Protect functions with a mutex to avoid concurrent calls, \
            return original value otherwise."
    ["",t,None,None] t
    (fun p ->
      let m = Mutex.create () in
      let v = List.assoc "" p in
      match v.Lang.value with
        | Lang.Fun (p,args,env,body) ->
            let fn args t = Tutils.mutexify m (fun () ->
              let env =
                List.rev_append args env
              in
              let v = {v with Lang.value =
                Lang.Fun ([],[],env,body)}
              in
              Lang.apply ~t v []) ()
            in
            { v with Lang.value =
                Lang.FFI (p, args, fn) }
        | Lang.FFI (p, args, fn) ->
            let fn args t = Tutils.mutexify m (fun () ->
              fn args t) ()
            in
            { v with Lang.value =
                Lang.FFI (p, args, fn) }
        | _ -> v)

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
    (fun _ ->
       Lang.int (Unix.getpid()))

let () =
  add_builtin "gettimeofday" ~cat:Sys
    []
    Lang.float_t
    ~descr:"Return the current time since \
            00:00:00 GMT, Jan. 1, 1970, in seconds."
    (fun _ ->
       Lang.float (Unix.gettimeofday ()))

let () =
  add_builtin "which" ~cat:Sys
    ~descr:"which(\"progname\") searches for an executable \
            named \"progname\" using directories from the PATH \
            environment variable and returns \"\" if it could not \
            find one."
    ["",Lang.string_t,None,None]
    Lang.string_t
    (fun p ->
      Lang.string (try
          Utils.which ~path:Configure.path (Lang.to_string (List.assoc "" p))
        with Not_found -> ""))

let () =
  let ret_t = Lang.product_t
    (Lang.product_t Lang.string_t Lang.string_t)
    (Lang.product_t Lang.string_t Lang.string_t)
  in
  let env_t =
    Lang.product_t Lang.string_t Lang.string_t
  in
  add_builtin "run_process" ~cat:Sys
    ~descr:"Run a process in a shell environment. Returns: \
            @((stdout,stderr),status)@ where status is one of: \
            @(\"exit\",\"<code>\")@, @(\"killed\",\"<signal number>\")@, \
            @(\"stopped\",\"<signal number>\")@, @(\"exception\",\"<exception description>\")@, \
            @(\"timeout\",\"<run time>\")@."
    ["env",Lang.list_t env_t,
     Some (Lang.list ~t:env_t []),Some "Process environment";
     "inherit_env", Lang.bool_t,
     Some (Lang.bool true), Some "Inherit calling process's environment when \
       @env@ parameter is empty.";
     "timeout",Lang.float_t,Some (Lang.float (-1.)),
     Some "Cancel process after @timeout@ has elapsed. Ignored if negative.";
     "",Lang.string_t,None,Some "Command to run"] ret_t
    (fun p ->
       let env = Lang.to_list
         (List.assoc "env" p)
       in
       let env = List.map (fun e ->
         let (k,v) = Lang.to_product e in
         Lang.to_string k, Lang.to_string v) env
       in
       let inherit_env = Lang.to_bool
         (List.assoc "inherit_env" p)
       in
       let env =
         if env = [] && inherit_env then
           Utils.environment ()
         else
           env
       in
       let timeout = Lang.to_float
         (List.assoc "timeout" p)
       in
       let env = List.map
         (fun (k,v) -> Printf.sprintf "%s=%s" k v)
         env
       in
       let env = Array.of_list env in
       let cmd = Lang.to_string (List.assoc "" p) in
       let buflen = 1024 in
       let out_buf = Buffer.create buflen in
       let err_buf = Buffer.create buflen in
       let on_done (timed_out,status) =
         let stdout = Buffer.contents out_buf in
         let stderr = Buffer.contents err_buf in
         let status, arg =
           match timed_out, status with
             | f, _ when 0. <= f ->
                "timeout", (string_of_float f)
             | _, Some (`Exception e) ->
                "exception", (Printexc.to_string e)
             | _, Some (`Status s) ->
                 begin match s with
                   | Unix.WEXITED c -> "exit", (string_of_int c)
                   | Unix.WSIGNALED s -> "killed", (string_of_int s)
                   | Unix.WSTOPPED s -> "stopped", (string_of_int s)
                 end
             | _ -> assert false
         in
         Lang.product
           (Lang.product (Lang.string stdout) (Lang.string stderr))
           (Lang.product (Lang.string status) (Lang.string arg))
       in
       let synchronous () =
         let ((in_chan,out_ch,err_chan) as p) = Unix.open_process_full cmd env in
         close_out out_ch;
         let pull buf ch =
           let tmp = Bytes.create 1024 in
           let rec aux () =
             let n = input ch tmp 0 1024 in
               if n = 0 then () else
                begin
                 Buffer.add_subbytes buf tmp 0 n;
                 aux()
                end
           in
           aux ()
         in
         pull out_buf in_chan;
         pull err_buf err_chan;
         (-1.,Some (`Status (Unix.close_process_full p)))
       in
       let asynchronous () =
         let out_pipe,in_pipe = Unix.pipe () in
         Tutils.finalize ~k:(fun () ->
           ignore(Unix.close in_pipe);
           ignore(Unix.close out_pipe))
           (fun () ->
             let pull buf fn =
               let bytes = Bytes.create buflen in
               let ret = fn bytes 0 buflen in
               Buffer.add_subbytes buf bytes 0 ret;
              `Continue
             in
             let on_stdout = pull out_buf in
             let on_stderr = pull err_buf in
             let status = ref None in
             let on_stop s =
                status := Some s;
                begin
                  try
                    ignore(Unix.write in_pipe (Bytes.of_string " ") 0 1);
                  with _ -> ()
                end;
                false
             in
             let on_start _ =
               `Stop
             in
             let log = Dtools.Log.make ["lang";"run_process"] in
             let log s = log#f 4 "%s" s in
             let p = Process_handler.run ~env ~on_start ~on_stop
                       ~on_stdout ~on_stderr ~log cmd
             in
             let timed_out =
               try
                 Tutils.wait_for (`Read out_pipe) timeout ;
                 (-1.)
               with Tutils.Timeout f ->
                 Process_handler.kill p;
                 f
             in
             (timed_out, !status))
       in
       on_done (if 0. <= timeout && Tutils.has_started() then asynchronous() else synchronous ()))      

let () =
  let ret_t = Lang.list_t (Lang.product_t Lang.string_t Lang.string_t) in
  add_builtin "environment" ~cat:Sys
    ~descr:"Return the process environment."
    [] ret_t
    (fun _ ->
      let l = Utils.environment () in
      let l = List.map (fun (x,y) -> (Lang.string x, Lang.string y)) l in
      let l = List.map (fun (x,y) -> Lang.product x y) l in
      Lang.list ~t:ret_t l)

let () =
  add_builtin "setenv" ~cat:Sys
    ~descr:"Set the value associated to a variable in the process environment."
    ["",Lang.string_t,None,Some "Variable to be set.";
     "",Lang.string_t,None,Some "Value to set."] Lang.unit_t
    (fun p ->
      let label = Lang.to_string (Lang.assoc "" 1 p) in
      let value = Lang.to_string (Lang.assoc "" 2 p) in
      Unix.putenv label value;
      Lang.unit)

let () =
  add_builtin "log" ~cat:Liq ~descr:"Log a message."
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
  (** Cheap implementation of "getopt" which does not really deserve its name
    * since it has little to do with the standards that getopt(3) implements.
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
         if i = 0 then
           (* Special case so that argv(0) returns the script name *)
           let i = offset - 1 in
           if 0 <= i && i < Array.length argv then
             Lang.string argv.(i)
           else
             Lang.string default
         else if i < List.length opts then
           Lang.string (List.nth opts i)
         else
           Lang.string default)

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
         | {Lang.value=Lang.String s;_} -> Lang.string s
         | v -> Lang.string (Lang.print_value v))

let rec to_json_compact v =
  (* Utils.escape implements
   * JSON's escaping RFC. *)
  let print_s s =
    Utils.escape_string (fun x -> Utils.escape_utf8 x) s
  in
  match v.Lang.value with
    | Lang.Unit -> "null"
    | Lang.Bool b -> Printf.sprintf "%b" b
    | Lang.Int  i -> Printf.sprintf "%i" i
    | Lang.String s -> print_s s
    (* JSON specs do not allow a trailing . *)
    | Lang.Float  n ->
          let s = string_of_float n in
          let s = Printf.sprintf "%s" s in
          if s.[ (String.length s) - 1 ] = '.' then
            Printf.sprintf "%s0" s
          else
            s
    | Lang.List l ->
        (* Convert (string*'a) list to object *)
        begin
         try
          let t = v.Lang.t in
          let t = Lang.of_list_t t in
          let (t,_) = Lang.of_product_t t in
          let compare = Lang_types.( <: ) in
          ignore(compare t Lang.string_t);
          let l =
            List.map (fun x ->
                        let (x,y) = Lang.to_product x in
                        Printf.sprintf "%s:%s"
                          (to_json_compact x) (to_json_compact y))
                      l
          in
          Printf.sprintf "{%s}" (String.concat "," l)
         with _ ->
               Printf.sprintf "[%s]"
                (String.concat ","
                  (List.map to_json_compact l))
        end
    | Lang.Product (p,q) ->
       Printf.sprintf "[%s,%s]"  (to_json_compact p) (to_json_compact q)
    | Lang.Source _ -> "\"<source>\""
    | Lang.Ref v -> Printf.sprintf  "{\"reference\":%s}" (to_json_compact !v)
    | Lang.Encoder e -> print_s (Encoder.string_of_format e)
    | Lang.Request _ -> "\"<request>\""
    | Lang.FFI _
    | Lang.Fun _ -> "\"<fun>\""

let rec to_json_pp f v =
  match v.Lang.value with
    | Lang.List l ->
        (* Convert (string*'a) list to object *)
        begin
         try
          let t = v.Lang.t in
          let t = Lang.of_list_t t in
          let (t,_) = Lang.of_product_t t in
            let compare = Lang_types.( <: ) in
            ignore(compare t Lang.string_t);
            let print f l =
              let len = List.length l in
              let f pos x =
                let (x,y) = Lang.to_product x in
                if pos != len - 1 then
                  Format.fprintf f "%a: %a,@;<1 0>"
                    to_json_pp x to_json_pp y
                else
                  Format.fprintf f "%a: %a"
                    to_json_pp x to_json_pp y ;
                pos+1
              in
              ignore(List.fold_left f 0 l)
            in
            Format.fprintf f "@[{@;<1 1>@[%a@]@;<1 0>}@]" print l
         with _ ->
               let print f l =
                 let len = List.length l in
                 let f pos x =
                   if pos < len -1 then
                     Format.fprintf f "%a,@;<1 0>"
                       to_json_pp x
                   else
                     Format.fprintf f "%a"
                        to_json_pp x ;
                 pos+1
                 in
                 ignore(List.fold_left f 0 l)
               in
               Format.fprintf f "@[[@;<1 1>@[%a@]@;<1 0>]@]" print l
        end
    | Lang.Product (p,q) ->
       Format.fprintf f
         "@[[@;<1 1>@[%a,@;<1 0>%a@]@;<1 0>]@]"
         to_json_pp p to_json_pp q
    | Lang.Ref v ->
       Format.fprintf  f
         "@[{@;<1 1>@[\"reference\":@;<0 1>%a@]@;<1 0>}@]"
         to_json_pp !v
    | _ -> Format.fprintf f "%s" (to_json_compact v)

let to_json_pp v =
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  ignore(to_json_pp f v);
  Format.pp_print_flush f ();
  Buffer.contents b

let to_json ~compact v =
  if compact then
     to_json_compact v
  else
     to_json_pp v

let () =
  add_builtin "json_of" ~cat:String
    ~descr:"Convert a value to a json string."
     ["compact",Lang.bool_t,Some (Lang.bool false),
      Some "Output compact text.";
      "",Lang.univ_t 1,None,None] Lang.string_t
    (fun p ->
      let compact = Lang.to_bool (List.assoc "compact" p) in
      let v = to_json ~compact (List.assoc "" p) in
      Lang.string v)

let () =
  add_builtin "ignore" ~descr:"Convert anything to unit, preventing warnings."
    ~cat:Control
    ["",Lang.univ_t 1,None,None] Lang.unit_t
    (fun _ -> Lang.unit)

(* More liquidsoap stuff: sources and requests *)

let () =
  add_builtin "source.skip" ~cat:Liq ~descr:"Skip to the next track."
    [ "",Lang.source_t (Lang.univ_t 1),None,None ] Lang.unit_t
    (fun p -> (Lang.to_source (List.assoc "" p))#abort_track ; Lang.unit)

let () =
  add_builtin "source.seek" ~cat:Liq
    ~descr:"Seek forward, in seconds. \
            Returns the amount of time effectively seeked."
    [ "",Lang.source_t (Lang.univ_t 1),None,None;
      "",Lang.float_t,None,None ] Lang.float_t
    (fun p ->
       let s = Lang.to_source (Lang.assoc "" 1 p) in
       let time = Lang.to_float (Lang.assoc "" 2 p) in
       let len = Frame.master_of_seconds time in
       let ret = s#seek len in
       Lang.float (Frame.seconds_of_master ret))

let () =
  add_builtin "source.id" ~cat:Liq ~descr:"Get one source's identifier."
    [ "",Lang.source_t (Lang.univ_t 1),None,None ] Lang.string_t
    (fun p -> Lang.string (Lang.to_source (List.assoc "" p))#id)

let () =
  add_builtin "source.fallible" ~cat:Liq
    ~descr:"Indicate if a source may fail, i.e. may not be ready to stream."
    [ "",Lang.source_t (Lang.univ_t 1),None,None ] Lang.bool_t
    (fun p ->
      Lang.bool ((Lang.to_source (List.assoc "" p))#stype == Source.Fallible))

let () =
  add_builtin "source.is_ready" ~cat:Liq
    ~descr:"Indicate if a source is ready to stream, or currently streaming."
    [ "", Lang.source_t (Lang.univ_t 1), None, None ] Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#is_ready)

let () =
  add_builtin "source.remaining" ~cat:Liq
    ~descr:"Estimation of remaining time in the current track."
    [ "", Lang.source_t (Lang.univ_t 1), None, None ] Lang.float_t
    (fun p ->
       let r = (Lang.to_source (List.assoc "" p))#remaining in
       let f = if r = -1 then infinity else Frame.seconds_of_master r in
         Lang.float f)

let () =
  add_builtin "source.shutdown" ~cat:Liq ~descr:"Desactivate a source."
    [ "", Lang.source_t (Lang.univ_t 1), None, None ] Lang.unit_t
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
        (Clock.get s#clock)#detach
           (fun (s':Source.active_source) -> (s':>Source.source)=s) ;
        Lang.unit)

let () =
  let s_t =
    let v = Lang.variable_t in
      Lang.source_t (Lang.frame_kind_t ~audio:v ~video:v ~midi:v)
  in
  add_builtin "source.init" ~cat:Liq
    ~descr:"Simultaneously initialize sources, \
            return the sublist of sources that failed to initialized."
    [ "", Lang.list_t s_t, None, None ] (Lang.list_t s_t)
    (fun p ->
       let l = Lang.to_list (List.assoc "" p) in
       let l = List.map Lang.to_source l in
       let l =
         (* TODO this whole function should be about active sources,
          *   just like source.shutdown() but the language has no runtime
          *   difference between sources and active sources, so we use
          *   this trick to compare active sources and passive ones... *)
         Clock.force_init (fun x -> List.exists (fun y -> Oo.id x = Oo.id y) l)
       in
         Lang.list ~t:s_t (List.map (fun x -> Lang.source (x:>Source.source)) l))

let () =
  add_builtin "request.create.raw" ~cat:Liq
    ~descr:"Create a raw request, i.e. for files that should not be decoded \
            for streaming. Creation may fail if there is no available RID, \
            which cannot be detected currently: in that case one will obtain \
            a request that will fail to be resolved."
    [("indicators",
      Lang.list_t Lang.string_t,
      Some (Lang.list ~t:Lang.string_t []),
      None);
     "persistent",Lang.bool_t,Some (Lang.bool false),None;
     "",Lang.string_t,None,None]
    (Lang.request_t
      (Lang.frame_kind_t ~audio:Lang.zero_t ~video:Lang.zero_t ~midi:Lang.zero_t))
    (fun p ->
       let indicators = List.assoc "indicators" p in
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
         List.map (fun x -> Request.indicator x) indicators
       in
         Lang.request (Request.create_raw ~persistent ~indicators initial))

let () =
  Lang.add_builtin "request.create" ~category:(string_of_category Liq)
    ~descr:"Create a request. Creation may fail if there is no available RID, \
            which cannot be detected currently: in that case one will obtain \
            a request that will fail to be resolved."
    [("indicators",
      Lang.list_t Lang.string_t,
      Some (Lang.list ~t:Lang.string_t []),
      None);
     "persistent",Lang.bool_t,Some (Lang.bool false),None;
     "",Lang.string_t,None,None]
    (Lang.request_t (Lang.univ_t 1))
    (fun p t ->
       let indicators = List.assoc "indicators" p in
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
         List.map (fun x -> Request.indicator x) indicators
       in
       let kind =
         let k_t = Lang.of_request_t t in
           Lang.frame_kind_of_kind_type k_t
       in
         Lang.request
           (Request.create ~kind ~persistent ~indicators initial))

let () =
  add_builtin "request.resolve" ~cat:Liq
    ["timeout",Lang.float_t,Some (Lang.float 30.),
     Some "Limit in seconds to the duration of the resolving." ;
     "",Lang.request_t (Lang.univ_t 1),None,None]
    Lang.bool_t
    ~descr:"Resolve a request, i.e. attempt to get a valid local file. \
            The operation can take some time. Return true if the resolving \
            was successful, false otherwise (timeout or invalid URI)."
    (fun p ->
       let timeout = Lang.to_float (List.assoc "timeout" p) in
       let r = Lang.to_request (List.assoc "" p) in
         Lang.bool (try
           Request.Resolved = Request.resolve r timeout
         with _ -> false))

let () =
  add_builtin "request.metadata" ~cat:Liq
    ["",Lang.request_t (Lang.univ_t 1),None,None] Lang.metadata_t
    ~descr:"Get the metadata associated to a request."
    (fun p ->
       let r = Lang.to_request (List.assoc "" p) in
       Lang.metadata (Request.get_all_metadata r))

let () =
  add_builtin "request.log" ~cat:Liq
    ["",Lang.request_t (Lang.univ_t 1),None,None] Lang.string_t
    ~descr:"Get log data associated to a request."
    (fun p ->
       let r = Lang.to_request (List.assoc "" p) in
       Lang.string (Request.string_of_log
         (Request.get_log r)))

let () =
  add_builtin "request.ready" ~cat:Liq
    ~descr:"Check if a request is ready, i.e. is associated to a valid \
            local file. Unless the initial URI was such a file, a request \
            has to be resolved before being ready."
    ["", Lang.request_t (Lang.univ_t 1),None,None] Lang.bool_t
    (fun p ->
       let e = Lang.to_request (List.assoc "" p) in
         Lang.bool (Request.is_ready e))

let () =
  add_builtin "request.filename" ~cat:Liq
    ~descr:"Return a valid local filename if the request is ready, \
            and the empty string otherwise."
    [ "",Lang.request_t (Lang.univ_t 1),None,None ] Lang.string_t
    (fun p ->
       let r = Lang.to_request (List.assoc "" p) in
         Lang.string
           (match Request.get_filename r with
              | Some f -> f
              | None -> ""))

let () =
  add_builtin "request.destroy" ~cat:Liq
    ~descr:"Destroying a request causes any temporary associated file \
            to be deleted, and releases its RID. Persistent requests resist \
            to destroying, unless forced."
    ["force",Lang.bool_t,Some (Lang.bool false),
     Some "Destroy the request even if it is persistent." ;
     "",Lang.request_t (Lang.univ_t 1),None,None ]
    Lang.unit_t
    (fun p ->
       let force = Lang.to_bool (List.assoc "force" p) in
       let e = Lang.to_request (List.assoc "" p) in
         Request.destroy ~force e ;
         Lang.unit)

let () =
  add_builtin "request.duration" ~cat:Liq
    ["",Lang.string_t,None,None] Lang.float_t
    ~descr:"Compute the duration in seconds of audio data contained in \
            a request. The computation may be expensive. \
            Returns -1. if computation failed, typically if the file was \
            not recognized as valid audio."
    (fun p ->
       let f = Lang.to_string (List.assoc "" p) in
         Lang.float (try Request.duration f with Not_found -> -1.))

let () =
  add_builtin "playlist.parse" ~cat:Liq
    [
      "path",Lang.string_t,Some (Lang.string ""),Some "Default path for files.";
      "mime",Lang.string_t,Some (Lang.string ""),Some "Mime type for the playlist";
      "", Lang.string_t,None,None
    ]
    (Lang.list_t (Lang.product_t Lang.metadata_t Lang.string_t))
    ~descr:"Try to parse a local playlist. \
            Return a list of (metadata,URI) items, where metadata is a list \
            of (key,value) bindings."
    (fun p ->
       let f = Lang.to_string (List.assoc "" p) in
       let f = Utils.home_unrelate f in
       let content = Utils.read_all f in
       let pwd     =
         let pwd = Lang.to_string (List.assoc "path" p) in
         if pwd = "" then Filename.dirname f else pwd
       in
       let ret_item_t = Lang.product_t Lang.metadata_t Lang.string_t in
       let mime = Lang.to_string (List.assoc "mime" p) in
         try
           let _,l =
             if mime = "" then
               Playlist_parser.search_valid ~pwd content
             else
               (
                 match Playlist_parser.parsers#get mime with
                 | Some plugin ->
                    (mime,plugin.Playlist_parser.parser ~pwd content)
                 | None ->
                    log#f 3 "Unknown mime type, trying autodetection." ;
                    Playlist_parser.search_valid ~pwd content
               )
           in
           let process m =
             let f (n,v) =
               Lang.product (Lang.string n) (Lang.string v)
             in
               Lang.list
                 ~t:(Lang.product_t Lang.string_t Lang.string_t)
                 (List.map f m)
           in
           let process (m,uri) =
             Lang.product (process m) (Lang.string uri)
           in
             Lang.list ~t:ret_item_t (List.map process l)
         with
           | _ -> Lang.list ~t:ret_item_t [])

(** Sound utils. *)

let () =
  add_builtin "dB_of_lin" ~cat:Math ["",Lang.float_t,None,None] Lang.float_t
    ~descr:"Convert linear scale into decibels."
    (fun p ->
       let x = Lang.to_float (Lang.assoc "" 1 p) in
         Lang.float (Audio.dB_of_lin x)) ;
  add_builtin "lin_of_dB" ~cat:Math ["",Lang.float_t,None,None] Lang.float_t
    ~descr:"Convert decibels into linear scale."
    (fun p ->
       let x = Lang.to_float (Lang.assoc "" 1 p) in
         Lang.float (Audio.lin_of_dB x))

(** Interactive parameters. *)

module Var =
struct
  exception Invalid_value of string

  type variable =
      {
        name : string;
        t : Lang.t;
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
        (fun _ ->
           String.concat "\n"
             (List.map
                (fun (_,v) ->
                   Printf.sprintf "%s : %s" v.name (Lang_types.print v.t))
                (List.sort (fun (m,_) (n,_) -> compare m n) !variables)))

  let () =
    let usage = "set <variable> = <value>" in
      Server.add ~ns ~usage "set" ~descr:"Set a variable's value."
        (fun s ->
           let pat = "^([a-zA-Z_][a-zA-Z0-9_.]*) *= *(\"[^\"]*\"|[^ ]+)" in
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
        t = t;
        get = get;
        set = set;
        validate = validate;
      }
    in
      variables := (name,var)::!variables
end

let () =
  add_builtin "interactive.string" ~cat:Interaction
    ~descr:"Read a string from an interactive input."
    ["",Lang.string_t,None,None; "",Lang.string_t,None,None ]
    (Lang.fun_t [] Lang.string_t)
    (fun p ->
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let v = Lang.to_string (Lang.assoc "" 2 p) in
       let v = ref v in
         Var.add
           name
           Lang.string_t
           ~get:(fun () -> Printf.sprintf "%S" !v)
           ~set:(fun s -> v := (Scanf.sscanf s "%S" (fun s -> s)))
           ~validate:(fun s ->
                        try
                          ignore (Scanf.sscanf s "%S" (fun s -> s))
                        with _ ->
                          raise (Var.Invalid_value
                                   (s ^ " is not a string")));
         Lang.val_fun [] ~ret_t:Lang.string_t (fun _ _ -> Lang.string !v))

let () =
  add_builtin "interactive.float" ~cat:Interaction
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
         Lang.val_fun [] ~ret_t:Lang.float_t (fun _ _ -> Lang.float !v))

let () =
  add_builtin "interactive.bool" ~cat:Interaction
    ~descr:"Read a boolean from an interactive input."
    ["",Lang.string_t,None,None; "",Lang.bool_t,None,None ]
    (Lang.fun_t [] Lang.bool_t)
    (fun p ->
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let v = Lang.to_bool (Lang.assoc "" 2 p) in
       let v = ref v in
         Var.add
           name
           Lang.bool_t
           ~get:(fun () -> Printf.sprintf "%B" !v)
           ~set:(fun s -> v := s = "true")
           ~validate:
           (fun s ->
             if s <> "true" && s <> "false" then
               raise (Var.Invalid_value (s ^ " is not a boolean")));
         Lang.val_fun [] ~ret_t:Lang.bool_t (fun _ _ -> Lang.bool !v))

let () =
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

type request = Get | Post | Put | Head | Delete

let add_http_request http name descr request =
  let log = Dtools.Log.make [name] in
  let header_t = Lang.product_t Lang.string_t Lang.string_t in
  let headers_t = Lang.list_t header_t in
  let status_t =
    Lang.product_t (Lang.product_t Lang.string_t Lang.int_t) Lang.string_t
  in
  let request_return_t =
    Lang.product_t (Lang.product_t status_t headers_t) Lang.string_t
  in
  let params =
    if List.mem request [Get;Head;Delete] then
      []
    else
      ["data", Lang.string_t, Some (Lang.string ""), Some "POST data."]
  in
  let params = params @
    [
      "headers",headers_t, Some (Lang.list ~t:header_t []),
        Some "Additional headers.";
      "timeout",Lang.float_t, Some (Lang.float 10.),
        Some "Timeout for network operations.";
      "", Lang.string_t, None,
        Some "Requested URL, e.g. \"http://www.google.com:80/index.html\"."
    ]
  in
  let (module Http : Http.Http_t) = http in
  add_builtin name ~cat:Interaction ~descr
    params
    request_return_t
    (fun p ->
      let headers = List.assoc "headers" p in
      let headers = Lang.to_list headers in
      let headers = List.map Lang.to_product headers in
      let headers =
        List.map (fun (x,y) -> (Lang.to_string x, Lang.to_string y)) headers
      in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let url = Lang.to_string (List.assoc "" p) in
      let ((x,y,z),headers,data) =
        try
          let uri = Http.parse_url url in
          let request =
            match request with
              | Get -> Http.Get
              | Post ->
                 let data = Lang.to_string (List.assoc "data" p) in
                 Http.Post data
              | Put ->
                 let data = Lang.to_string (List.assoc "data" p) in
                 Http.Put data
              | Head -> Http.Head
              | Delete -> Http.Delete
          in 
          let log s =
            log#f 4 "%s" s
          in
          Http.full_request ~log ~timeout ~headers
                            ~uri ~request ()
        with
          | e ->
             (* Here we return a fake code.. *)
             ("Internal error",999,"Internal error"),[],
              (Printf.sprintf "Error while processing request: %s"
                  (Printexc.to_string e))
      in
      let status =
        Lang.product
          (Lang.product (Lang.string x) (Lang.int y))
          (Lang.string z)
      in
      let headers =
        List.map
          (fun (x,y) -> Lang.product (Lang.string x) (Lang.string y))
          headers
      in
      let headers = Lang.list ~t:header_t headers in
      Lang.product
        (Lang.product status headers)
        (Lang.string data))

let () =
  let add_http_request = add_http_request (module Http) in
  add_http_request
    "http.get"
    "Perform a full Http GET request and return (status,headers),data."
    Get;
  add_http_request
    "http.post"
    "Perform a full Http POST request and return (status,headers),data."
    Post;
  add_http_request
    "http.put"
    "Perform a full Http PUT request and return (status,headers),data."
    Put;
  add_http_request
    "http.head"
    "Perform a full Http HEAD request and return (status,headers),data."
    Head;
  add_http_request
    "http.delete"
    "Perform a full Http DELETE request and return (status,headers),data."
    Delete
