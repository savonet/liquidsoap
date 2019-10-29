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
      ("bindir", "Internal script directory", Configure.bin_dir) ;
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
    ~descr:("Identity function over " ^ name ^ " getters. " ^
            "This is useful to make types explicit.")
    ["",get_t 1,None,None]
    (get_t 1)
    (fun p -> List.assoc "" p)

let () =
  add_getters "string" Lang.string_getter_t Lang.string_t Lang.to_string_getter Lang.string;
  add_getters "float" Lang.float_getter_t Lang.float_t Lang.to_float_getter Lang.float;
  add_getters "int" Lang.int_getter_t Lang.int_t Lang.to_int_getter Lang.int;
  add_getters "bool" Lang.bool_getter_t Lang.bool_t Lang.to_bool_getter Lang.bool

let () =
  let kind = Lang.univ_t 1 in
  add_builtin ~cat:Liq "encoder.content_type"
    ~descr:"Return the content-type (mime) of an encoder, if known."
    ["", Lang.format_t kind,None,None]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try
        Lang.string (Encoder.mime f)
      with _ -> Lang.string "")

let () =
  let kind = Lang.univ_t 1 in
  add_builtin ~cat:Liq "encoder.extension"
    ~descr:"Return the file extension of an encoder, if known."
    ["", Lang.format_t kind,None,None]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try
        Lang.string (Encoder.extension f)
      with _ -> Lang.string "")

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
       Some "Identifier for the new clock. The default empty string means that \
             the identifier of the first source will be used.") ;
      ("sync", Lang.bool_t, Some (Lang.bool true),
       Some "Do not synchronize the clock on regular wallclock time, but try to \
             run as fast as possible (CPU burning mode).") ;
      ("", Lang.list_t (Lang.source_t (Lang.univ_t 1)), None,
       Some "List of sources to which the new clock will be assigned") ]
    Lang.unit_t
    (fun p ->
      match Lang.to_list (List.assoc "" p) with
      | [] -> Lang.unit
      | (hd::_) as sources ->
         let sync = Lang.to_bool (List.assoc "sync" p) in
         let id = Lang.to_string (List.assoc "id" p) in
         let id = if id = "" then (Lang.to_source hd)#id else id in
         let clock = new Clock.wallclock ~sync id in
         List.iter
           (fun s ->
             try
               let s = Lang.to_source s in
               Clock.unify s#clock (Clock.create_known (clock:>Clock.clock))
             with
             | Source.Clock_conflict (a,b) ->
                raise (Lang_errors.Clock_conflict (s.Lang.t.Lang_types.pos,a,b))
             | Source.Clock_loop (a,b) ->
                raise (Lang_errors.Clock_loop (s.Lang.t.Lang_types.pos,a,b)))
           sources;
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
           List.iter (fun s -> Clock.unify hd#clock s#clock) tl;
           Lang.unit
      with
      | Source.Clock_conflict (a,b) ->
         raise (Lang_errors.Clock_conflict (l.Lang.t.Lang_types.pos,a,b))
      | Source.Clock_loop (a,b) ->
         raise (Lang_errors.Clock_loop (l.Lang.t.Lang_types.pos,a,b)))

let () =
  let t = Lang.product_t Lang.string_t Lang.int_t in
  add_builtin "clock.status" ~cat:Liq
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
            unknown until the `buffer` last seconds of the file. \
            If possible, it is recommended \
            to decode from stdin and use `add_decoder`."
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

(** Comparison and boolean connectives *)

let compare_value a b =
  let rec aux = function
    | Lang.Float  a, Lang.Float b   -> compare a b
    | Lang.Int    a, Lang.Int b     -> compare a b
    | Lang.String a, Lang.String b  -> compare a b
    | Lang.Bool   a, Lang.Bool b    -> compare a b
    | Lang.Tuple   l, Lang.Tuple m ->
       List.fold_left2 (fun cmp a b -> if cmp <> 0 then cmp else aux (a.Lang.value,b.Lang.value)) 0 l m
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
        "Set to `false` if the execution of the code can take long \
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
  add_builtin "sleep" ~cat:Sys
    ~descr:"Sleep for a given amount of seconds (beware that it freezes the thread executing it)."
    ["", Lang.float_t, None, Some "Number of seconds of sleep."] Lang.unit_t
    (fun p ->
      let t = Lang.to_float (List.assoc "" p) in
      let t = int_of_float (t +. 0.5) in
      Unix.sleep t;
      Lang.unit)

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
            let fn (args:Lang.full_env) t = Tutils.mutexify m (fun () ->
              let args = List.map (fun (x,gv) -> x, Lazy.from_val gv) args in
              let env = List.rev_append args env in
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
    ~descr:"Shell command call."
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
  let ss = Lang.product_t Lang.string_t Lang.string_t in
  let ret_t = Lang.list_t ss in
  add_builtin "environment" ~cat:Sys
    ~descr:"Return the process environment."
    [] ret_t
    (fun _ ->
      let l = Utils.environment () in
      let l = List.map (fun (x,y) -> (Lang.string x, Lang.string y)) l in
      let l = List.map (fun (x,y) -> Lang.product x y) l in
      Lang.list ~t:ss l)

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
         (Log.make [label])#f level "%s" msg ;
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
   `getopt(\"-o\")` returns \"1\" if \"-o\" was passed \
   without any parameter, \"0\" otherwise.\n\
   `getopt(default=\"X\",\"-o\")` returns \"Y\" if \"-o Y\" \
   was passed, \"X\" otherwise.\n\
   The result is removed from the list of arguments, affecting subsequent\n\
   calls to `argv()` and `getopt()`."
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

  add_builtin "argv" ~cat:Sys
    ~descr:"Get command-line parameters. The parameters are numbered starting from 1, the zeroth parameter being the script name."
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
                    log#important "Unknown mime type, trying autodetection." ;
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
