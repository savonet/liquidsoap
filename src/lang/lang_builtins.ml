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
    "liquidsoap.executable"
    ~category:(string_of_category Liq)
    ~descr:"Path to the Liquidsoap executable."
    (Lang.String Sys.executable_name)
    Lang.string_t

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
    ["",get_t (),None,None]
    (Lang.fun_t [] type_t)
    (fun p ->
      let getter =
        to_get
          (Lang.assoc "" 1 p)
      in
      Lang.val_fun [] ~ret_t:type_t (fun _ _ ->
          to_val (getter ())));
  let get_t = get_t () in
  add_builtin ~cat:Liq (name ^ "_getter")
    ~descr:("Identity function over " ^ name ^ " getters. " ^
            "This is useful to make types explicit.")
    ["",get_t,None,None]
    get_t
    (fun p -> List.assoc "" p)

let () =
  add_getters "string" Lang.string_getter_t Lang.string_t Lang.to_string_getter Lang.string;
  add_getters "float" Lang.float_getter_t Lang.float_t Lang.to_float_getter Lang.float;
  add_getters "int" Lang.int_getter_t Lang.int_t Lang.to_int_getter Lang.int;
  add_getters "bool" Lang.bool_getter_t Lang.bool_t Lang.to_bool_getter Lang.bool

let () =
  let kind = Lang.univ_t () in
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
  let kind = Lang.univ_t () in
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
      ("sync", Lang.string_t, Some (Lang.string "auto"),
      Some "Synchronization mode. One of: `\"auto\"`, `\"cpu\"`, \
            or `\"none\"`.");
      ("", Lang.list_t (Lang.source_t (Lang.univ_t ())), None,
       Some "List of sources to which the new clock will be assigned") ]
    Lang.unit_t
    (fun p ->
      match Lang.to_list (List.assoc "" p) with
      | [] -> Lang.unit
      | (hd::_) as sources ->
         let sync = List.assoc "sync" p in
         let sync =
           match Lang.to_string sync with
             | s when s = "auto" -> `Auto
             | s when s = "cpu" -> `CPU
             | s when s = "none" -> `None
             | _ ->
                raise (Lang_errors.Invalid_value (sync, "Invalid sync value"));
         in
         let id = Lang.to_string (List.assoc "id" p) in
         let id = if id = "" then (Lang.to_source hd)#id else id in
         let clock = new Clock.clock ~sync id in
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
    [ ("", Lang.list_t (Lang.source_t (Lang.univ_t ())), None, None) ]
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

(** Operations on products. *)

let () =
  let t1 = Lang.univ_t () in
  let t2 = Lang.univ_t () in
  add_builtin "fst" ~cat:Pair
    ~descr:"Get the first component of a pair."
    ["",Lang.product_t t1 t2,None,None]
    t1
    (fun p -> fst (Lang.to_product (Lang.assoc "" 1 p))) ;
  add_builtin "snd" ~cat:Pair
    ~descr:"Get the second component of a pair."
    ["",Lang.product_t t1 t2,None,None]
    t2
    (fun p -> snd (Lang.to_product (Lang.assoc "" 1 p)))

(** Misc control/system functions. *)

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
  let t = Lang.univ_t () in
  Lang.add_builtin "if"
    ~category:(string_of_category Control)
    ~descr:"The basic conditional."
    ~flags:[Lang.Hidden]
    [ "",Lang.bool_t,None,None ;
      "then", Lang.fun_t [] t, None,None ;
      "else", Lang.fun_t [] t, None,None ]
    t
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
    ["",Lang.univ_t (),None,None] Lang.unit_t
    (fun _ -> Lang.unit)

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

let () =
  add_builtin "seconds_of_master" ~cat:Liq
    ~descr:"Convert a number of master ticks in seconds."
    [ "", Lang.int_t, None, None ] Lang.float_t
    (fun p ->
      Lang.float
        (Frame.seconds_of_master 
          (Lang.to_int (List.assoc "" p))))

let () =
  add_builtin "print" ~cat:Interaction ~descr:"Print on standard output."
    ["newline",Lang.bool_t,Some (Lang.bool true),
     Some "If true, a newline is added after displaying the value." ;
     "",Lang.univ_t (),None,None]
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
