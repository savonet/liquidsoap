(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let () =
  Lang.add_builtin_base ~category:`Liquidsoap
    ~descr:"Liquidsoap version string." "liquidsoap.version"
    Lang.(Ground (Ground.String Configure.version))
    Lang.string_t;
  List.iter
    (fun (name, kind, str) ->
      Lang.add_builtin_base ~category:`Liquidsoap
        ~descr:(Printf.sprintf "Liquidsoap's %s." kind)
        ("configure." ^ name)
        Lang.(Ground (Ground.String str))
        Lang.string_t)
    [
      ("libdir", "library directory", Configure.liq_libs_dir);
      ("bindir", "Internal script directory", Configure.bin_dir);
      ("rundir", "PID file directory", Configure.rundir);
      ("logdir", "logging directory", Configure.logdir);
      ("camomile_dir", "camomile files directory", Configure.camomile_dir);
      ("default_font", "default font file", Configure.default_font);
    ];
  Lang.add_builtin ~category:`Liquidsoap
    ~descr:"Ensure that Liquidsoap version is greater or equal to given one."
    "liquidsoap.version.at_least"
    [("", Lang.string_t, None, Some "Minimal version.")]
    Lang.bool_t
    (fun p ->
      let v = List.assoc "" p |> Lang.to_string in
      Lang.bool
        (Utils.Version.compare
           (Utils.Version.of_string v)
           (Utils.Version.of_string Configure.version)
        <= 0))

let () =
  Lang.add_builtin_base "liquidsoap.executable" ~category:`Liquidsoap
    ~descr:"Path to the Liquidsoap executable."
    Lang.(Ground (Ground.String Sys.executable_name))
    Lang.string_t

let () =
  Lang.add_builtin_base ~category:`System
    ~descr:"Type of OS running liquidsoap." "os.type"
    Lang.(Ground (Ground.String Sys.os_type))
    Lang.string_t

let () =
  Lang.add_builtin_base ~category:`System ~descr:"Executable file extension."
    "exe_ext"
    Lang.(Ground (Ground.String Configure.exe_ext))
    Lang.string_t

(** Liquidsoap stuff *)

let log = Lang.log

let () =
  let kind = Lang.univ_t () in
  Lang.add_builtin ~category:`Liquidsoap "encoder.content_type"
    ~descr:"Return the content-type (mime) of an encoder, if known."
    [("", Lang.format_t kind, None, None)]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try Lang.string (Encoder.mime f) with _ -> Lang.string "")

let () =
  let kind = Lang.univ_t () in
  Lang.add_builtin ~category:`Liquidsoap "encoder.extension"
    ~descr:"Return the file extension of an encoder, if known."
    [("", Lang.format_t kind, None, None)]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try Lang.string (Encoder.extension f) with _ -> Lang.string "")

let () =
  let proto =
    [
      ( "id",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Identifier for the new clock. The default empty string means that \
           the identifier of the first source will be used." );
      ( "sync",
        Lang.string_t,
        Some (Lang.string "auto"),
        Some
          "Synchronization mode. One of: `\"auto\"`, `\"cpu\"`, or `\"none\"`. \
           Defaults to `\"auto\"`, which synchronizes with the CPU clock if \
           none of the active sources are attached to their own clock (e.g. \
           ALSA input, etc). `\"cpu\"` always synchronizes with the CPU clock. \
           `\"none\"` removes all synchronization control." );
    ]
  in
  let assign id sync l =
    match l with
      | [] -> Lang.unit
      | hd :: _ as sources ->
          let id = Option.value ~default:(Lang.to_source hd)#id id in
          let sync =
            match Lang.to_string sync with
              | s when s = "auto" -> `Auto
              | s when s = "cpu" -> `CPU
              | s when s = "none" -> `None
              | _ -> raise (Error.Invalid_value (sync, "Invalid sync value"))
          in
          let clock = new Clock.clock ~sync id in
          List.iter
            (fun s ->
              try
                let s = Lang.to_source s in
                Clock.unify s#clock (Clock.create_known (clock :> Clock.clock))
              with
                | Source.Clock_conflict (a, b) ->
                    raise (Error.Clock_conflict (s.Lang.pos, a, b))
                | Source.Clock_loop (a, b) ->
                    raise (Error.Clock_loop (s.Lang.pos, a, b)))
            sources;
          Lang.unit
  in
  Lang.add_builtin "clock.assign_new" ~category:`Liquidsoap
    ~descr:"Create a new clock and assign it to a list of sources."
    (proto
    @ [
        ( "",
          Lang.list_t (Lang.source_t (Lang.univ_t ())),
          None,
          Some "List of sources to which the new clock will be assigned." );
      ])
    Lang.unit_t
    (fun p ->
      let id = Lang.to_valued_option Lang.to_string (List.assoc "id" p) in
      let sync = List.assoc "sync" p in
      let l = Lang.to_list (List.assoc "" p) in
      assign id sync l)

let () =
  Lang.add_builtin "clock.unify" ~category:`Liquidsoap
    ~descr:"Enforce that a list of sources all belong to the same clock."
    [("", Lang.list_t (Lang.source_t (Lang.univ_t ())), None, None)]
    Lang.unit_t
    (fun p ->
      let l = List.assoc "" p in
      try
        match Lang.to_source_list l with
          | [] -> Lang.unit
          | hd :: tl ->
              List.iter (fun s -> Clock.unify hd#clock s#clock) tl;
              Lang.unit
      with
        | Source.Clock_conflict (a, b) ->
            raise (Error.Clock_conflict (l.Lang.pos, a, b))
        | Source.Clock_loop (a, b) ->
            raise (Error.Clock_loop (l.Lang.pos, a, b)))

let () =
  let t = Lang.product_t Lang.string_t Lang.int_t in
  Lang.add_builtin "clock.status" ~category:`Liquidsoap
    ~descr:"Get the current time (in ticks) for all allocated clocks." []
    (Lang.list_t t) (fun _ ->
      let l =
        Clock.fold
          (fun clock l ->
            Lang.product (Lang.string clock#id) (Lang.int clock#get_tick) :: l)
          []
      in
      let l =
        Lang.product (Lang.string "uptime")
          (Lang.int
             (int_of_float (Utils.uptime () /. Lazy.force Frame.duration)))
        :: l
      in
      Lang.list l)

let () =
  (* The type of the test function for external decoders.
   * Return is one of:
   * . 0: no audio
   * . -1: audio with unknown number of channels.
   * . x >= 1: audio with a fixed number (x) of channels. *)
  let test_file_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.int_t in
  let test_arg =
    ( "test",
      test_file_t,
      None,
      Some
        "Function used to determine if a file should be decoded by the \
         decoder. Returned values are: 0: no decodable audio, -1: decodable \
         audio but number of audio channels unknown, x: fixed number of \
         decodable audio channels." )
  in
  let test_f f file = Lang.to_int (Lang.apply f [("", Lang.string file)]) in
  Lang.add_builtin "add_decoder" ~category:`Liquidsoap
    ~descr:
      "Register an external decoder. The encoder should output in WAV format \
       to his standard output (stdout) and read data from its standard input \
       (stdin)."
    [
      ("name", Lang.string_t, None, Some "Format/decoder's name.");
      ("description", Lang.string_t, None, Some "Description of the decoder.");
      ( "mimes",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          "List of mime types supported by this decoder. Empty means any mime \
           type should be accepted." );
      ( "file_extensions",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          "List of file extensions. Empty means any file extension should be \
           accepted." );
      ("priority", Lang.int_t, Some (Lang.int 1), Some "Decoder priority");
      test_arg;
      ("", Lang.string_t, None, Some "Process to start.");
    ]
    Lang.unit_t
    (fun p ->
      let process = Lang.to_string (Lang.assoc "" 1 p) in
      let name = Lang.to_string (List.assoc "name" p) in
      let sdoc = Lang.to_string (List.assoc "description" p) in
      let mimes =
        List.map Lang.to_string (Lang.to_list (List.assoc "mimes" p))
      in
      let mimes = if mimes = [] then None else Some mimes in
      let file_extensions =
        List.map Lang.to_string (Lang.to_list (List.assoc "file_extensions" p))
      in
      let file_extensions =
        if file_extensions = [] then None else Some file_extensions
      in
      let priority = Lang.to_int (List.assoc "priority" p) in
      let test = List.assoc "test" p in
      External_decoder.register_stdin ~name ~sdoc ~priority ~mimes
        ~file_extensions ~test:(test_f test) process;
      Lang.unit);

  let process_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t in
  Lang.add_builtin "add_oblivious_decoder" ~category:`Liquidsoap
    ~descr:
      "Register an external file decoder. The encoder should output in WAV \
       format to his standard output (stdout) and read data from the file it \
       receives. The estimated remaining duration for this decoder will be \
       unknown until the `buffer` last seconds of the file. If possible, it is \
       recommended to decode from stdin and use `add_decoder`."
    [
      ("name", Lang.string_t, None, Some "Format/decoder's name.");
      ("description", Lang.string_t, None, Some "Description of the decoder.");
      test_arg;
      ("priority", Lang.int_t, Some (Lang.int 1), Some "Decoder priority");
      ( "mimes",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          "List of mime types supported by this decoder. Empty means any mime \
           type should be accepted." );
      ( "file_extensions",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          "List of file extensions. Empty means any file extension should be \
           accepted." );
      ("buffer", Lang.float_t, Some (Lang.float 5.), None);
      ( "",
        process_t,
        None,
        Some
          "Process to start. The function takes the filename as argument and \
           returns the process to start." );
    ]
    Lang.unit_t
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let name = Lang.to_string (List.assoc "name" p) in
      let sdoc = Lang.to_string (List.assoc "description" p) in
      let prebuf = Lang.to_float (List.assoc "buffer" p) in
      let process file =
        Lang.to_string (Lang.apply f [("", Lang.string file)])
      in
      let test = List.assoc "test" p in
      let priority = Lang.to_int (List.assoc "priority" p) in
      let mimes =
        List.map Lang.to_string (Lang.to_list (List.assoc "mimes" p))
      in
      let mimes = if mimes = [] then None else Some mimes in
      let file_extensions =
        List.map Lang.to_string (Lang.to_list (List.assoc "file_extensions" p))
      in
      let file_extensions =
        if file_extensions = [] then None else Some file_extensions
      in
      External_decoder.register_oblivious ~name ~sdoc ~priority ~mimes
        ~file_extensions ~test:(test_f test) ~process prebuf;
      Lang.unit)

let () =
  Lang.add_builtin "metadata.export" ~category:`Liquidsoap
    ~descr:"Filter-out internal metadata."
    [("", Lang.metadata_t, None, None)]
    Lang.metadata_t
    (fun p ->
      Lang.metadata
        (Meta_format.to_metadata
           (Meta_format.export_metadata (Lang.to_metadata (List.assoc "" p)))))

(** Operations on products. *)

let () =
  let t1 = Lang.univ_t () in
  let t2 = Lang.univ_t () in
  Lang.add_builtin "fst" ~category:`Liquidsoap
    ~descr:"Get the first component of a pair."
    [("", Lang.product_t t1 t2, None, None)]
    t1
    (fun p -> fst (Lang.to_product (Lang.assoc "" 1 p)));
  Lang.add_builtin "snd" ~category:`Liquidsoap
    ~descr:"Get the second component of a pair."
    [("", Lang.product_t t1 t2, None, None)]
    t2
    (fun p -> snd (Lang.to_product (Lang.assoc "" 1 p)))

(** Misc control/system functions. *)

let () =
  let descr = "Execute a liquidsoap server command." in
  let category = `Liquidsoap in
  let params =
    [
      ("", Lang.string_t, None, Some "Command to execute.");
      ( "",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Argument for the command." );
    ]
  in
  let return_t = Lang.list_t Lang.string_t in
  let execute p =
    let c = Lang.to_string (Lang.assoc "" 1 p) in
    let a = Lang.to_string (Lang.assoc "" 2 p) in
    let s = match a with "" -> c | _ -> c ^ " " ^ a in
    let r = try Server.exec s with Not_found -> "Command not found!" in
    Lang.list (List.map Lang.string (Pcre.split ~pat:"\r?\n" r))
  in
  Lang.add_builtin "server.execute" ~category ~descr params return_t execute

let () =
  let t = Lang.univ_t () in
  Lang.add_builtin "if" ~category:`Control ~descr:"The basic conditional."
    ~flags:[`Hidden]
    [
      ("", Lang.bool_t, None, None);
      ("then", Lang.fun_t [] t, None, None);
      ("else", Lang.fun_t [] t, None, None);
    ]
    t
    (fun p ->
      let c = List.assoc "" p in
      let fy = List.assoc "then" p in
      let fn = List.assoc "else" p in
      let c = Lang.to_bool c in
      Lang.apply (if c then fy else fn) [])

let () =
  Lang.add_builtin "shutdown" ~category:`System
    ~descr:"Shutdown the application."
    [("code", Lang.int_t, Some (Lang.int 0), Some "Exit code. Default: `0`")]
    Lang.unit_t
    (fun p ->
      Configure.restart := false;
      let code = Lang.to_int (List.assoc "code" p) in
      Tutils.shutdown code;
      Lang.unit);
  Lang.add_builtin "restart" ~category:`System ~descr:"Restart the application."
    [] Lang.unit_t (fun _ ->
      Configure.restart := true;
      Tutils.shutdown 0;
      Lang.unit);
  Lang.add_builtin "exit" ~category:`System
    ~descr:
      "Immediately stop the application. This should only be used in extreme \
       cases or to specify an exit value. The recommended way of stopping \
       Liquidsoap is to use shutdown."
    [("", Lang.int_t, None, Some "Exit value.")]
    Lang.unit_t
    (fun p ->
      let n = Lang.to_int (List.assoc "" p) in
      exit n)

let () =
  Lang.add_builtin "sleep" ~category:`System
    ~descr:
      "Interrupt execution for a given amount of seconds. This freezes the \
       calling thread and should not be used in the main streaming loop."
    [("", Lang.float_t, None, Some "Number of seconds of sleep.")]
    Lang.unit_t
    (fun p ->
      let t = Lang.to_float (List.assoc "" p) in
      Unix.sleepf t;
      Lang.unit)

let () =
  let reopen name descr f =
    Lang.add_builtin name ~category:`System ~descr
      [("", Lang.string_t, None, None)]
      Lang.unit_t
      (fun p ->
        let file = Lang.to_string (List.assoc "" p) in
        f file;
        Lang.unit)
  in
  reopen "reopen.stdin" "Reopen standard input on the given file"
    (Utils.reopen_in stdin);
  reopen "reopen.stdout" "Reopen standard output on the given file"
    (Utils.reopen_out stdout);
  reopen "reopen.stderr" "Reopen standard error on the given file"
    (Utils.reopen_out stderr)

let () =
  Lang.add_builtin "getpid" ~category:`System [] Lang.int_t
    ~descr:"Get the process' pid." (fun _ -> Lang.int (Unix.getpid ()))

let () =
  let ss = Lang.product_t Lang.string_t Lang.string_t in
  let ret_t = Lang.list_t ss in
  Lang.add_builtin "environment" ~category:`System
    ~descr:"Return the process environment." [] ret_t (fun _ ->
      let l = Utils.environment () in
      let l = List.map (fun (x, y) -> (Lang.string x, Lang.string y)) l in
      let l = List.map (fun (x, y) -> Lang.product x y) l in
      Lang.list l)

let () =
  Lang.add_builtin "setenv" ~category:`System
    ~descr:"Set the value associated to a variable in the process environment."
    [
      ("", Lang.string_t, None, Some "Variable to be set.");
      ("", Lang.string_t, None, Some "Value to set.");
    ]
    Lang.unit_t
    (fun p ->
      let label = Lang.to_string (Lang.assoc "" 1 p) in
      let value = Lang.to_string (Lang.assoc "" 2 p) in
      Unix.putenv label value;
      Lang.unit)

let () =
  Lang.add_builtin "log" ~category:`Liquidsoap ~descr:"Log a message."
    [
      ("label", Lang.string_t, Some (Lang.string "lang"), None);
      ("level", Lang.int_t, Some (Lang.int 3), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let msg = Lang.to_string (List.assoc "" p) in
      let label = Lang.to_string (List.assoc "label" p) in
      let level = Lang.to_int (List.assoc "level" p) in
      (Log.make [label])#f level "%s" msg;
      Lang.unit)

let () =
  (* Cheap implementation of "getopt" which does not really deserve its name
   * since it has little to do with the standards that getopt(3) implements.
   * A complete rework of argv() and getopt() should eventually be done. *)
  let argv = Shebang.argv in
  let offset =
    (* Index of the last non-script parameter on the command-line. *)
    let rec find i =
      if i >= Array.length argv || argv.(i) = "--" then i else find (i + 1)
    in
    find 0
  in
  let opts =
    ref (Array.to_list (Array.sub argv offset (Array.length argv - offset)))
  in
  Lang.add_builtin "getopt" ~category:`System
    [
      ("default", Lang.string_t, Some (Lang.string ""), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    ~descr:
      "Parse command line options:\n\
       `getopt(\"-o\")` returns \"1\" if \"-o\" was passed without any \
       parameter, \"0\" otherwise.\n\
       `getopt(default=\"X\",\"-o\")` returns \"Y\" if \"-o Y\" was passed, \
       \"X\" otherwise.\n\
       The result is removed from the list of arguments, affecting subsequent\n\
       calls to `argv()` and `getopt()`."
    (fun p ->
      let default = Lang.to_string (List.assoc "default" p) in
      let name = Lang.to_string (List.assoc "" p) in
      let argv = !opts in
      if default = "" then (
        try
          ignore (List.find (fun x -> x = name) argv);
          opts := List.filter (fun x -> x <> name) argv;
          Lang.string "1"
        with Not_found -> Lang.string "0")
      else (
        let rec find l l' =
          match l with
            | [] -> (default, List.rev l')
            | e :: v :: l when e = name -> (v, List.rev_append l' l)
            | e :: l -> find l (e :: l')
        in
        let v, l = find argv [] in
        opts := l;
        Lang.string v));

  Lang.add_builtin "argv" ~category:`System
    ~descr:
      "Get command-line parameters. The parameters are numbered starting from \
       1, the zeroth parameter being the script name."
    [
      ("default", Lang.string_t, Some (Lang.string ""), None);
      ("", Lang.int_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let default = Lang.to_string (List.assoc "default" p) in
      let i = Lang.to_int (List.assoc "" p) in
      let opts = !opts in
      if i = 0 then (
        (* Special case so that argv(0) returns the script name *)
        let i = offset - 1 in
        if 0 <= i && i < Array.length argv then Lang.string argv.(i)
        else Lang.string default)
      else if i < List.length opts then Lang.string (List.nth opts i)
      else Lang.string default)

let () =
  Lang.add_builtin "ignore"
    ~descr:"Convert anything to unit, preventing warnings." ~category:`Control
    [("", Lang.univ_t (), None, None)]
    Lang.unit_t
    (fun _ -> Lang.unit)

let () =
  Lang.add_builtin "playlist.parse" ~category:`Liquidsoap
    [
      ( "path",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Default path for files." );
      ( "mime",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Mime type for the playlist" );
      ("", Lang.string_t, None, None);
    ]
    (Lang.list_t (Lang.product_t Lang.metadata_t Lang.string_t))
    ~descr:
      "Try to parse a local playlist. Return a list of (metadata,URI) items, \
       where metadata is a list of (key,value) bindings."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Utils.home_unrelate f in
      if not (Sys.file_exists f) then
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf "File %s does not exist!" (Utils.quote_string f))
          "playlist";
      if Sys.is_directory f then
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf
               "File %s is a directory! A regular file was expected."
               (Utils.quote_string f))
          "playlist";
      let content = Utils.read_all f in
      let pwd =
        let pwd = Lang.to_string (List.assoc "path" p) in
        if pwd = "" then Filename.dirname f else pwd
      in
      let mime = Lang.to_valued_option Lang.to_string (List.assoc "mime" p) in
      try
        let _, l =
          match mime with
            | None -> Playlist_parser.search_valid ~pwd content
            | Some mime -> (
                match Playlist_parser.parsers#get mime with
                  | Some plugin ->
                      (mime, plugin.Playlist_parser.parser ~pwd content)
                  | None ->
                      log#important "Unknown mime type, trying autodetection.";
                      Playlist_parser.search_valid ~pwd content)
        in
        let process m =
          let f (n, v) = Lang.product (Lang.string n) (Lang.string v) in
          Lang.list (List.map f m)
        in
        let process (m, uri) = Lang.product (process m) (Lang.string uri) in
        Lang.list (List.map process l)
      with _ -> Lang.list [])

(** Sound utils. *)

let () =
  Lang.add_builtin "seconds_of_main" ~category:`Liquidsoap
    ~descr:"Convert a number of main ticks in seconds."
    [("", Lang.int_t, None, None)]
    Lang.float_t
    (fun p ->
      Lang.float (Frame.seconds_of_main (Lang.to_int (List.assoc "" p))))

let () =
  Lang.add_builtin "print" ~category:`Interaction
    ~descr:"Print on standard output."
    [
      ( "newline",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "If true, a newline is added after displaying the value." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.unit_t
    (fun p ->
      let nl = Lang.to_bool (List.assoc "newline" p) in
      let v = List.assoc "" p in
      let v =
        match v.Lang.value with
          | Lang.(Ground (Ground.String s)) -> s
          | _ -> Value.to_string v
      in
      let v = if nl then v ^ "\n" else v in
      print_string v;
      flush stdout;
      Lang.unit)

(** Loops. *)

let () =
  Lang.add_builtin "while" ~category:`Liquidsoap ~descr:"A while loop."
    [
      ("", Lang.getter_t Lang.bool_t, None, Some "Condition guarding the loop.");
      ("", Lang.fun_t [] Lang.unit_t, None, Some "Function to execute.");
    ]
    Lang.unit_t
    (fun p ->
      let c = Lang.to_bool_getter (Lang.assoc "" 1 p) in
      let f = Lang.to_fun (Lang.assoc "" 2 p) in
      while c () do
        ignore (f [])
      done;
      Lang.unit)

let () =
  let a = Lang.univ_t () in
  Lang.add_builtin "for" ~category:`Liquidsoap ~descr:"A for loop."
    ~flags:[`Hidden]
    [
      ("", Lang.fun_t [] (Lang.nullable_t a), None, Some "Values to iterate on.");
      ( "",
        Lang.fun_t [(false, "", a)] Lang.unit_t,
        None,
        Some "Function to execute." );
    ]
    Lang.unit_t
    (fun p ->
      let i = Lang.to_fun (Lang.assoc "" 1 p) in
      let f = Lang.to_fun (Lang.assoc "" 2 p) in
      let rec aux () =
        match Lang.to_option (i []) with
          | Some i ->
              ignore (f [("", i)]);
              aux ()
          | None -> Lang.unit
      in
      aux ())

let () =
  Lang.add_builtin "iterator.int" ~category:`Liquidsoap
    ~descr:"Iterator on integers." ~flags:[`Hidden]
    [
      ("", Lang.int_t, None, Some "First value.");
      ("", Lang.int_t, None, Some "Last value (included).");
    ]
    (Lang.fun_t [] (Lang.nullable_t Lang.int_t))
    (fun p ->
      let a = Lang.to_int (Lang.assoc "" 1 p) in
      let b = Lang.to_int (Lang.assoc "" 2 p) in
      let i = ref a in
      let f _ =
        let ans = !i in
        incr i;
        if ans > b then Lang.null else Lang.int ans
      in
      Lang.val_fun [] f)
