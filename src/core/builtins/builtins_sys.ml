(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let configure = Modules.configure

let () =
  List.iter
    (fun (name, kind, str) ->
      ignore
        (Lang.add_builtin_base ~category:`Configuration
           ~descr:(Printf.sprintf "Liquidsoap's %s." kind)
           ~base:configure name (`String str) Lang.string_t))
    [
      ("libdir", "library directory", Configure.liq_libs_dir ());
      ("bindir", "Internal script directory", Configure.bin_dir ());
      ("rundir", "PID file directory", Configure.rundir ());
      ("logdir", "logging directory", Configure.logdir ());
    ]

(** Liquidsoap stuff *)

let log = Lang.log
let encoder = Modules.encoder

let conf_runtime =
  Dtools.Conf.void ~p:(Configure.conf#plug "runtime") "Runtime configuration."

let conf_strip_types_types =
  Dtools.Conf.bool
    ~p:(conf_runtime#plug "strip_types")
    ~d:true "Strip runtime types whenever possible to optimize memory usage."

let _ =
  let kind = Lang.univ_t () in
  Lang.add_builtin ~category:`Liquidsoap ~base:encoder "content_type"
    ~descr:"Return the content-type (mime) of an encoder, if known."
    [("", Lang.format_t kind, None, None)]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try Lang.string (Encoder.mime f) with _ -> Lang.string "")

let _ =
  let kind = Lang.univ_t () in
  Lang.add_builtin ~category:`Liquidsoap ~base:encoder "extension"
    ~descr:"Return the file extension of an encoder, if known."
    [("", Lang.format_t kind, None, None)]
    Lang.string_t
    (fun p ->
      let f = Lang.to_format (List.assoc "" p) in
      try Lang.string (Encoder.extension f) with _ -> Lang.string "")

let decoder = Modules.decoder
let decoder_pipe = Lang.add_module ~base:decoder "pipe"
let decoder_stdout = Lang.add_module ~base:decoder "stdout"
let decoder_file = Lang.add_module ~base:decoder "file"

let _ =
  (* The type of the test function for external decoders.
     Return is one of:
     . 0: no audio
     . -1: audio with unknown number of channels.
     . x >= 1: audio with a fixed number (x) of channels. *)
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
  ignore
    (Lang.add_builtin ~base:decoder_pipe "add" ~category:`Liquidsoap
       ~descr:
         "Register an external decoder. The encoder should output in WAV \
          format to his standard output (stdout) and read data from its \
          standard input (stdin)."
       [
         ("name", Lang.string_t, None, Some "Format/decoder's name.");
         ("description", Lang.string_t, None, Some "Description of the decoder.");
         ( "mimes",
           Lang.list_t Lang.string_t,
           Some (Lang.list []),
           Some
             "List of mime types supported by this decoder. Empty means any \
              mime type should be accepted." );
         ( "file_extensions",
           Lang.list_t Lang.string_t,
           Some (Lang.list []),
           Some
             "List of file extensions. Empty means any file extension should \
              be accepted." );
         ("priority", Lang.int_t, Some (Lang.int 1), Some "Decoder priority");
         test_arg;
         ("", Lang.string_t, None, Some "Process to start.");
       ]
       Lang.unit_t
       (fun p ->
         let process = Lang.to_string (Lang.assoc "" 1 p) in
         let name = Lang.to_string (List.assoc "name" p) in
         let doc = Lang.to_string (List.assoc "description" p) in
         let mimes =
           List.map Lang.to_string (Lang.to_list (List.assoc "mimes" p))
         in
         let mimes = if mimes = [] then None else Some mimes in
         let file_extensions =
           List.map Lang.to_string
             (Lang.to_list (List.assoc "file_extensions" p))
         in
         let file_extensions =
           if file_extensions = [] then None else Some file_extensions
         in
         let priority = Lang.to_int (List.assoc "priority" p) in
         let test = List.assoc "test" p in
         External_decoder.register_pipe ~name ~doc ~priority ~mimes
           ~file_extensions ~test:(test_f test) process;
         Lang.unit));
  let process_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t in
  Lang.add_builtin ~base:decoder_stdout "add" ~category:`Liquidsoap
    ~descr:
      "Register an external file decoder. The encoder should output in WAV \
       format to his standard output (stdout) and read data from the file it \
       receives. The estimated remaining duration for this decoder will be \
       unknown until the `buffer` last seconds of the file. If possible, it is \
       recommended to decode from stdin and use `decoder.add`."
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
      let doc = Lang.to_string (List.assoc "description" p) in
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
      External_decoder.register_stdout ~name ~doc ~priority ~mimes
        ~file_extensions ~test:(test_f test) ~process prebuf;
      Lang.unit)

(** Misc control/system functions. *)

let _ =
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
    Lang.list
      (List.map Lang.string (Re.Pcre.split ~rex:(Re.Pcre.regexp "\r?\n") r))
  in
  Lang.add_builtin ~base:Modules.server "execute" ~category ~descr params
    return_t execute

let locale = Lang.add_module ~base:Modules.runtime "locale"

let _ =
  Lang.add_builtin ~base:locale "set" ~category:`System
    ~descr:
      "Set the system's locale. This sets `LANG` and `LC_ALL` environment \
       variables to the given value and then calls `setlocale`. This is set to \
       `\"C\"` on startup, which defaults to the system's default locale. Keep \
       in mind that changing this can potentially impact some functions such a \
       `float_of_string`."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      Utils.force_locale (Lang.to_string (List.assoc "" p));
      Lang.unit)

let _ =
  Lang.add_builtin "shutdown" ~category:`System
    ~descr:"Shutdown the application."
    [("code", Lang.int_t, Some (Lang.int 0), Some "Exit code. Default: `0`")]
    Lang.unit_t
    (fun p ->
      Configure.restart := false;
      let code = Lang.to_int (List.assoc "code" p) in
      Tutils.shutdown code;
      Lang.unit)

let _ =
  Lang.add_builtin "restart" ~category:`System ~descr:"Restart the application."
    [] Lang.unit_t (fun _ ->
      Configure.restart := true;
      Tutils.shutdown 0;
      Lang.unit)

let _ =
  Lang.add_builtin "exit" ~category:`System
    ~descr:
      "Immediately stop the application. This should only be used in extreme \
       cases or to specify an exit value. The recommended way of stopping \
       Liquidsoap is to use shutdown."
    [("", Lang.int_t, None, Some "Exit value.")]
    Lang.unit_t
    (fun p ->
      let n = Lang.to_int (List.assoc "" p) in
      flush_all ();
      exit n)

let reopen = Lang.add_module "reopen"

let () =
  let reopen name descr f =
    ignore
      (Lang.add_builtin ~base:reopen name ~category:`System ~descr
         [("", Lang.string_t, None, None)]
         Lang.unit_t
         (fun p ->
           let file = Lang.to_string (List.assoc "" p) in
           f file;
           Lang.unit))
  in
  reopen "stdin" "Reopen standard input on the given file"
    (Utils.reopen_in stdin);
  reopen "stdout" "Reopen standard output on the given file"
    (Utils.reopen_out stdout);
  reopen "stderr" "Reopen standard error on the given file"
    (Utils.reopen_out stderr)

let _ =
  Lang.add_builtin ~base:Modules.process "pid" ~category:`System [] Lang.int_t
    ~descr:"Get the process' pid." (fun _ -> Lang.int (Unix.getpid ()))

let _ =
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

let _ =
  (* Cheap implementation of "getopt" which does not really deserve its name
     since it has little to do with the standards that getopt(3) implements.
     A complete rework of argv() and getopt() should eventually be done. *)
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
  ignore
    (Lang.add_builtin "getopt" ~category:`System
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
           Lang.string v)));
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

let playlist_parse =
  Lang.add_builtin ~base:Modules.playlist "parse" ~category:`Liquidsoap
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
      let f = Lang_string.home_unrelate f in
      if not (Sys.file_exists f) then
        Lang.raise_error ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf "File %s does not exist!"
               (Lang_string.quote_string f))
          "playlist";
      if Sys.is_directory f then
        Lang.raise_error ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf
               "File %s is a directory! A regular file was expected."
               (Lang_string.quote_string f))
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
                match Plug.get Playlist_parser.parsers mime with
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

let _ =
  Lang.add_builtin "seconds_of_main" ~category:`Liquidsoap
    ~descr:"Convert a number of main ticks in seconds."
    [("", Lang.int_t, None, None)]
    Lang.float_t
    (fun p ->
      Lang.float (Frame.seconds_of_main (Lang.to_int (List.assoc "" p))))

let environment =
  let ss = Lang.product_t Lang.string_t Lang.string_t in
  let ret_t = Lang.list_t ss in
  Lang.add_builtin "environment" ~category:`System
    ~descr:"Return the process environment." [] ret_t (fun _ ->
      let l = Lang.environment () in
      let l = List.map (fun (x, y) -> (Lang.string x, Lang.string y)) l in
      let l = List.map (fun (x, y) -> Lang.product x y) l in
      Lang.list l)

let _ =
  Lang.add_builtin ~base:environment "set" ~category:`System
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

let _ =
  Lang.add_builtin ~base:decoder_file "add" ~category:`Liquidsoap
    ~descr:
      "Register an external file decoder. The decoder receives a local file \
       and produces another local file. Produced file can be any format \
       decodable by liquidsoap and can also be a request uri. Recommended \
       returned value is: `annotate:metadata=\"value\",..:/path/to/file.wav`. \
       File decoders are applied during the request resolution process."
    [
      ("name", Lang.string_t, None, None);
      ("description", Lang.string_t, None, Some "Description of the decoder.");
      ( "mimes",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          "List of mime types supported by this decoder. Empty means any mime \
           type should be accepted." );
      ( "file_extensions",
        Lang.list_t Lang.string_t,
        None,
        Some "List of file extensions. Should not be empty." );
      ( "",
        Lang.fun_t
          [
            (false, "rlog", Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t);
            (false, "maxtime", Lang.float_t);
            (false, "", Lang.string_t);
          ]
          Lang.(nullable_t string_t),
        None,
        Some "Resolution function. Returns `null` if no file could be decoded."
      );
    ]
    Lang.unit_t
    (fun p ->
      let name = Lang.to_string (List.assoc "name" p) in
      let description = Lang.to_string (List.assoc "description" p) in
      let mime_types =
        List.map Lang.to_string (Lang.to_list (List.assoc "mimes" p))
      in
      let _file_extensions = List.assoc "file_extensions" p in
      let file_extensions =
        List.map Lang.to_string (Lang.to_list _file_extensions)
      in
      if file_extensions = [] then
        Runtime_error.raise ~pos:(Lang.pos p)
          "file_extensions should not be empty!";
      let fn = List.assoc "" p in
      let fn fname ~log timeout =
        let log =
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let v = List.assoc "" p in
              log (Lang.to_string v);
              Lang.unit)
        in
        let ret =
          Lang.apply fn
            [
              ("rlog", log);
              ("maxtime", Lang.float timeout);
              ("", Lang.string fname);
            ]
        in
        Option.map
          (fun s -> Request.indicator ~temporary:true (Lang.to_string s))
          (Lang.to_option ret)
      in
      Lang.add_protocol ~doc:description ~syntax:"/path/to/file"
        ~static:(fun _ -> true)
        ~mode:Request.(File { file_extensions; mime_types })
        name fn;
      Lang.unit)
