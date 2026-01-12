let _ =
  Lang.add_builtin ~base:Lang.error_module "methods"
    ~descr:"Decorate an error with all its methods" ~category:`Liquidsoap
    [("", Lang.error_t, None, None)]
    Lang.error_meths_t
    (fun p -> List.assoc "" p)

let _ =
  Lang.add_builtin "ignore"
    ~descr:"Convert anything to unit, preventing warnings."
    ~category:`Programming
    [("", Lang.univ_t (), None, None)]
    Lang.unit_t
    (fun _ -> Lang.unit)

let _ =
  Lang.add_builtin "position" ~descr:"Return the current position in the script"
    ~category:`Programming [] Lang_core.Position.t (fun p ->
      match Lang.pos p with
        | [] -> Lang.raise_error ~pos:[] ~message:"Unknown position" "eval"
        | p :: _ -> Lang_core.Position.to_value p)

let _ =
  let t = Lang.univ_t () in
  Lang.add_builtin "if" ~category:`Programming ~descr:"The basic conditional."
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
      Lang.apply ~pos:(Lang.pos p) (if c then fy else fn) [])

(** Operations on products. *)

let _ =
  let t1 = Lang.univ_t () in
  let t2 = Lang.univ_t () in
  Lang.add_builtin "fst" ~category:`Programming
    ~descr:"Get the first component of a pair."
    [("", Lang.product_t t1 t2, None, None)]
    t1
    (fun p -> fst (Lang.to_product (Lang.assoc "" 1 p)))

let _ =
  let t1 = Lang.univ_t () in
  let t2 = Lang.univ_t () in
  Lang.add_builtin "snd" ~category:`Programming
    ~descr:"Get the second component of a pair."
    [("", Lang.product_t t1 t2, None, None)]
    t2
    (fun p -> snd (Lang.to_product (Lang.assoc "" 1 p)))

let _ =
  Lang.add_builtin "print" ~category:`Programming
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
        match v with
          | String { value = s; flags } when not (Flags.has flags Flags.binary)
            ->
              s
          | _ -> Value.to_string v
      in
      print_string v;
      if nl then print_string "\n";
      flush stdout;
      Lang.unit)

(** Loops. *)

let _ =
  Lang.add_builtin "while" ~category:`Programming ~descr:"A while loop."
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

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin "for" ~category:`Programming ~descr:"A for loop."
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

let _ =
  Lang.add_builtin ~base:Modules.iterator "int" ~category:`Programming
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

let liquidsoap = Modules.liquidsoap

let liquidsoap_cache =
  Lang.add_builtin ~category:`Configuration ~descr:"Liquidsoap cache directory."
    ~base:liquidsoap "cache"
    [
      ( "mode",
        Lang.string_t,
        None,
        Some "Cache mode, one of: \"user\" or \"system\"" );
    ]
    (Lang.nullable_t Lang.string_t)
    (fun p ->
      let mode = List.assoc "mode" p in
      let dirtype =
        match Lang.to_string mode with
          | "system" -> `System
          | "user" -> `User
          | _ ->
              raise
                (Error.Invalid_value
                   ( mode,
                     "Invalid mode. Should be one of: \"user\" or \"system\"" ))
      in
      match Cache.dir dirtype with
        | None -> Lang.null
        | Some dir -> Lang.string dir)

let _ =
  Lang.add_builtin ~category:`Configuration
    ~descr:"Execute cache maintenance routine." ~base:liquidsoap_cache
    "maintenance"
    [
      ( "mode",
        Lang.string_t,
        None,
        Some "Cache mode, one of: \"user\" or \"system\"" );
    ]
    Lang.unit_t
    (fun p ->
      let mode = List.assoc "mode" p in
      let dirtype =
        match Lang.to_string mode with
          | "system" -> `System
          | "user" -> `User
          | _ ->
              raise
                (Error.Invalid_value
                   ( mode,
                     "Invalid mode. Should be one of: \"user\" or \"system\"" ))
      in
      let fn = !Hooks.cache_maintenance in
      fn dirtype;
      Lang.unit)

let liquidsoap_version =
  Lang.add_builtin_base ~category:`Configuration
    ~descr:"Liquidsoap version string." ~base:liquidsoap "version"
    (`String Build_config.version) Lang.string_t

let _ =
  Lang.add_builtin_base ~base:liquidsoap "executable" ~category:`Liquidsoap
    ~descr:"Path to the Liquidsoap executable." (`String Sys.executable_name)
    Lang.string_t

let liquidsoap_functions = Lang.add_module ~base:liquidsoap "functions"

let _ =
  Lang.add_builtin ~base:liquidsoap_functions "count" ~category:`Liquidsoap
    ~descr:"Number of functions registered in the standard library." []
    Lang.int_t (fun _ -> Doc.Value.count () |> Lang.int)

let _ =
  Lang.add_builtin_base ~category:`System
    ~descr:"Type of OS running liquidsoap." ~base:Modules.os "type"
    (`String Sys.os_type) Lang.string_t

let _ =
  Lang.add_builtin_base ~category:`System ~descr:"Executable file extension."
    "exe_ext" (`String Build_config.ext_exe) Lang.string_t

let _ =
  Lang.add_builtin ~category:`Liquidsoap
    ~descr:"Ensure that Liquidsoap version is greater or equal to given one."
    ~base:liquidsoap_version "at_least"
    [("", Lang.string_t, None, Some "Minimal version.")]
    Lang.bool_t
    (fun p ->
      let v = List.assoc "" p |> Lang.to_string in
      Lang.bool
        (Lang_string.Version.compare
           (Lang_string.Version.of_string v)
           (Lang_string.Version.of_string Build_config.version)
        <= 0))

let liquidsoap_script = Lang.add_module ~base:liquidsoap "script"

let _ =
  Lang.add_builtin_base ~category:`Liquidsoap
    ~descr:"Path to the current script, if available" ~base:liquidsoap_script
    "path" `Null
    (Lang.nullable_t Lang.string_t)

let liquidsoap_build_config = Lang.add_module ~base:liquidsoap "build_config"

let _ =
  Lang.add_builtin_base ~category:`Configuration
    ~descr:"OCaml version used to compile liquidspap."
    ~base:liquidsoap_build_config "ocaml_version" (`String Sys.ocaml_version)
    Lang.string_t

let _ =
  Lang.add_builtin_base ~category:`Configuration
    ~descr:"Git sha used to compile liquidsoap." ~base:liquidsoap_build_config
    "git_sha"
    (match Build_config.git_sha with None -> `Null | Some sha -> `String sha)
    Lang.(nullable_t string_t)

let _ =
  Lang.add_builtin_base ~category:`Configuration
    ~descr:"Is this build a development snapshot?" ~base:liquidsoap_build_config
    "is_snapshot" (`Bool Build_config.is_snapshot) Lang.bool_t

let _ =
  Lang.add_builtin_base ~category:`Configuration
    ~descr:"Is this build a release build?" ~base:liquidsoap_build_config
    "is_release" (`Bool (not Build_config.is_snapshot)) Lang.bool_t

let () =
  List.iter
    (fun (name, value) ->
      ignore
        (Lang.add_builtin_base ~category:`Configuration
           ~descr:("Build-time configuration value for " ^ name)
           ~base:liquidsoap_build_config name (`String value) Lang.string_t))
    [
      ("architecture", Build_config.architecture);
      ("host", Build_config.host);
      ("target", Build_config.target);
      ("system", Build_config.system);
      ("ocamlopt_cflags", Build_config.ocamlopt_cflags);
      ("native_c_compiler", Build_config.native_c_compiler);
      ("native_c_libraries", Build_config.native_c_libraries);
    ]

let _ =
  Lang.add_builtin ~category:`Programming
    ~descr:"Return any value with a fresh universal type for testing purposes."
    ~flags:[`Hidden] "💣"
    [("", Lang.univ_t (), Some Lang.null, None)]
    (Lang.univ_t ())
    (fun p -> List.assoc "" p)
