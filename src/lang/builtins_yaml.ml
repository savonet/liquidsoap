type yaml =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of yaml list
  | `O of (string * yaml) list ]

let yaml_parser : (string -> yaml) Atomic.t =
  Atomic.make (fun _ ->
      Runtime_error.raise
        ~message:
          "YAML support not enabled! Please re-compile liquidsoap with the \
           `yaml` module to enable YAML parsing and rendering."
        ~pos:[] "not_found")

let rec json_of_yaml = function
  | `O l -> `Assoc (List.map (fun (lbl, v) -> (lbl, json_of_yaml v)) l)
  | `A l -> `Tuple (List.map json_of_yaml l)
  | `String s -> `String s
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Null -> `Null

let _ =
  Lang.add_builtin "_internal_yaml_parser_" ~category:`String ~flags:[`Hidden]
    ~descr:"Internal yaml parser"
    [
      ("type", Value.RuntimeType.t, None, Some "Runtime type");
      ("", Lang.string_t, None, None);
    ]
    (Lang.univ_t ())
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let ty = Value.RuntimeType.of_value (List.assoc "type" p) in
      let ty = Type.fresh ty in
      let parser = Atomic.get yaml_parser in
      try
        let yaml = parser s in
        Builtins_json.value_of_typed_json ~ty (json_of_yaml yaml)
      with exn -> (
        let bt = Printexc.get_raw_backtrace () in
        match exn with
          | Runtime_error.Runtime_error e when e.Runtime_error.kind = "not_found" ->
              Printexc.raise_with_backtrace exn bt
          | _ ->
              Runtime_error.raise ~bt ~pos:(Lang.pos p)
                ~message:
                  (Printf.sprintf
                     "Parse error: yaml value cannot be parsed as type: %s"
                     (Type.to_string ty))
                "yaml"))
