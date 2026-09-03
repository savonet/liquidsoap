(* MiniYaml has a single numeric constructor: an integral number that fits in an
   [int] is handed back as one, so that `let yaml.parse (_ : int)` works, and
   anything wider degrades to a float rather than silently wrapping. *)
let json_of_float f =
  if Float.is_integer f && Float.abs f < Float.of_int max_int then
    `Int (int_of_float f)
  else `Float f

let rec json_of_yaml = function
  | Yaml.Null -> `Null
  | Yaml.Bool b -> `Bool b
  | Yaml.Float f -> json_of_float f
  | Yaml.String s -> `String s
  | Yaml.List l -> `Tuple (List.map json_of_yaml l)
  | Yaml.Assoc l -> `Assoc (List.map (fun (k, v) -> (k, json_of_yaml v)) l)

let yaml_parser s =
  match Yaml.of_string s with
    | Ok v -> json_of_yaml v
    | Error msg -> failwith msg

let _ =
  Lang.add_builtin "_0_yaml_parser" ~category:`String ~flags:[`Hidden]
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
      try Builtins_json.value_of_typed_json ~ty (yaml_parser s)
      with _ ->
        let bt = Printexc.get_raw_backtrace () in
        Runtime_error.raise ~bt ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf
               "Parse error: yaml value cannot be parsed as type: %s"
               (Type.to_string ty))
          "yaml")

let yaml = Lang.add_module "yaml"

let rec yaml_of_json = function
  | `Assoc l -> Yaml.Assoc (List.map (fun (k, v) -> (k, yaml_of_json v)) l)
  | `Tuple l -> Yaml.List (List.map yaml_of_json l)
  | `String s -> Yaml.String s
  | `Bool b -> Yaml.Bool b
  | `Float f -> Yaml.Float f
  | `Int i -> Yaml.Float (float_of_int i)
  | `Null -> Yaml.Null

let _ =
  Lang.add_builtin ~base:yaml "stringify" ~category:`String
    ~descr:
      "Convert a value to YAML. If the value cannot be represented as YAML \
       (for instance a function), a `error.yaml` exception is raised."
    [("", Lang.univ_t (), None, None)]
    Lang.string_t
    (fun p ->
      let pos = Lang.pos p in
      let v = List.assoc "" p in
      try
        let json = Builtins_json.json_of_value v in
        Lang.string (Yaml.to_string (yaml_of_json json))
      with _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf "Value %s cannot be represented as YAML"
               (Value.to_string v))
          ~pos "yaml")
