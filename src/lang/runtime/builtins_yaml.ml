(* Mapping keys are flattened to strings: the language's record labels are
   strings, so a non-scalar key has no representation and is dropped. *)
let string_of_key = function
  | YAMLx.String (_, s) -> Some s
  | YAMLx.Int (_, i) -> Some (Int64.to_string i)
  | YAMLx.Float (_, f) -> Some (string_of_float f)
  | YAMLx.Bool (_, b) -> Some (string_of_bool b)
  | YAMLx.Null _ -> Some "null"
  | _ -> None

(* Integers wider than OCaml's native int degrade to float rather than
   silently wrapping. *)
let json_of_int i =
  let n = Int64.to_int i in
  if Int64.equal (Int64.of_int n) i then `Int n else `Float (Int64.to_float i)

let rec json_of_yaml = function
  | YAMLx.Null _ -> `Null
  | YAMLx.Bool (_, b) -> `Bool b
  | YAMLx.Int (_, i) -> json_of_int i
  | YAMLx.Float (_, f) -> `Float f
  | YAMLx.String (_, s) -> `String s
  | YAMLx.Seq (_, l) -> `Tuple (List.map json_of_yaml l)
  | YAMLx.Map (_, l) ->
      `Assoc
        (List.filter_map
           (fun (_, k, v) ->
             Option.map (fun k -> (k, json_of_yaml v)) (string_of_key k))
           l)

let yaml_parser s =
  match YAMLx.Values.of_yaml_exn s with
    | [] -> `Null
    | [v] -> json_of_yaml v
    | _ -> failwith "Multiple YAML documents not supported"

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
  | `Assoc l ->
      YAMLx.Map
        ( YAMLx.zero_loc,
          List.map
            (fun (k, v) ->
              (YAMLx.zero_loc, YAMLx.String (YAMLx.zero_loc, k), yaml_of_json v))
            l )
  | `Tuple l -> YAMLx.Seq (YAMLx.zero_loc, List.map yaml_of_json l)
  | `String s -> YAMLx.String (YAMLx.zero_loc, s)
  | `Bool b -> YAMLx.Bool (YAMLx.zero_loc, b)
  | `Float f -> YAMLx.Float (YAMLx.zero_loc, f)
  | `Int i -> YAMLx.Int (YAMLx.zero_loc, Int64.of_int i)
  | `Null -> YAMLx.Null YAMLx.zero_loc

let rec apply_styles ~scalar_style ~flow = function
  | YAMLx.Scalar_node r ->
      let style = Option.value ~default:r.style scalar_style in
      YAMLx.Scalar_node { r with style }
  | YAMLx.Sequence_node r ->
      let items = List.map (apply_styles ~scalar_style ~flow) r.items in
      YAMLx.Sequence_node { r with items; flow }
  | YAMLx.Mapping_node r ->
      let pairs =
        List.map
          (fun (k, v) ->
            ( apply_styles ~scalar_style ~flow k,
              apply_styles ~scalar_style ~flow v ))
          r.pairs
      in
      YAMLx.Mapping_node { r with pairs; flow }
  | node -> node

let scalar_style pos = function
  | "any" -> None
  | "plain" -> Some YAMLx.Plain
  | "single_quoted" -> Some YAMLx.Single_quoted
  | "double_quoted" -> Some YAMLx.Double_quoted
  | "literal" -> Some YAMLx.Literal
  | "folded" -> Some YAMLx.Folded
  | v ->
      Runtime_error.raise
        ~message:(Printf.sprintf "Invalid scalar style: %s" v)
        ~pos "yaml"

let layout_style pos = function
  | "any" | "block" -> false
  | "flow" -> true
  | v ->
      Runtime_error.raise
        ~message:(Printf.sprintf "Invalid layout style: %s" v)
        ~pos "yaml"

let _ =
  Lang.add_builtin ~base:yaml "stringify" ~category:`String
    ~descr:
      "Convert a value to YAML. If the value cannot be represented as YAML \
       (for instance a function), a `error.yaml` exception is raised."
    [
      ( "scalar_style",
        Lang.string_t,
        Some (Lang.string "any"),
        Some
          "Scalar style. One of: \"any\", \"plain\", \"single_quoted\", \
           \"double_quoted\", \"literal\" or \"folded\"." );
      ( "layout_style",
        Lang.string_t,
        Some (Lang.string "any"),
        Some "Layout style. One of: \"any\", \"block\" or \"flow\"." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.string_t
    (fun p ->
      let pos = Lang.pos p in
      let v = List.assoc "" p in
      let scalar_style =
        scalar_style pos (Lang.to_string (List.assoc "scalar_style" p))
      in
      let flow =
        layout_style pos (Lang.to_string (List.assoc "layout_style" p))
      in
      try
        let json = Builtins_json.json_of_value v in
        let nodes = YAMLx.Values.to_nodes [yaml_of_json json] in
        let nodes = List.map (apply_styles ~scalar_style ~flow) nodes in
        Lang.string (YAMLx.Nodes.to_yaml nodes)
      with _ ->
        Runtime_error.raise
          ~message:
            (Printf.sprintf "Value %s cannot be represented as YAML"
               (Value.to_string v))
          ~pos "yaml")
