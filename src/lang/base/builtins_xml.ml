let rec methods_of_xml = function
  | Xml.PCData s ->
      ( "xml_text",
        Lang.meth (Lang.string s) (xml_node ~text:s ~params:[] ~children:[] ())
      )
  | Xml.Element (name, params, ([Xml.PCData s] as children)) ->
      (name, Lang.meth (Lang.string s) (xml_node ~text:s ~params ~children ()))
  | Xml.Element (name, params, children) ->
      ( name,
        Lang.record
          (Methods.bindings
             (List.fold_left
                (fun methods el ->
                  let name, v = methods_of_xml el in
                  let v =
                    match Methods.find_opt name methods with
                      | None -> v
                      | Some (Value.Tuple { value = [] } as value) ->
                          Lang.tuple [value; v]
                      | Some (Value.Tuple { value }) -> Lang.tuple (value @ [v])
                      | Some value -> Lang.tuple [value; v]
                  in
                  Methods.add name v methods)
                (Methods.from_list (xml_node ~params ~children ()))
                children)) )

and xml_node ?text ~params ~children () =
  [
    ("xml_text", match text with None -> Lang.null | Some s -> Lang.string s);
    ( "xml_params",
      Lang.meth
        (Lang.list
           (List.map
              (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
              params))
        (List.map (fun (k, v) -> (k, Lang.string v)) params) );
    ("xml_children", Lang.list (List.map (fun v -> value_of_xml v) children));
  ]

and value_of_xml v =
  let name, methods = methods_of_xml v in
  Lang.meth (Lang.tuple [Lang.string name; methods]) [(name, methods)]

let rec check_value v ty =
  let typ_meths, ty = Type.split_meths ty in
  let meths, v = Value.split_meths v in
  let v =
    match (v, ty.Type.descr) with
      | Value.Tuple { value = [] }, Type.Nullable _ -> Lang.null
      | _, Type.Tuple [] -> Lang.tuple []
      | Value.Tuple { value }, Type.Tuple l ->
          Lang.tuple
            (List.mapi (fun idx v -> check_value v (List.nth l idx)) value)
      | Value.List { value = l }, Type.List { t = ty } ->
          Lang.list (List.map (fun v -> check_value v ty) l)
      | Value.String { value = s }, Type.Int -> Lang.int (int_of_string s)
      | Value.String { value = s }, Type.Float -> Lang.float (float_of_string s)
      | Value.String { value = s }, Type.Bool -> Lang.bool (bool_of_string s)
      | Value.String _, Type.Nullable ty -> check_value v ty
      | _, Type.Var _ | Value.String _, Type.String -> v
      | _ -> assert false
  in
  let meths =
    List.fold_left
      (fun checked_meths { Type.meth; scheme = _, ty } ->
        let v = List.assoc meth meths in
        (meth, check_value v ty) :: checked_meths)
      [] typ_meths
  in
  let v = Lang.meth v meths in
  v

let _ =
  Lang.add_builtin "_internal_xml_parser_" ~category:`String ~flags:[`Hidden]
    ~descr:"Internal xml parser"
    [
      ("type", Value.RuntimeType.t, None, Some "Runtime type");
      ("", Lang.string_t, None, None);
    ]
    (Lang.univ_t ())
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let ty = Value.RuntimeType.of_value (List.assoc "type" p) in
      let ty = Type.fresh ty in
      try
        let xml = Xml.parse_string s in
        let value = value_of_xml xml in
        check_value value ty
      with exn -> (
        let bt = Printexc.get_raw_backtrace () in
        match exn with
          | _ ->
              Runtime_error.raise ~bt ~pos:(Lang.pos p)
                ~message:
                  (Printf.sprintf
                     "Parse error: xml value cannot be parsed as type: %s"
                     (Type.to_string ty))
                "xml"))

let xml = Lang.add_module "xml"

let string_of_ground v =
  match v with
    | Value.String { value = s } -> s
    | Value.Bool { value = b } -> string_of_bool b
    | Value.Float { value = f } -> Utils.string_of_float f
    | Value.Int { value = i; flags } -> Value.string_of_int_value ~flags i
    | _ -> assert false

let params_of_xml_params v =
  match Lang.split_meths v with
    | [], Value.List { value = params } ->
        List.map
          (function
            | Value.Tuple { value = [Value.String { value = s }; v] } ->
                (s, string_of_ground v)
            | _ -> assert false)
          params
    | params, Value.Tuple { value = [] } ->
        List.map (fun (s, v) -> (s, string_of_ground v)) params
    | _ -> assert false

let params_of_optional_params = function
  | None -> []
  | Some params -> params_of_xml_params params

let rec xml_of_value = function
  | Value.Tuple
      {
        value =
          [Value.String { value = name }; Value.Tuple { value = []; methods }];
      } ->
      xml_of_node ~name (Methods.bindings methods)
  | Value.Tuple { value = []; methods } -> (
      match Methods.bindings methods with
        | [(name, Value.Tuple { value = []; methods })] ->
            xml_of_node ~name (Methods.bindings methods)
        | [(name, (Value.String { methods } as v))]
        | [(name, (Value.Float { methods } as v))]
        | [(name, (Value.Int { methods } as v))]
        | [(name, (Value.Bool { methods } as v))] ->
            xml_of_node ~xml_text:(string_of_ground v) ~name
              (Methods.bindings methods)
        | _ -> assert false)
  | _ -> assert false

and xml_of_node ?xml_text ~name meths =
  let xml_text =
    match xml_text with
      | Some s -> Some s
      | None -> Option.map Lang.to_string (List.assoc_opt "xml_text" meths)
  in
  let xml_children =
    Option.map Lang.to_list (List.assoc_opt "xml_children" meths)
  in
  let xml_params = List.assoc_opt "xml_params" meths in
  let meths =
    List.filter
      (fun (k, _) ->
        not (List.mem k ["xml_text"; "xml_children"; "xml_params"]))
      meths
  in
  match (name, xml_params, xml_children, xml_text, meths) with
    | "xml_text", None, None, Some s, [] -> Xml.PCData s
    | name, xml_params, None, Some s, [] ->
        Xml.Element (name, params_of_optional_params xml_params, [Xml.PCData s])
    | name, xml_params, Some nodes, None, [] ->
        Xml.Element
          ( name,
            params_of_optional_params xml_params,
            List.map xml_of_value nodes )
    | name, xml_params, None, None, nodes ->
        Xml.Element
          ( name,
            params_of_optional_params xml_params,
            List.map
              (fun (name, value) -> xml_of_value (Lang.record [(name, value)]))
              nodes )
    | _ -> assert false

let _ =
  Lang.add_builtin ~base:xml "stringify" ~category:`String
    ~descr:
      "Convert a value to XML. If the value cannot be represented as XML (for \
       instance a function), a `error.xml` exception is raised."
    [
      ( "compact",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Output compact text." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.string_t
    (fun p ->
      let v = List.assoc "" p in
      let compact = Lang.to_bool (List.assoc "compact" p) in
      try
        let xml = xml_of_value v in
        Lang.string
          (if compact then Xml.to_string xml else Xml.to_string_fmt xml)
      with _ ->
        let bt = Printexc.get_raw_backtrace () in
        Runtime_error.raise ~bt ~pos:(Lang.pos p)
          ~message:"Value could not be converted to XML!" "xml")
