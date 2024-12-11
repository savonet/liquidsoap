let rec methods_of_xml = function
  | Xml.PCData s -> ("text", Lang.string s)
  | Xml.Element (name, params, ([Xml.PCData s] as children)) ->
      (name, Lang.meth (Lang.string s) (xml_node ~params ~children name))
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
                      | Some (Value.Tuple { value }) -> Lang.tuple (value @ [v])
                      | Some value -> Lang.tuple [value; v]
                  in
                  Methods.add name v methods)
                (Methods.from_list (xml_node ~params ~children name))
                children)) )

and xml_node ~params ~children name =
  [
    ("xml_node", Lang.string name);
    ( "xml_params",
      Lang.meth
        (Lang.list
           (List.map
              (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
              params))
        (List.map (fun (k, v) -> (k, Lang.string v)) params) );
    ("xml_children", Lang.tuple (List.map (fun v -> value_of_xml v) children));
  ]

and value_of_xml v = Lang.record [methods_of_xml v]

let rec check_value v ty =
  let typ_meths, ty = Type.split_meths ty in
  let meths, v = Value.split_meths v in
  let v =
    match (v, ty.Type.descr) with
      | Value.Tuple { value = [] }, Type.Nullable _ -> Lang.null
      | Value.String { value = s }, Type.Int -> Lang.int (int_of_string s)
      | Value.String { value = s }, Type.Float -> Lang.float (float_of_string s)
      | Value.String { value = s }, Type.Bool -> Lang.bool (bool_of_string s)
      | Value.String _, Type.Nullable ty -> check_value v ty
      | _, Type.Var _
      | Value.Tuple { value = [] }, Type.Tuple []
      | Value.String _, Type.String ->
          v
      | _ -> assert false
  in
  let meths =
    List.fold_left
      (fun meths (name, v) ->
        match List.find_opt (fun { Type.meth } -> meth = name) typ_meths with
          | None -> meths
          | Some { Type.scheme = _, ty } -> (name, check_value v ty) :: meths)
      [] meths
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
    | Value.String _ | Value.Bool _ | Value.Float _ | Value.Int _ ->
        Value.to_string (Lang.demeth v)
    | _ -> assert false

let params_of_xml_params v =
  match Lang.split_meths v with
    | [], Value.Tuple { value = params } ->
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

let rec xml_of_value v =
  match Lang.split_meths v with
    | meths, Value.Tuple { value = [] } -> xml_of_meths meths
    | _ -> assert false

and xml_of_meths meths =
  let xml_name = List.assoc_opt "xml_name" meths in
  let xml_nodes = List.assoc_opt "xml_nodes" meths in
  let xml_params = List.assoc_opt "xml_params" meths in
  let meths =
    List.filter
      (fun (k, _) -> not (List.mem k ["xml_name"; "xml_nodes"; "xml_params"]))
      meths
  in
  match (xml_name, xml_params, xml_nodes, meths) with
    | ( Some (Value.String { value = name }),
        params,
        Some (Value.Tuple { value = nodes }),
        [] )
    | None, params, None, [(name, Value.Tuple { value = nodes, methods = [] })] ->
        Xml.Element
          (name, params_of_optional_params params, List.map xml_of_value nodes)
    | None, params, None,  

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
