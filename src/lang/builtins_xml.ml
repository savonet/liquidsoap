let rec methods_of_xml = function
  | Xml.PCData s -> ("text", Lang.string s)
  | Xml.Element (name, params, ([Xml.PCData s] as children)) ->
      (name, Lang.meth (Lang.string s) (xml_node ~params ~children))
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
                (Methods.from_list (xml_node ~params ~children))
                children)) )

and xml_node ~params ~children =
  [
    ( "xml_params",
      Lang.record (List.map (fun (k, v) -> (k, Lang.string v)) params) );
    ( "xml_params_list",
      Lang.list
        (List.map
           (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
           params) );
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
