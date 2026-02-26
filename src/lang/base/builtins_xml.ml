let xml_text_content = function
  | Xml.PCData s -> Some s
  | Xml.Element (_, _, [Xml.PCData s]) -> Some s
  | Xml.Element (_, _, []) -> None
  | _ -> None

let is_nullable ty =
  match (Type.deref ty).Type.descr with Type.Nullable _ -> true | _ -> false

let unwrap_nullable ty =
  match (Type.deref ty).Type.descr with
    | Type.Nullable ty -> (true, ty)
    | _ -> (false, ty)

let xml_element_name = function
  | Xml.PCData _ -> "xml_text"
  | Xml.Element (name, _, _) -> name

let xml_params = function
  | Xml.PCData _ -> []
  | Xml.Element (_, params, _) -> params

let xml_children = function
  | Xml.PCData _ -> []
  | Xml.Element (_, _, children) -> children

let rec parse_ground_meths ~typ_meths xml =
  List.filter_map
    (fun Type.{ meth; json_name; optional; scheme = _, meth_ty } ->
      let lbl = Option.value ~default:meth json_name in
      let nullable_meth = optional || is_nullable meth_ty in
      if lbl = "xml_params" then
        Some (meth, parse_xml_params ~ty:meth_ty (xml_params xml))
      else if lbl = "xml_text" then
        Some
          ( meth,
            match xml_text_content xml with
              | Some s -> Lang.string s
              | None when nullable_meth -> Lang.null
              | None -> Lang.string "" )
      else None)
    typ_meths

and value_of_typed_xml ~ty xml =
  let typ_meths, base_ty = Type.split_meths ty in
  let nullable, base_ty = unwrap_nullable base_ty in
  try
    match (xml, base_ty.Type.descr) with
      | _, Type.String -> (
          let meths = parse_ground_meths ~typ_meths xml in
          match xml_text_content xml with
            | Some s -> Lang.meth (Lang.string s) meths
            | None when nullable -> Lang.null
            | None -> Lang.meth (Lang.string "") meths)
      | _, Type.Int -> (
          let meths = parse_ground_meths ~typ_meths xml in
          match xml_text_content xml with
            | Some s -> Lang.meth (Lang.int (int_of_string s)) meths
            | None -> raise Not_found)
      | _, Type.Float -> (
          let meths = parse_ground_meths ~typ_meths xml in
          match xml_text_content xml with
            | Some s -> Lang.meth (Lang.float (float_of_string s)) meths
            | None -> raise Not_found)
      | _, Type.Bool -> (
          let meths = parse_ground_meths ~typ_meths xml in
          match xml_text_content xml with
            | Some s -> Lang.meth (Lang.bool (bool_of_string s)) meths
            | None -> raise Not_found)
      | _, Type.Tuple [_name_ty; _props_ty] when typ_meths = [] ->
          let name = xml_element_name xml in
          let props_meths, _ = Type.split_meths _props_ty in
          let props = parse_xml_props ~typ_meths:props_meths xml in
          Lang.tuple [Lang.string name; props]
      | _, Type.Tuple [] when typ_meths <> [] -> (
          (* Check if the element name matches a method in the expected type.
             If so, wrap the content in that method. *)
          let name = xml_element_name xml in
          let matching_meth =
            List.find_opt
              (fun Type.{ meth; json_name; _ } ->
                let lbl = Option.value ~default:meth json_name in
                lbl = name)
              typ_meths
          in
          match matching_meth with
            | Some Type.{ meth; scheme = _, meth_ty; _ } ->
                let typ_meths, base_ty = Type.split_meths meth_ty in
                let content =
                  match base_ty.Type.descr with
                    | Type.Tuple [] when typ_meths <> [] ->
                        parse_xml_record ~typ_meths xml
                    | _ -> value_of_typed_xml ~ty:meth_ty xml
                in
                Lang.record [(meth, content)]
            | None -> parse_xml_record ~typ_meths xml)
      | _, Type.List { t = elem_ty; _ } ->
          let children = xml_children xml in
          Lang.list
            (List.map
               (fun child -> value_of_typed_xml ~ty:elem_ty child)
               children)
      | _, Type.Var _ -> parse_untyped_xml xml
      | _ -> raise Not_found
  with _ when nullable -> Lang.null

and parse_xml_record ~typ_meths xml =
  let params = xml_params xml in
  let children = xml_children xml in
  let children_by_name =
    List.fold_left
      (fun acc child ->
        let name = xml_element_name child in
        let existing = Option.value ~default:[] (List.assoc_opt name acc) in
        (name, existing @ [child]) :: List.remove_assoc name acc)
      [] children
  in
  let meths =
    List.filter_map
      (fun Type.{ meth; json_name; optional; scheme = _, meth_ty } ->
        let lbl = Option.value ~default:meth json_name in
        let nullable_meth = optional || is_nullable meth_ty in
        if lbl = "xml_params" then
          Some (meth, parse_xml_params ~ty:meth_ty params)
        else if lbl = "xml_text" then (
          let text = xml_text_content xml in
          Some
            ( meth,
              match text with
                | Some s -> Lang.string s
                | None -> if nullable_meth then Lang.null else Lang.string "" ))
        else if lbl = "xml_children" then (
          let _, inner_ty = unwrap_nullable meth_ty in
          let inner_ty =
            match (Type.deref inner_ty).Type.descr with
              | Type.List { t; _ } -> t
              | _ -> inner_ty
          in
          Some
            ( meth,
              Lang.list
                (List.map
                   (fun child -> value_of_typed_xml ~ty:inner_ty child)
                   children) ))
        else (
          match List.assoc_opt lbl children_by_name with
            | Some [child] -> Some (meth, value_of_typed_xml ~ty:meth_ty child)
            | Some children_list ->
                let _, inner_ty = unwrap_nullable meth_ty in
                let v =
                  match (Type.deref inner_ty).Type.descr with
                    | Type.Tuple expected_tys ->
                        Lang.tuple
                          (List.mapi
                             (fun i child ->
                               let child_ty =
                                 if i < List.length expected_tys then
                                   List.nth expected_tys i
                                 else inner_ty
                               in
                               value_of_typed_xml ~ty:child_ty child)
                             children_list)
                    | _ ->
                        value_of_typed_xml ~ty:meth_ty (List.hd children_list)
                in
                Some (meth, v)
            | None when nullable_meth -> Some (meth, Lang.null)
            | None -> None))
      typ_meths
  in
  Lang.record meths

and parse_xml_params ~ty params =
  let _, ty = unwrap_nullable ty in
  let typ_meths, base_ty = Type.split_meths ty in
  match base_ty.Type.descr with
    | Type.List _ ->
        let list_value =
          Lang.list
            (List.map
               (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
               params)
        in
        if typ_meths <> [] then
          Lang.meth list_value
            (List.filter_map
               (fun Type.{ meth; json_name; optional; scheme = _, meth_ty } ->
                 let lbl = Option.value ~default:meth json_name in
                 match List.assoc_opt lbl params with
                   | Some v -> Some (meth, parse_param_value ~ty:meth_ty v)
                   | None when optional || is_nullable meth_ty ->
                       Some (meth, Lang.null)
                   | None -> None)
               typ_meths)
        else list_value
    | Type.Tuple [] when typ_meths <> [] ->
        Lang.record
          (List.filter_map
             (fun Type.{ meth; json_name; optional; scheme = _, meth_ty } ->
               let lbl = Option.value ~default:meth json_name in
               match List.assoc_opt lbl params with
                 | Some v -> Some (meth, parse_param_value ~ty:meth_ty v)
                 | None when optional || is_nullable meth_ty ->
                     Some (meth, Lang.null)
                 | None -> None)
             typ_meths)
    | _ ->
        Lang.list
          (List.map
             (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
             params)

and parse_param_value ~ty v =
  let _, ty = unwrap_nullable ty in
  match (Type.deref ty).Type.descr with
    | Type.Int -> Lang.int (int_of_string v)
    | Type.Float -> Lang.float (float_of_string v)
    | Type.Bool -> Lang.bool (bool_of_string v)
    | _ -> Lang.string v

and parse_xml_props ~typ_meths xml =
  let meths =
    List.filter_map
      (fun Type.{ meth; json_name; optional; scheme = _, meth_ty } ->
        let lbl = Option.value ~default:meth json_name in
        let nullable_meth = optional || is_nullable meth_ty in
        if lbl = "xml_text" then (
          let text = xml_text_content xml in
          Some
            ( meth,
              match text with
                | Some s -> Lang.string s
                | None when nullable_meth -> Lang.null
                | None -> Lang.string "" ))
        else if lbl = "xml_params" then
          Some (meth, parse_xml_params ~ty:meth_ty (xml_params xml))
        else if lbl = "xml_children" then (
          let _, inner_ty = unwrap_nullable meth_ty in
          let elem_ty =
            match (Type.deref inner_ty).Type.descr with
              | Type.List { t; _ } -> t
              | _ -> inner_ty
          in
          Some
            ( meth,
              Lang.list
                (List.map
                   (fun child -> value_of_typed_xml ~ty:elem_ty child)
                   (xml_children xml)) ))
        else None)
      typ_meths
  in
  Lang.record meths

and parse_untyped_xml xml =
  match xml with
    | Xml.PCData s -> Lang.string s
    | Xml.Element (name, params, children) ->
        let params_value =
          Lang.list
            (List.map
               (fun (k, v) -> Lang.product (Lang.string k) (Lang.string v))
               params)
        in
        let children_value = Lang.list (List.map parse_untyped_xml children) in
        let text =
          match children with [Xml.PCData s] -> Lang.string s | _ -> Lang.null
        in
        let props =
          Lang.record
            [
              ("xml_text", text);
              ("xml_params", params_value);
              ("xml_children", children_value);
            ]
        in
        Lang.meth (Lang.tuple [Lang.string name; props]) [(name, props)]

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
        value_of_typed_xml ~ty xml
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
