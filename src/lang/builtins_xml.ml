let rec methods_of_xml = function
  | Xml.PCData s -> Methods.from_list [("text", Term.make (`String s))]
  | Xml.Element (name, params, ([Xml.PCData s] as children)) ->
      Methods.from_list
        [(name, Term.make ~methods:(xml_node ~params ~children) (`String s))]
  | Xml.Element (name, params, children) ->
      Methods.from_list
        [
          ( name,
            Term.make
              ~methods:
                (List.fold_left
                   (fun elements el ->
                     Methods.append elements (methods_of_xml el))
                   (xml_node ~params ~children)
                   children)
              `Null );
        ]

and xml_node ~params ~children =
  Methods.from_list
    [
      ( "xml_params",
        Term.make
          ~methods:
            (Methods.from_list
               (List.map (fun (k, v) -> (k, Term.make (`String v))) params))
          Term.unit );
      ( "xml_children",
        Term.make (`Tuple (List.map (fun v -> term_of_xml v) children)) );
    ]

and term_of_xml v = Term.make ~methods:(methods_of_xml v) `Null

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
        let tm = term_of_xml xml in
        Typechecking.check ~throw:(fun exn -> raise exn) tm;
        Typing.(ty <: tm.Term.t);
        Evaluation.eval tm
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
