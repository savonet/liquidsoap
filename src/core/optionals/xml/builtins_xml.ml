(* XML parsing and rendering for the language's `xml.parse` and `xml.stringify`,
   kept out of liquidsoap-lang so that package does not depend on xml-light. *)

let rec of_xml_light = function
  | Xml.PCData s -> `Text s
  | Xml.Element (name, params, children) ->
      `Element (name, params, List.map of_xml_light children)

let rec to_xml_light = function
  | `Text s -> Xml.PCData s
  | `Element (name, params, children) ->
      Xml.Element (name, params, List.map to_xml_light children)

let () =
  Atomic.set Liquidsoap_lang.Builtins_xml.xml_parser (fun s ->
      of_xml_light (Xml.parse_string s));
  Atomic.set Liquidsoap_lang.Builtins_xml.xml_printer (fun ~compact xml ->
      let xml = to_xml_light xml in
      if compact then Xml.to_string xml else Xml.to_string_fmt xml)
