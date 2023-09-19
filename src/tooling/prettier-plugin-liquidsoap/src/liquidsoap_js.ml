open Js_of_ocaml

let parse content =
  let content = Js.to_string content in
  let json = Liquidsoap_tooling.Parsed_json.parse_string content in
  Js._JSON##parse
    (Js.string (Liquidsoap_lang.Json.to_string ~compact:true json))

let _ =
  Js.export "lang"
    (object%js
       method parse = parse
    end)
