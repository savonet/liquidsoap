let () =
  let fname = Sys.argv.(1) in
  let term = Liquidsoap_lang.Runtime.parse_file fname in
  let json = Liquidsoap_tooling.Parsed_json.to_json term in
  Printf.printf "%s\n%!" (Liquidsoap_lang.Json.to_string ~compact:false json)
