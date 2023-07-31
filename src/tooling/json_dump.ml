open Liquidsoap_lang

let parse_content content =
  let lexbuf = Sedlexing.Utf8.from_string content in
  let tokenizer = Preprocessor.mk_tokenizer ~pwd:(Sys.getcwd ()) lexbuf in
  Runtime.program tokenizer

let () =
  let content = Sys.argv.(1) in
  let term = parse_content content in
  let json = Liquidsoap_tooling.Parsed_json.to_json term in
  Printf.printf "%s\n%!" (Json.to_string ~compact:false json)
