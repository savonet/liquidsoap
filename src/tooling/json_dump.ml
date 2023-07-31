open Liquidsoap_lang

let rec read_stdin ?(content = []) () =
  try read_stdin ~content:(read_line () :: content) ()
  with End_of_file -> String.concat "\n" (List.rev content)

let parse_content content =
  let lexbuf = Sedlexing.Utf8.from_string content in
  let tokenizer = Preprocessor.mk_tokenizer ~pwd:(Sys.getcwd ()) lexbuf in
  Runtime.program tokenizer

let () =
  let content = read_stdin () in
  let term = parse_content content in
  let json = Liquidsoap_tooling.Parsed_json.to_json term in
  Printf.printf "%s\n%!" (Json.to_string ~compact:false json)
