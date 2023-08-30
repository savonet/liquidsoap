open Liquidsoap_lang

let rec read_stdin ?(content = []) () =
  try read_stdin ~content:(read_line () :: content) ()
  with End_of_file -> String.concat "\n" (List.rev content)

let () =
  let content = read_stdin () in
  let json = Liquidsoap_tooling.Parsed_json.parse_string content in
  Printf.printf "%s\n%!" (Json.to_string ~compact:false json)
