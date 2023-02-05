let types = [Lang.int_t]

let () =
  Printf.printf "Testing JSON serialization of types...\n\n%!";
  List.iter
    (fun a ->
      Printf.printf "# %s\n\n" (Type.to_string a);
      Printf.printf "%s\n" (Type.to_json a |> Json.to_string ~compact:true))
    types
