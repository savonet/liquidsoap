let types =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  [
    Lang.int_t;
    a;
    Lang.list_t a;
    Lang.list_t Lang.int_t;
    Lang.tuple_t [Lang.string_t; Lang.int_t; a];
    Lang.fun_t [(false, "l", a)] b;
    (* Lang.ref_t Lang.float_t; *)
    Lang.getter_t Lang.bool_t;
    Lang.error_t;
  ]

let () =
  Printf.printf "Testing JSON serialization of types...\n\n%!";
  List.iter
    (fun a ->
      Printf.printf "# %s\n\n" (Type.to_string a);
      Printf.printf "%s\n\n" (Type.to_json a |> Json.to_string ~compact:true))
    types
