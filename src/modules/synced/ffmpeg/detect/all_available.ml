let () =
  let files =
    Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list
  in
  let all_available =
    List.for_all
      (fun path ->
        let ic = open_in path in
        let value = input_line ic in
        close_in ic;
        value = "true")
      files
  in
  print_string (if all_available then "true" else "false")
