let () =
  let ic = open_in_bin Sys.argv.(1) in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  let normalized = String.split_on_char '\r' content |> String.concat "" in
  print_string normalized
