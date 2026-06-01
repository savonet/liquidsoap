let () =
  let ic = open_in_bin Sys.argv.(1) in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  let normalized = String.split_on_char '\r' content |> String.concat "" in
  let oc = open_out Sys.argv.(2) in
  output_string oc normalized;
  close_out oc
