let detected =
  let dep = Filename.basename (List.hd (String.split_on_char '_' __FILE__)) in
  [%string "no (requires %{dep})"]

let enabled = false
