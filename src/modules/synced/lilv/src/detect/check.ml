let () =
  let name = Sys.argv.(1) in
  let available = String.trim (input_line stdin) in
  if available <> "true" then (
    Printf.eprintf "Error: %s C library not found via pkg-config.\n" name;
    exit 1)
