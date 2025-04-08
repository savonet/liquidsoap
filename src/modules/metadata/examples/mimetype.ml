let () =
  let fname = Sys.argv.(1) in
  let mime = try Metadata.MIME.of_file fname with Not_found -> "unknown" in
  print_endline mime
