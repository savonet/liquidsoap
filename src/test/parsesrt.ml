let () =
  let f = "test.srt" in
  if not (Sys.file_exists f) then (
    Printf.printf "Test %s not found, exiting.\n" f;
    exit 0);
  let h = Srt_parser.parse_file f in
  Printf.printf "Found %d subtitles.\n\n%!" (List.length h);
  List.iter
    (fun (((h1, m1, s1, n1), (h2, m2, s2, n2)), sub) ->
      Printf.printf "%02d:%02d:%02d,%03d --> %02d:%02d:%02d,%03d\n%s\n\n%!" h1
        m1 s1 n1 h2 m2 s2 n2 sub)
    h
