let () =
  let f = "test.srt" in
  if not (Sys.file_exists f) then (
    Printf.printf "Test %s not found, exiting.\n" f;
    exit 0);
  let h = Srt_parser.parse_file f in
  Printf.printf "Found %d subtitles.\n%!" (List.length h)
