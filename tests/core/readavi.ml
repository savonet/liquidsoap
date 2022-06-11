let () =
  let f = "../testsrc.avi" in
  if not (Sys.file_exists f) then (
    Printf.printf "Test %s not found, exiting.\n" f;
    exit 0);
  let f = Unix.openfile f [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o640 in
  let h = Avi.Read.headers_simple (Unix.read f) in
  ignore h
