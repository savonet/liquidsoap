open Metadata

let () =
  let metadata = Reader.with_file MP4.parse Sys.argv.(1) in
  assert (List.assoc "title" metadata = "foo")
