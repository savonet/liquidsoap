(** Basic usage of the library. *)

let () =
  let metadata = Metadata.parse_file "test.mp3" in
  List.iter (fun (k, v) -> Printf.printf "- %s: %s\n" k v) metadata
