let () =
  let fname =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "../test.ogg"
  in
  if not (Sys.file_exists fname) then (
    Printf.printf "File %s not found, exiting." fname ;
    exit 0 ) ;
  Printf.printf "Reading %s...\n%!" fname ;
  let ic = open_in fname in
  let tags = VorbisComment.parse (input ic) in
  Printf.printf "Tags:\n%!";
  List.iter (fun (k,v) -> Printf.printf "%s: %s\n%!" k v) tags
