open Id3v2

let () =
  let fname =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "../test.mp3"
  in
  if not (Sys.file_exists fname) then (
    Printf.printf "File %s not found, exiting." fname;
    exit 0 );
  Printf.printf "Reading %s...\n%!" fname;
  let ic = open_in fname in
  let tags = parse (input ic) in
  let apic = try List.assoc "APIC" tags with Not_found -> "" in
  let tags = List.remove_assoc "APIC" tags in
  List.iter (fun (t, v) -> Printf.printf "%s: %s\n%!" t v) tags;
  if apic <> "" then (
    let apic = parse_apic apic in
    Printf.printf "\nAPIC\n";
    Printf.printf "mime: %s\n" apic.mime;
    Printf.printf "type: %d\n" apic.picture_type;
    Printf.printf "description: %s\n" apic.description )
