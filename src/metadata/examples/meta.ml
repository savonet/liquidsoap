let () =
  let fname = ref [] in
  let format = ref "" in
  Arg.parse
    [
      ("--format", Arg.Set_string format, "File format.");
      ("-f", Arg.Set_string format, "File format.");
    ]
    (fun f -> fname := f :: !fname)
    "meta [options] file";
  let parser =
    match !format with
      | "id3" | "mp3" -> Metadata.ID3.parse_file
      | "id3v1" -> Metadata.ID3v1.parse_file
      | "id3v2" -> Metadata.ID3v2.parse_file
      | "ogg" -> Metadata.OGG.parse_file
      | "mp4" -> Metadata.MP4.parse_file
      | "" -> Metadata.Any.parse_file
      | _ -> failwith "Unknown format."
  in
  let fname = !fname in
  if fname = [] then (
    Printf.eprintf "Please enter a filename.";
    exit 1);
  List.iter
    (fun fname ->
      Printf.printf "\n# %s\n\n%!" fname;
      let m = parser fname in
      List.iter
        (fun (k, v) ->
          let v = if k = "APIC" || k = "PIC" then "<redacted>" else v in
          Printf.printf "- %s: %S\n%!" k v)
        m)
    fname
