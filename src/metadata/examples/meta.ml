let () =
  let fname = ref "" in
  let format = ref "" in
  Arg.parse
    [
      ("--format", Arg.Set_string format, "File format.");
      ("-f", Arg.Set_string format, "File format.");
    ]
    (fun f -> fname := f)
    "meta [options] file";
  let parser =
    match !format with
      | "id3" | "mp3" -> Metadata.ID3.parse_file
      | "id3v1" -> Metadata.ID3v1.parse_file
      | "id3v2" -> Metadata.ID3v2.parse_file
      | "mp4" -> Metadata.MP4.parse_file
      | "" -> Metadata.Any.parse_file
      | _ -> failwith "Unknown format."
  in
  let m = parser !fname in
  List.iter
    (fun (k, v) ->
      let v = if k = "APIC" then "<redacted>" else v in
      Printf.printf "- %s: %s\n%!" k v)
    m
