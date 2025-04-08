let () =
  let fname = ref "" in
  let format = ref "id3v2" in
  let outfile = ref None in
  Arg.parse
    [
      ("-f", Arg.Set_string format, "File format.");
      ( "-o",
        Arg.String (fun s -> outfile := Some s),
        "Output file (default is standard output)." );
    ]
    (fun f -> fname := f)
    "dump [options] file";
  let dump =
    match !format with
      | "id3v2" -> Metadata.ID3v2.dump_file
      | _ -> failwith "Unknown format."
  in
  let fname = !fname in
  if fname = "" then (
    Printf.eprintf "Please enter a filename.\n%!";
    exit 1);
  let oc = match !outfile with Some f -> open_out f | None -> stdout in
  output_string oc (dump fname)
