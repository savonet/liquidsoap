let log = Log.make ["decoder";"file"]

let () =
  Decoder.uri_decoders#register "file"
  ~sdoc:"Decode a local file."
  (fun ~metadata uri kind ->
    match Decoder.get_file_decoder ~metadata uri.URI.path kind with
      | None -> None
      | Some (_, f) -> Some (f)
  )
