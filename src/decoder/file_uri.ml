let log = Log.make ["decoder";"file"]

let () =
  Decoder.uri_decoders#register "file"
  ~sdoc:"Decode a local file."
  (fun ~metadata:_ uri kind ->
    let filename = uri.URI.path in
    Decoder.get_file_decoder ~metadata filename kind
  )
