val parse : ?format:string -> MetadataBase.Reader.t -> MetadataBase.metadata

val parse_file :
  ?format:string ->
  ?custom_parser:MetadataBase.custom_parser ->
  string ->
  MetadataBase.metadata
