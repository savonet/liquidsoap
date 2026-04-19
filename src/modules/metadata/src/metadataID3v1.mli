val parse :
  ?recode:MetadataCharEncoding.recode ->
  MetadataBase.Reader.t ->
  MetadataBase.metadata

val parse_file :
  ?custom_parser:MetadataBase.custom_parser -> string -> MetadataBase.metadata
