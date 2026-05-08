val parse : MetadataBase.Reader.t -> MetadataBase.metadata

val parse_file :
  ?custom_parser:MetadataBase.custom_parser -> string -> MetadataBase.metadata

type picture = {
  picture_type : int;
  picture_mime : string;
  picture_description : string;
  picture_width : int;
  picture_height : int;
  picture_bpp : int;
  picture_colors : int;
  picture_data : string;
}

val parse_picture : string -> picture
