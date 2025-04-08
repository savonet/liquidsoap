module CharEncoding = MetadataCharEncoding
module MIME = MetadataMIME

module Make (E : CharEncoding.T) = struct
  include MetadataBase
  module ID3v1 = MetadataID3v1
  module ID3v2 = MetadataID3v2
  module OGG = MetadataOGG
  module FLAC = MetadataFLAC
  module JPEG = MetadataJPEG
  module PNG = MetadataPNG
  module AVI = MetadataAVI
  module MP4 = MetadataMP4
  module WAV = MetadataWAV
  module RIFF = MetadataRIFF

  (** Charset conversion function. *)
  let recode = E.convert

  module ID3 = struct
    let parse f =
      let failure, v2 =
        try (false, ID3v2.parse ~recode f) with _ -> (true, [])
      in
      let v1 =
        try
          Reader.reset f;
          ID3v1.parse ~recode f
        with _ -> if failure then raise Invalid else []
      in
      v2 @ v1

    let parse_file ?custom_parser file =
      Reader.with_file ?custom_parser parse file
  end

  let rec first_valid l file =
    match l with
      | f :: l -> (
          try f file
          with Invalid ->
            Reader.reset file;
            first_valid l file)
      | [] -> raise Invalid

  module Audio = struct
    let parsers = [ID3.parse; OGG.parse; FLAC.parse; WAV.parse]
    let parse = first_valid parsers

    let parse_file ?custom_parser file =
      Reader.with_file ?custom_parser parse file
  end

  module Image = struct
    let parsers = [JPEG.parse; PNG.parse]
    let parse = first_valid parsers

    let parse_file ?custom_parser file =
      Reader.with_file ?custom_parser parse file
  end

  module Video = struct
    let parsers = [AVI.parse; MP4.parse]
    let parse = first_valid parsers

    let parse_file ?custom_parser file =
      Reader.with_file ?custom_parser parse file
  end

  module Any = struct
    let parsers = Audio.parsers @ Image.parsers @ Video.parsers @ [RIFF.parse]

    (** Genering parsing of metadata. *)
    let parse = first_valid parsers

    let parse_file ?custom_parser file =
      Reader.with_file ?custom_parser parse file

    (** Parse the metadatas of a string. *)
    let parse_string ?custom_parser file =
      Reader.with_string ?custom_parser parse file
  end

  include Any
end

include Make (CharEncoding.Naive)
