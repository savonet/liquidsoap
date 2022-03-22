include MetadataBase (* for the exception Invalid *)
module ID3v1 = MetadataID3v1
module ID3v2 = MetadataID3v2
module JPEG = MetadataJPEG
module PNG = MetadataPNG
module AVI = MetadataAVI
module MP4 = MetadataMP4

module ID3 = struct
  let parse f =
    let v2 = try ID3v2.parse f with _ -> [] in
    let v1 = try Reader.reset f; ID3v1.parse f with _ -> [] in
    v2@v1

  let parse_file = Reader.with_file parse
end

module Audio = struct
  let parse = first_valid [ID3v2.parse; ID3v1.parse]
  let parse_file = Reader.with_file parse
end

module Image = struct
  let parse = first_valid [JPEG.parse; PNG.parse]
  let parse_file = Reader.with_file parse
end

module Video = struct
  let parse = first_valid [AVI.parse; MP4.parse]
  let parse_file = Reader.with_file parse
end
