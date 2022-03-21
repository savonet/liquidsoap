include MetadataBase (* for the exception Invalid *)
module ID3v2 = MetadataID3v2
module JPEG = MetadataJPEG
module PNG = MetadataPNG
module AVI = MetadataAVI

module Image = struct
  let parse = first_valid [JPEG.parse; PNG.parse]
end

module Video = struct
  let parse = first_valid [AVI.parse]
end
