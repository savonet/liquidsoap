include MetadataBase (* for the exception Invalid *)
module ID3v2 = MetadataID3v2
module JPEG = MetadataJPEG
module PNG = MetadataPNG

module Image = struct
  let parse = first_valid [JPEG.parse; PNG.parse]
end
