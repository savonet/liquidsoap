let metadata.getter = ()

# Create a getter from a metadata.
# @category Interaction
# @flag hidden
# @param init Initial value.
# @param map Function to apply to the metadata value to obtain the new value.
# @param metadata Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.getter.base(init, map, metadata, s)
  x = ref(init)
  def f(m)
    v = m[metadata]
    if v != "" then x := map(v) end
  end
  source.on_metadata(s, f)
  {!x}
end

# Create a getter from a metadata: this is a string, whose value can be changed
# with a metadata.
# @category Interaction
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def replaces metadata.getter(init, m, s)
  metadata.getter.base(init, fun (v) -> v, m, s)
end

# Create a float getter from a metadata: this is a float, whose value can be
# changed with a metadata.
# @category Interaction
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.getter.float(init, m, s)
  metadata.getter.base(init, float_of_string, m, s)
end

# Extract filename from metadata.
# @category Source / Track Processing
def metadata.filename(m)
  m["filename"]
end

# Extract title from metadata.
# @category Source / Track Processing
def metadata.title(m)
  m["title"]
end

# Extract artist from metadata.
# @category Source / Track Processing
def metadata.artist(m)
  m["artist"]
end

# Extract comment from metadata.
# @category Source / Track Processing
def metadata.comment(m)
  m["comment"]
end

# Extract cover from metadata.
# @category Source / Track Processing
# @param m Metadata from which the cover should be extracted.
# @method mime MIME type for the cover.
def metadata.cover(m)
  fname = metadata.filename(m)
  if list.assoc.mem("coverart", m) or list.assoc.mem("metadata_block_picture", m)then
    # We are likely to have an ogg/vorbis file
    cover = list.assoc(default="", "coverart", m)
    if cover != "" then
      string.base64.decode(cover).{mime=""}
    else
      # See https://xiph.org/flac/format.html#metadata_block_picture
      cover = list.assoc(default=cover, "metadata_block_picture", m)
      cover = string.base64.decode(cover)
      i = ref(4)
      mime_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4
      mime = string.sub(cover, start=!i, length=mime_len)
      i := !i + mime_len
      desc_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4 + desc_len
      i := !i + 4 * 4
      pic_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4
      cover = string.sub(cover, start=!i, length=pic_len)
      if cover == "" then log.info("Failed to read cover metadata for #{fname}.") end
      cover.{mime=mime}
    end
  else
    # Assume we have an mp3 file
    m =
      if list.assoc.mem("apic", m) then
        m
      else
        # Try the builtin tag reader because APIC tags are not read by default,
        # e.g. with taglib
        log.debug(label="metadata.cover", "APIC not found for #{fname}, trying builtin tag reader.")
        file.mp3.metadata(fname)
      end
    apic = list.assoc(default="", "apic", m)
    if apic == "" then
      log.info("No cover found for #{fname}.")
      "".{mime=""}
    else
      log.debug(label="metadata.cover", "Found APIC for #{fname}.")
      # TODO: we could use file type in order to select cover if there are many
      let (mime,_,_,data) = file.mp3.parse_apic(apic)
      data.{mime=mime}
    end
  end
end

# Obtain cover-art for a file. An empty string is returned in case there is no
# such information.
# @category System
# @param file File from which the cover should be obtained
def file.cover(fname)
  metadata.cover(file.metadata(fname))
end

# Remove cover metadata. This is mostly useful in order not to flood logs
# with coverart when logging metadata.
# @category Source / Track Processing
def metadata.cover.remove(m)
  list.assoc.filter(fun (k, _) -> k != "metadata_block_picture" and k != "apic", m)
end
