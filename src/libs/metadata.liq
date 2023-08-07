let metadata.getter = ()

# Create a getter from a metadata.
# @category Metadata
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
  s.on_metadata(f)
  ref.getter(x)
end

# Create a getter from a metadata: this is a string, whose value can be changed
# with a metadata.
# @category Metadata
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def replaces metadata.getter(init, m, s)
  metadata.getter.base(init, fun (v) -> v, m, s)
end

# Create a float getter from a metadata: this is a float, whose value can be
# changed with a metadata.
# @category Metadata
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.getter.float(init, m, s)
  metadata.getter.base(init, float_of_string, m, s)
end

# Extract filename from metadata.
# @category Metadata
def metadata.filename(m)
  m["filename"]
end

# Extract title from metadata.
# @category Metadata
def metadata.title(m)
  m["title"]
end

# Extract artist from metadata.
# @category Metadata
def metadata.artist(m)
  m["artist"]
end

# Extract comment from metadata.
# @category Metadata
def metadata.comment(m)
  m["comment"]
end

# Extract cover from metadata. This function implements cover extraction
# for the following formats: coverart (ogg), apic (flac, mp3) and pic (mp3).
# @category Metadata
# @param m Metadata from which the cover should be extracted.
# @param ~coverart_mime Mime type to use for `"coverart"` metadata. Support disabled if `null`.
# @method mime MIME type for the cover.
def metadata.cover(~coverart_mime=null(), m)
  fname = metadata.filename(m)
  if list.assoc.mem("coverart", m) and null.defined(coverart_mime) then
    cover = list.assoc(default="", "coverart", m)
    string.base64.decode(cover).{mime=null.get(coverart_mime)}
  elsif list.assoc.mem("metadata_block_picture", m) then
    # See https://xiph.org/flac/format.html#metadata_block_picture
    cover = list.assoc(default="", "metadata_block_picture", m)
    cover = file.metadata.flac.cover.decode(cover)
    if not null.defined(cover) then
      log.info("Failed to read cover metadata for #{fname}.")
      null()
    else
      null.get(cover)
    end
  else
    # Assume we have an mp3 file
    m =
      if list.assoc.mem("apic", m) or list.assoc.mem("pic", m) then
        m
      else
        # Try the builtin tag reader because APIC tags are not read by default,
        # e.g. with taglib
        log.debug(label="metadata.cover", "APIC or PIC not found for #{fname}, trying builtin tag reader.")
        file.metadata.id3v2(fname)
      end
    pic = list.assoc(default="", "pic", m)
    apic = list.assoc(default="", "apic", m)
    if apic != "" then
      log.debug(label="metadata.cover", "Found APIC for #{fname}.")
      # TODO: we could use file type in order to select cover if there are many
      string.apic.parse(apic)
    elsif pic != "" then
      log.debug(label="metadata.cover", "Found APIC for #{fname}.")
      # TODO: we could use file type in order to select cover if there are many
      pic = string.pic.parse(pic)
      mime =
        if pic.format == "JPG" then
          "image/jpeg"
         elsif pic.format == "PNG" then
           "image/png"
         else
           "application/octet-stream"
         end
      pic.{mime=mime}
    else
      log.info("No cover found for #{fname}.")
      null()
    end
  end
end

# Obtain cover-art for a file. `null` is returned in case there is no
# such information.
# @category Metadata
# @param file File from which the cover should be obtained
def file.cover(fname)
  metadata.cover(file.metadata(fname))
end

# Remove cover metadata. This is mostly useful in order not to flood logs
# with coverart when logging metadata.
# @category Metadata
def metadata.cover.remove(m)
  list.assoc.filter(fun (k, _) -> k != "metadata_block_picture" and k != "apic" and k != "pic" and k != "cover", m)
end

# Cleanup metadata for export. This is used to remove Liquidsoap's internal
# metadata entries before sending them. List of exported metadata is set using
# `settings.encoder.metadata.export.set`.
# @category Metadata
def metadata.export(m) =
  exported_keys = settings.encoder.metadata.export()
  list.assoc.filter((fun (k, (_:string)) -> list.mem(k, exported_keys)), m)
end

let metadata.json = ()

# Export metadata as JSON object. Cover art, if found, is extracted using
# `metadata.cover` and exported with key `"cover"` and exported using
# `string.data_uri.encode`.
# @category Metadata
# @param ~coverart_mime Mime type to use for `"coverart"` metadata. Support disasbled if `null`.
# @param ~compact Output compact text.
# @param ~json5 Use json5 extended spec.
def metadata.json.stringify(~coverart_mime=null(), ~base64=true, ~compact=false, ~json5=false, m) =
  c = metadata.cover(coverart_mime=coverart_mime, m)
  m = metadata.cover.remove(m)
  m = metadata.export(m)

  m =
    if null.defined(c) then
      c = null.get(c)
      [("cover", string.data_uri.encode(base64=base64, mime=c.mime, c)), ...m]
    else
      m
    end

  data = json()
  list.iter(fun (v) -> data.add(fst(v),snd(v)), m)
  json.stringify(json5=json5,compact=compact,data)
end

# Parse metadata from JSON object.
# @category Metadata
def metadata.json.parse(json_string) =
  let json.parse ( metadata_list : [(string * string)] as json.object ) = json_string
  metadata_list
end
