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

# Extract cover from metadata. This function implements cover extraction
# for the following formats: coverart (ogg) and apic (flac, mp3). 
# @category Source / Track Processing
# @param m Metadata from which the cover should be extracted.
# @param ~coverart_mime Mime type to use for `"coverart"` metadata. Support disasbled if `null`.
# @method mime MIME type for the cover.
def metadata.cover(~coverart_mime=null(), m)
  fname = metadata.filename(m)
  if list.assoc.mem("coverart", m) and null.defined(coverart_mime) then
    cover = list.assoc(default="", "coverart", m)
    null(string.base64.decode(cover).{mime=null.get(coverart_mime)})
  elsif list.assoc.mem("metadata_block_picture", m) then
    # See https://xiph.org/flac/format.html#metadata_block_picture
    cover = list.assoc(default="", "metadata_block_picture", m)
    cover = file.metadata.flac.cover.decode(string.base64.decode(cover))
    if not null.defined(cover) then
      log.info("Failed to read cover metadata for #{fname}.")
      null()
    else
      cover = null.get(cover)
      null((cover:string).{mime=cover.mime})
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
        file.metadata.id3v2(fname)
      end
    apic = list.assoc(default="", "apic", m)
    if apic == "" then
      log.info("No cover found for #{fname}.")
      null()
    else
      log.debug(label="metadata.cover", "Found APIC for #{fname}.")
      # TODO: we could use file type in order to select cover if there are many
      meta = string.apic.parse(apic)
      null((meta:string).{mime=meta.mime})
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

# Cleanup metadata for export. This is used to remove Liquidsoap's internal
# metadata entries before sending them. List of exported metadata is set using
# `settings.encoder.metadata.export.set`.
# @category Source / Track Processing
def metadata.export(m) =
  exported_keys = settings.encoder.metadata.export()
  list.assoc.filter((fun (k, (_:string)) -> list.mem(k, exported_keys)), m)
end

let metadata.json = ()

# Export metadata as JSON object. Cover art, if found, is extracted using
# `metadata.cover` and exported with key `"cover"` and exported using
# `string.data_uri.encode`.
# @category String 
# @param ~coverart_mime Mime type to use for `"coverart"` metadata. Support disasbled if `null`.
# @param ~base64 Encode cover data as `base64`
# @argsof json.stringify
def metadata.json.stringify(~coverart_mime=null(), ~base64=true, %argsof(json.stringify), m) =
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
  j = json()
  list.iter((fun (v) -> j.add(fst(v), (snd(v):string))), m)
  json.stringify(%argsof(json.stringify), j)
end

# Parse metadata from JSON object
# @category String
def metadata.json.parse(s) =
  m = json.parse(default=[("invalid","invalid")], s)
  if m == [("invalid","invalid")] then [] else m end
end
