# Create a getter from a metadata.
# @category Interaction
# @flag hidden
# @param init Initial value.
# @param map Function to apply to the metadata value to obtain the new value.
# @param metadata Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.getter(init, map, metadata, s)
  x = ref(init)
  def f(m)
    v = m[metadata]
    if v != "" then x := map(v) end
  end
  s.on_metadata(f)
  (s, {!x})
end

# Create a string getter from a metadata: this is a string, whose value can be changed with a metadata.
# @category Interaction
# @flag extra
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.string_getter(init, m, s)
  metadata.getter(init, fun (v) -> v, m, s)
end

# Create a float getter from a metadata: this is a float, whose value can be changed with a metadata.
# @category Interaction
# @flag extra
# @param init Initial value.
# @param m Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.float_getter(init, m, s)
  metadata.getter(init, float_of_string, m, s)
end

# Obtain cover-art for a file. An empty string is returned in case there is no
# such information.
# @category System
# @flag extra
# @param file File from which the cover should be obtained
def file.cover(~id="file.cover", fname)
  m = file.metadata(fname)
  m = list.map (fun (lv) -> (string.case(lower=true, fst(lv)), snd(lv)), m)
  if list.mem_assoc("coverart", m) or list.mem_assoc("metadata_block_picture", m)then
    # We are likely to have an ogg/vorbis file
    cover = list.assoc(default="", "coverart", m)
    if cover != "" then
      string.base64.decode(cover)
    else
      cover = list.assoc(default=cover, "metadata_block_picture", m)
      cover = string.base64.decode(cover)
      i = ref(4)
      mime_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4 + mime_len
      desc_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4 + desc_len
      i := !i + 4 * 4
      pic_len = string.binary.to_int(little_endian=false, string.sub(cover, start=!i, length=4))
      i := !i + 4
      cover = string.sub(cover, start=!i, length=pic_len)
      if cover == "" then log.info("Failed to read cover metadata for #{file}.") end
      cover
    end
  else
    # Assume we have an mp3 file
    m =
      if list.mem_assoc("apic", m) then
        m
      else
        # Try the builtin tag reader because APIC tags are not read by default,
        # e.g. with taglib
        log.debug(label=id, "APIC not found for #{file}, trying builtin tag reader.")
        file.mp3.metadata(fname)
      end
    apic = list.assoc(default="", "apic", m)
    if apic == "" then
      log.info("No cover found for #{file}.")
      ""
    else
      log.debug(label=id, "Found APIC for #{file}.")
      # TODO: we could use file type in order to select cover if there are many
      let (_,_,_,data) = file.mp3.parse_apic(apic)
      data
    end
  end
end
