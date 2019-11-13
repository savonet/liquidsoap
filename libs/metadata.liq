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
  s = on_metadata(f, s)
  (s, {!x})
end

# Create a string getter from a metadata: this is a string, whose value can be changed with a metadata.
# @category Interaction
# @param init Initial value.
# @param metadata Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.string_getter(init, metadata, s)
  metadata.getter(init, fun (v) -> v, metadata, s)
end

# Create a float getter from a metadata: this is a float, whose value can be changed with a metadata.
# @category Interaction
# @param init Initial value.
# @param metadata Metadata on which the value should be updated.
# @param s Source containing the metadata.
def metadata.float_getter(init, metadata, s)
  metadata.getter(init, float_of_string, metadata, s)
end

# Obtain cover-art for a file. Only MP3 files are supported for now, athough
# more should be in the future. An empty string is return in case there is no
# such information.
# @param file File from which the cover should be obtained
def file.cover(file)
  m = file.mp3.tags(file)
  m = list.filter(fun (lv) -> fst(lv) == "APIC", m)
  m = list.map(snd, m)
  if m == [] then "" else
    # TODO: we could use file type in order to select cover if there are many
    let (_,_,_,data) = file.mp3.parse_apic(list.hd(default="", m))
    data
  end
end
