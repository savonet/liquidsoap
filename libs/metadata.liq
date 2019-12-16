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

# Read metadata from a file.
# @category System
# @param URI of the file for which the metadata should be read.
def file.metadata(file)
  r = request.create(file)
  if request.resolve(r) then
    request.metadata(r)
  else
    []
  end
end

# Obtain cover-art for a file. An empty string is returned in case there is no
# such information.
# @category System
# @param file File from which the cover should be obtained
def file.cover(file)
  m = file.metadata(file)
  m = list.map (fun (lv) -> (string.case(lower=true, fst(lv)), snd(lv)), m)
  if list.mem_assoc("coverart", m) then
    # We are likely to have an ogg/vorbis file
    m = list.filter(fun (lv) -> fst(lv) == "coverart", m)
    m = list.map(snd, m)
    m = list.hd(default="", m)
    if m == "" then "" else string.base64.decode(m) end
  else
    # Assume we have an mp3 file
    m =
      if list.exists(fun (lv) -> fst(lv) == "apic", m) then
        m
      else
        # Try the builting tag reader because APIC tags are not read by default,
        # e.g. with taglib
        file.mp3.metadata(file)
      end
    m = list.filter(fun (lv) -> fst(lv) == "apic", m)
    m = list.map(snd, m)
    if m == [] then
      ""
    else
      # TODO: we could use file type in order to select cover if there are many
      let (_,_,_,data) = file.mp3.parse_apic(list.hd(default="", m))
      data
    end
  end
end
