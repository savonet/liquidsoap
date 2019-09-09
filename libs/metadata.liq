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
