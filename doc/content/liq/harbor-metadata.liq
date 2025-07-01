s = sine()

# BEGIN
meta = ref([])

# s = some source
s.on_metadata(fun (m) -> meta := m)

# Return the json content of meta
def get_meta(_, response) =
  response.json(meta())
end

# Register get_meta at port 700
harbor.http.register(port=7000, method="GET", "/getmeta", get_meta)
