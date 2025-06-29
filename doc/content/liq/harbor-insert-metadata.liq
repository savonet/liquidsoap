s = sine()
# BEGIN

# s = some source

# The handler
def set_meta(request, response) =
  # Filter out unusual metadata
  meta = metadata.export(request.query)

  # Grab the returned message
  ret =
    if
      meta != []
    then
      s.insert_metadata(meta)
      "OK!"
    else
      "No metadata to add!"
    end

  response.html("<html><body><b>#{ret}</b></body></html>")
end

# Register handler on port 700
harbor.http.register(port=7000, method="GET", "/setmeta", set_meta)
