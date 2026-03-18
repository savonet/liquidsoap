# Start the icecast-compatible server
server = icecast.server(
  port=8000,
  password="source_password",
  config="/etc/liquidsoap/icecast.xml"
)

# Create a fallback source (e.g., playlist)
fallback_source = playlist("/path/to/music")

# When a DJ connects, log it and optionally do something
server.on_connect(fun (r) ->
  begin
    log.important("DJ connected on #{r.mount}")
    # You could trigger notifications, update a database, etc.
  end
)

server.on_disconnect(fun (r) ->
  log.important("DJ disconnected from #{r.mount}")
)

# Output the stream
# The server handles incoming connections automatically
# and creates output.harbor instances for each mount
