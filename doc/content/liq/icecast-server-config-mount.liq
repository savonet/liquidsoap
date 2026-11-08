config = icecast.server.config.load("/etc/icecast2/icecast.xml")

# /live.webm gets a dedicated encoder per listener, on top of what the file
# says about it; every other mount keeps the file's settings.
def mount(path) =
  m = config.mount(path)
  if path == "/live.webm" then (m ?? {}).{dedicated_encoder = true} else m end
end

icecast.server(config.{mount = mount})
