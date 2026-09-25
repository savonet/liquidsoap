config = "/etc/icecast.xml"
server = icecast.server(config=config)

def reload() =
  try
    server.reload()
  catch err do
    log.important(
      "Config not reloaded: #{err.message}"
    )
  end
end

ignore(file.watch(config, reload))
