config = "/etc/icecast2/icecast.xml"

# Every reload computes the whole record again, overrides included.
def parameters() =
  icecast.server.config.load(compatibility="icecast-2.4", config).{port = 8100}
end

server = icecast.server(parameters())

def reload() =
  try
    server.reload(parameters())
  catch err do
    log.important(
      "Configuration not reloaded: #{err.message}"
    )
  end
end

ignore(on_signal("hup", reload))
ignore(file.watch(config, reload))
