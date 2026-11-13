def parameters() =
  cache = icecast.server.preroll_cache(directory="/srv/radio/jingles")
  {
    source_password = "change-me",
    mount =
      fun (_) ->
        {listener_decision = fun (_) -> {preroll = cache("welcome.mp3")}}
  }
end

server = icecast.server(parameters())
