# Each listener of /live.mkv gets its own encoder; other mounts share one.
icecast.server(
  {
    source_password = "change-me",
    mount =
      fun (path) -> path == "/live.mkv" ? {dedicated_encoder = true} : null
  }
)
