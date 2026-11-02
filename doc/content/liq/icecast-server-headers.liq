icecast.server(
  {
    source_password = "change-me",
    http_headers =
      icecast.server.headers.cors(origins=["https://radio.example"]),
    mount =
      fun (_) -> {http_headers = [{name = "X-Robots-Tag", value = "noindex"}]}
  }
)
