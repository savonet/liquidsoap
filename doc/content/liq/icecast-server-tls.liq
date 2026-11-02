icecast.server(
  {
    source_password = "change-me",
    port = 8443,
    transport =
      http.transport.tls(
        certificate="/etc/radio/cert.pem",
        key="/etc/radio/key.pem"
      )
  }
)
