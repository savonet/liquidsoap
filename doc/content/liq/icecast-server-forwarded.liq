# Cloudflare sends the client's address in its own header.
icecast.server(
  {
    source_password = "change-me",
    forwarded_address =
      fun (request) -> list.assoc.nullable("cf-connecting-ip", request.headers)
  }
)
