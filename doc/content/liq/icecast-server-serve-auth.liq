icecast.server(
  serve_auth=fun (req) -> req.headers["x-secret"] == "my-secret-token"
)
