icecast.server(
  {
    source_password = "change-me",
    status_access =
      fun (request) -> request.headers["x-secret"] == "my-secret-token"
  }
)
