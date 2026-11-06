# The key must stay secret: whoever holds it can check guessed addresses.
icecast.server(
  {
    source_password = "change-me",
    address_hash = fun (ip) -> string.digest("my-secret-key" ^ ip)
  }
)
