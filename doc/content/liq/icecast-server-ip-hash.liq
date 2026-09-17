# The key must stay secret: whoever holds it can check guessed addresses.
icecast.server(
  password="hackme",
  ip_hash=fun (ip) -> string.digest("my-secret-key" ^ ip)
)
