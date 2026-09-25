transport =
  http.transport.tls(
    certificate="/etc/letsencrypt/live/example.com/fullchain.pem",
    key="/etc/letsencrypt/live/example.com/privkey.pem"
  )

s = input.harbor(transport=transport, port=8005, "live")

# Run `reload-certificate` from a certbot deploy hook after each renewal.
server.register(
  "reload-certificate",
  fun (_) ->
    begin
      transport.reload()
      "Done"
    end
)
