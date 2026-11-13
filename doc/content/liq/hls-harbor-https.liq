radio = mksafe(playlist("~/music"))

# BEGIN
transport =
  http.transport.ssl(
    certificate="/etc/ssl/radio.example.org.pem",
    key="/etc/ssl/radio.example.org.key"
  )

output.harbor.hls(
  transport=transport,
  port=8443,
  tmpdir="/var/lib/radio/hls",
  persist_at="state.json",
  headers=[("Access-Control-Allow-Origin", "https://player.example.org")],
  [("aac", %fdkaac(bitrate = 128))],
  radio
)
# END
