%include "../test.liq"

settings.icecast.prefer_address := "ipv4"

# A `reload_on` getter that raises is logged and the current certificate kept:
# clients must still connect.
transport =
  http.transport.tls(
    reload_on={
      error.raise(
        error.invalid,
        "reload_on failed"
      )
    },
    certificate="./ssl.cert",
    key="./ssl.key"
  )
client = http.transport.tls(certificate="./ssl.cert")

port = 1451

def on_wake_up() =
  o =
    output.icecast(
      port=port,
      mount="tls_reload_on_error",
      connection_timeout=0.1,
      timeout=0.1,
      transport=client,
      %vorbis,
      sine()
    )
  o.on_connect(synchronous=true, test.pass)
end

i =
  input.harbor(buffer=2., port=port, transport=transport, "tls_reload_on_error")
i.on_wake_up(synchronous=false, on_wake_up)
output.dummy(fallible=true, i)
