%include "../test.liq"

settings.icecast.prefer_address := "ipv4"

# The server starts on certificate A while the client only trusts B: the
# client connects once B is set and `reload_on` returns `true`.
certificate = ref("./ssl.cert")
key = ref("./ssl.key")
should_reload = ref(false)
transport =
  http.transport.tls(
    reload_on={should_reload()},
    certificate={certificate()},
    key={key()}
  )
client = http.transport.tls(certificate="./ssl2.cert")

port = 1449
errors = ref(0)

def on_error(~restart_in, _) =
  errors := errors() + 1
  if
    errors() == 2
  then
    certificate := "./ssl2.cert"
    key := "./ssl2.key"
  elsif
    errors() == 3
  then
    should_reload := true
  end
  restart_in(0.1)
end

def on_wake_up() =
  o =
    output.icecast(
      port=port,
      mount="tls_reload_on",
      connection_timeout=0.1,
      timeout=0.1,
      transport=client,
      %vorbis,
      sine()
    )
  o.on_error(synchronous=true, on_error)
  o.on_connect(
    synchronous=true,
    fun () -> if should_reload() then test.pass() else test.fail() end
  )
end

i = input.harbor(buffer=2., port=port, transport=transport, "tls_reload_on")
i.on_wake_up(synchronous=false, on_wake_up)
output.dummy(fallible=true, i)
