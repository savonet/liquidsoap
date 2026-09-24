%include "../test.liq"

settings.icecast.prefer_address := "ipv4"

# The server starts on certificate A while the client only trusts B: the
# client must keep failing until B is set and `reload()` is called.
certificate = ref("./ssl.cert")
key = ref("./ssl.key")
transport = http.transport.ssl(certificate={certificate()}, key={key()})
client = http.transport.ssl(certificate="./ssl2.cert")

port = 1447
errors = ref(0)
reloaded = ref(false)

def on_error(~restart_in, _) =
  errors := errors() + 1
  if
    errors() == 2
  then
    certificate := "./missing.cert"
    raised =
      try
        transport.reload()
        false
      catch _ do
        true
      end
    test.equal(raised, true)

    certificate := "./ssl2.cert"
    key := "./ssl2.key"
    transport.reload()
    reloaded := true
  end
  restart_in(0.1)
end

def on_wake_up() =
  o =
    output.icecast(
      port=port,
      mount="ssl_reload",
      connection_timeout=0.1,
      timeout=0.1,
      transport=client,
      %vorbis,
      sine()
    )
  o.on_error(synchronous=true, on_error)
  o.on_connect(
    synchronous=true,
    fun () -> if reloaded() then test.pass() else test.fail() end
  )
end

i = input.harbor(buffer=2., port=port, transport=transport, "ssl_reload")
i.on_wake_up(synchronous=false, on_wake_up)
output.dummy(fallible=true, i)
