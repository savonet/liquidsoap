port = 6756
settings.icecast.prefer_address := "ipv4"

s = sine()

o = output.icecast(port=port, mount="icecast_no_restart", %mp3, s)

i = input.harbor(buffer=2., port=port, "icecast_no_restart")
d = output.dummy(fallible=true, i)

has_connected = ref(false)
pass = ref(true)

def on_error(~restart_in, err) =
  print(
    "Error: #{err}"
  )
  restart_in(null)
  thread.run(delay=4., {if pass() then test.pass() else test.fail() end})
end

def on_connect() =
  if
    has_connected()
  then
    test.fail()
  else
    has_connected := true
    o.on_error(on_error)
    i.shutdown()
    d.shutdown()
  end
end

o.on_connect(synchronous=true, on_connect)

def on_disconnect() =
  pass := true
end

o.on_disconnect(synchronous=true, on_disconnect)
