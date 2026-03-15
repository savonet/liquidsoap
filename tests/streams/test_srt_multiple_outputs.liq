port = 8002
settings.srt.prefer_address := "ipv4"

def fn() =
  connected = ref(0)

  def on_connect() =
    connected := !connected + 1
    if !connected == 2 then test.pass() end
  end

  o = output.srt(%wav, blank(), port=port, mode="listener")
  o.on_connect(synchronous=true, on_connect)
  output.dummy(fallible=true, input.srt(mode="caller", port=port))
  output.dummy(fallible=true, input.srt(mode="caller", port=port))
end

test.check(fn)
