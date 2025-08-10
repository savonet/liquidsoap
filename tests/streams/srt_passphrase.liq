port = 8003
settings.srt.prefer_address := "ipv4"

def fn() =
  o =
    output.srt(
      %wav, blank(), port=port, mode="listener", passphrase="foobarfoobarfoobar"
    )

  o.on_connect(synchronous=true, test.pass)

  output.dummy(
    fallible=true,
    input.srt(mode="caller", port=port, passphrase="foobarfoobarfoobar")
  )
end

test.check(fn)
