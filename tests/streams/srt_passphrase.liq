port = 8003

def fn() =
  output.srt(
    %wav,
    blank(),
    port=port,
    mode="listener",
    passphrase="foobarfoobarfoobar",
    on_connect=test.pass
  )

  output.dummy(
    fallible=true,
    input.srt(mode="caller", port=port, passphrase="foobarfoobarfoobar")
  )
end

test.check(fn)
