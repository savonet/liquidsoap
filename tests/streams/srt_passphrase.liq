def fn() =
 output.srt(%wav,blank(),mode="listener",passphrase="foobarfoobarfoobar",on_connect=test.pass)
 output.dummy(fallible=true,input.srt(mode="caller",passphrase="foobarfoobarfoobar"))
end

test.check(fn)
