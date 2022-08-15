log.level.set(4)

def fn() =
 def listen_callback(~hs_version=_, ~peeraddr=_, ~streamid, _) =
   if streamid == null("foobar") then test.pass() end
   false
 end


 output.srt(%wav,blank(),mode="listener",listen_callback=listen_callback)
 output.dummy(mksafe(input.srt(mode="caller",streamid="foobar")))
end

test.check(fn)
