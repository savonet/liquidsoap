s = once(single("@mp3[mono].mp3"))
s = (s : source(audio=pcm_s16(mono)))
s.on_track(fast=true, fun (_) -> test.pass())
output.dummy(fallible=true, s)
