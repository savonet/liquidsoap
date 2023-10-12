s = once(single("@mp3[mono].mp3"))

s = (s:source(audio=pcm_s16(mono)))

s = source.on_track(s, fun (_) -> test.pass())

output.dummy(fallible=true, s)
