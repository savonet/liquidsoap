enable_replaygain_metadata()

s = single("../media/@wav[stereo].wav")

s = replaygain(s)

s = source.on_track(s, fun (_) -> test.pass())

output.dummy(fallible=true, s)
