enable_replaygain_metadata()
enable_autocue_metadata()

audio = once(single("../media/@shine[channels=2].mp3"))

o = output.dummy(fallible=true, audio)
o.on_start(synchronous=true, test.pass)
