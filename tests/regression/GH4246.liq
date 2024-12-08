enable_replaygain_metadata()
enable_autocue_metadata()

audio = once(single("../media/@shine[channels=2].mp3"))

output.dummy(fallible=true, on_start=test.pass, audio)
