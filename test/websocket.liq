#!../src/liquidsoap ../scripts/utils.liq

set("log.file",false)
set("log.stdout",true)
set("log.level",5)
set("server.telnet",false)
set("request.metadata_decoders.duration",true)

s = input.harbor(port=1234, "/mount")
#s = input.harbor(port=8080, "/foo")
s = audio_to_stereo(s)
s = mksafe(s)

output.pulseaudio(s)
