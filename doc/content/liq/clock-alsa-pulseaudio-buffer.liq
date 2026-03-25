s = mksafe(playlist("~/Music"))
output.alsa(fallible=true, buffer(s))
output.pulseaudio(s)
