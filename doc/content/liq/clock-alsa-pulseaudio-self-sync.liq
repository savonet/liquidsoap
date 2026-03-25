s = mksafe(playlist("~/Music"))
output.alsa(self_sync=false, s)
output.pulseaudio(s)
