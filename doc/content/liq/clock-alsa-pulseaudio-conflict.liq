# Two simultaneous sync sources in the same clock — fails with Error 17 at startup.
s = mksafe(playlist("~/Music"))
output.alsa(s)
output.pulseaudio(s)
