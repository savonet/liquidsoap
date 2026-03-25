# input.srt controls its own latency and cannot be passed directly to crossfade — fails with Error 7.
s = mksafe(input.srt())
output.pulseaudio(crossfade(s))
