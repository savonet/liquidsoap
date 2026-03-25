# All three operators share the ALSA clock — Icecast network stalls affect file recording.
mic = input.alsa()
output.file(%mp3, "backup.mp3", mic)
output.icecast(%mp3, mount="radio", mic)
