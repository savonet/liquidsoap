mic = input.alsa()
output.file(%mp3, "backup.mp3", mic)
output.icecast(%mp3, mount="radio", mksafe(buffer(mic)))
