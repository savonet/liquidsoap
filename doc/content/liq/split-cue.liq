# Log to stdout
log.file.set(false)
log.stdout.set(true)
log.level.set(4)

# Initial playlist
cue = "/path/to/sheet.cue"

# Create a reloadable playlist with this CUE sheet and tell liquidsoap to
# shutdown when we are done.
x = playlist.reloadable(cue, on_done=shutdown)

# We will never reload this playlist so we drop the first returned value:
s = snd(x)

# Add a cue_cut to cue-in/cue-out according to markers in "sheet.cue"
s = cue_cut(s)

# Shove all that to a output.file operator.
output.file(%mp3(id3v2=true,bitrate=320),
            fallible=true,
            reopen_on_metadata=true,
            "/path/to/$(track) - $(title).mp3",
            s)
