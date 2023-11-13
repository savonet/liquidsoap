# Log to stdout
log.file := false
log.stdout := true
log.level := 4

# Initial playlist
cue = "/path/to/sheet.cue"

# Create a playlist with this CUE sheet and tell Liquidsoap to shutdown when we
# are done.
s = playlist(cue, on_done=shutdown)

# Add a cue_cut to cue-in/cue-out according to markers in `sheet.cue`.
s = cue_cut(s)

# Shove all that to a output.file operator.
output.file(%mp3(id3v2=true,bitrate=320),
            fallible=true,
            reopen_on_metadata=fun(_) -> true,
            "/path/to/$(track) - $(title).mp3",
            s)
