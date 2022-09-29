Split and re-encode a CUE sheet.
================================
CUE sheets are sometimes distributed along with a single audio file containing a whole CD.
Liquidsoap can parse CUE sheets as playlists and use them in your request-based sources.

Here's for instance an example of a simple code to split a CUE sheet into several mp3 files
with `id3v2` tags:
```liquidsoap
 # Log to stdout
 log.file.set(false)
 log.stdout.set(true)
 log.level.set(4)

 # Initial playlist
 cue = "/path/to/sheet.cue"

 # Create a reloadable playlist with this CUE sheet.
 # Tell liquidsoap to shutdown when we are done.
 x = playlist.reloadable(cue, on_done=shutdown)

 # We will never reload this playlist so we drop the first
 # returned value:
 s = snd(x)

 # Add a cue_cut to cue-in/cue-out according to
 # markers in "sheet.cue"
 s = cue_cut(s)

 # Shove all that to a output.file operator.
 output.file(%mp3(id3v2=true,bitrate=320),
             fallible=true,
             reopen_on_metadata=true,
             "/path/to/$(track) - $(title).mp3",
             s)
```
