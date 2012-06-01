#
# A more complex example,
# showing what the historical Geek Radio of ENS Lyon used to be
#

# User requests can be pushed in a queue(),
# we add speech-synthetized metadata information at the end of tracks.
req = say_metadata(request.queue())

# This needs to be infallible, so the following must be a valid audio file.
on_fail   = single("/path/to/some.ogg")

# Then a few playlists
default   = playlist(reload=3600, "/home/dbaelde/playlist.local")
techno    = playlist(reload=3600, "/home/dbaelde/playlist.techno")
classique = playlist(reload=3600, "/etc/liquidsoap/playlist.classique")
smb       = playlist(reload=3600, "smb://footwar/mp3/playlist")

# Our clock jingles
clock     = playlist(mode="random",reload=3600,
                     "/usr/local/share/liquidsoap/jingles/list")

# First, define the source, then we'll add the outputs
scheduler =
  add(weights=[1,3],
      [fallback([ req,
                  switch([ ({ 0h    }, techno),
                           ({ 1h-4h }, classique) ]),
                  smb,
                  default,
                  on_fail ]),
       switch([({ 0m and 0s-5s },delay(5.,clock))])])

# On top of the automatic programming, add some live shows possibilities
def fullscheduler
  emission = input.http("http://sci4:8000/emission.ogg")
  live = amplify(1.2,input.http("http://sparc8:8000/live.ogg"))
  fallback(track_sensitive=false,
           [ emission,
             add(weights=[1,5],[ scheduler,live ]) ])
end

# Partial application of icecast output to specialize it for a given host
out = output.icecast(host="sci4")
# Now we add the outputs
out(mount="geek.pure.ogg",%vorbis,scheduler)
out(mount="geek.modem.ogg",%vorbis(quality=1.,samplerate=22050),fullscheduler)
out(mount="geek.ogg",%vorbis,fullscheduler)
