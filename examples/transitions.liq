#
# An example of custom transition: inserting a jingle
#

# Simple settings for example scripts
set("log.stdout",true)
set("log.dir","/tmp")

auto = fallback([ playlist("/some/playlist.m3u"),
                  single("/local/file.ogg") ])

# A mount where live shows will connect in order to be relayed here
live = input.http("http://localhost:8000/test.live.ogg")

jingle = single("/some/jingle.ogg")

def tr(x,old,new)
  sequence([ x , new ])
end

output.icecast(%vorbis,name="test",
    fallback(track_sensitive=false,
      transitions=[ tr(jingle), tr(jingle) ],
      [ live, auto ]))
