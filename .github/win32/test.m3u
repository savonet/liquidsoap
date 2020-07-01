set("log.stdout",true)
set("log.file",false)

# Examples

# A playlist source with a smart crossfade
s = smart_crossfade(playlist("mp3"))

# A HTTP input
#s = input.http("http://flows.liquidsoap.fm/radio/01dbfc6d2f3e50d6ba243e68c1ca469b44babd1f/mp3/128k")

# An output to the local soundcard
output.ao(fallible=true,s)

# An output to a local file, encoding
# in ogg/vorbis+ogg/theora
#output.file(%ogg(%theora,%vorbis),
#            fallible=true,
#            "z:\tmp\output.ogv",
#            s)

# An icecast output in AAC+ format
#output.icecast(%aacplus(bitrate=32),
#              fallible=true,
#              mount="test",
#              s)
