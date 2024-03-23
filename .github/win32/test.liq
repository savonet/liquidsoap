log.stdout.set(true)
log.file.set(false)

# Examples

# A playlist source with a smart crossfade
#s = smart_crossfade(playlist("mp3"))

# A HTTP input
s = input.http("http://wwoz-sc.streamguys1.com/wwoz-hi.mp3")

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

# An output to the local soundcard
output.ao(self_sync=false, fallible=true, s)
