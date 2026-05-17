source = sine()

# BEGIN
# General-purpose music stream
output.icecast(%opus(bitrate=128), host="myserver", mount="stream.opus", source)

# Low-bitrate voice stream (use mean() to downmix stereo to mono)
output.icecast(%opus(bitrate=24, application="voip", signal="voice", channels=1), host="myserver", mount="stream-voice.opus", mean(source))

# Low-latency real-time audio
output.icecast(%opus(frame_size=10., application="restricted_lowdelay"), host="myserver", mount="stream-ll.opus", source)
# END
