# An ffmpeg encoder's tracks are named by convention, which needs no libav.
e = %ffmpeg(format="mp4", %audio(codec="aac", channel_layout="5.1"), %video.copy)
n = %ffmpeg(format="mp4", %audio(channel_layout=5.1))
ignore((e, n))

#? type 2:4
#? type 3:4
