# An ffmpeg encoder's tracks are named by convention, which needs no libav.
e = %ffmpeg(format="mp4", %audio(codec="aac", channel_layout="5.1"), %video.copy)
ignore(e)

#? type 2:4
