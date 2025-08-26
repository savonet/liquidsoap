log.level := 4

audio = once(single("/tmp/bla.mp3"))
video = single("/tmp/bla.jpg")

# Mux audio and video
source = source.mux.video(video=video, audio)

# Disable real-time processing, to process with the maximum speed
clock.assign_new(sync='none', [source])

# Encode video and copy audio:
encoder = %ffmpeg(format = "mp4", %audio.copy, %video(codec = "libx264"))

# Output to a theora file, shutdown on stop
o = output.file(fallible=true, encoder, "/tmp/encoded-video.mp4", source)

o.on_stop(synchronous=true, shutdown)
