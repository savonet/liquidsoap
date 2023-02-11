# A simple video script

The other day, I wanted to prepare some videos of my favorite reggae and soul
tunes for uploading them to YouTube.
My goal was very simple: prepare a video with the music,
and a static image.

After briefly digging for a simple software to do that,
which I could not find, I said ``hey, why not doing it with liquidsoap''?
Well, that is fairly easy!

Here is the code:

```liquidsoap
log.level := 4

audio = once(single("/tmp/bla.mp3"))
video = single("/tmp/bla.jpg")

# Mux audio and video
source = mux_video(video=video,audio)

# Disable real-time processing, to process with the maximum speed
clock.assign_new(sync='none',[source])

# Encode video and copy audio:
encoder = %ffmpeg(format="mp4",
                  %audio.copy,
                  %video(codec="libx264"))

 # Output to a theora file, shutdown on stop
 output.file(fallible=true,on_stop=shutdown,
             encoder, "/tmp/encoded-video.mp4",
             source)
```
