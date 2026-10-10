s = single("video.mp4")

# BEGIN
let {video, audio} = source.tracks(s)
video = track.ffmpeg.side_data(rotation=180., video)
s = source({audio = audio, video = video})

# END
output.dummy(fallible=true, s)
