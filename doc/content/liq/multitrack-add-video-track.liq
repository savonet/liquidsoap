# A playlist of audio files
s = playlist("a_playlist")

# A static image
image = single("/path/to/image.png")

# Get the playlist's audio track, metadata and track marks
let {audio = playlist_audio, metadata, track_marks} = source.tracks(s)

# Get the video track from our static image
let {video = image_video} = source.tracks(image)

# Mux the audio tracks with the image
s =
  source(
    {
      audio=playlist_audio,
      video=image_video,
      metadata=metadata,
      track_marks=track_marks
    }
  )

# END
output.dummy(fallible=true, s)
