server = icecast.server({source_password = "change-me"})

radio = mksafe(playlist("/srv/radio/music"))
encoded =
  ffmpeg.encode.audio(
    %ffmpeg(%audio(codec = "libmp3lame", sample_format = "fltp")),
    radio
  )

server.attach_source(
  format=%ffmpeg(format = "mp3", %audio.copy),
  headers=[
    (
      "ice-name",
      "My radio"
    )
  ],
  "/radio.mp3",
  encoded
)
