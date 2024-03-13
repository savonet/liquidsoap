def images =
  video.fade.in(
    duration=1.,
    video.fade.out(
      duration=1.,
      video.add_text(
        metadata="filename",
        "<no filename>",
        size=12,
        playlist(prefix="annotate:duration=3:", argv(1))
      )
    )
  )
end

def sound =
  playlist(argv(2))
end

# You can also combine the two in a Theora file.  Play the result in VLC rather
# than mplayer (or even liquidsoap) as it is a sequentialized ogg stream.
# output.file(%ogg(%vorbis,%theora),"slideshow.ogg", mksafe(source.mux.video(video=images,sound)))

output.ao(fallible=true, sound)
output.sdl(fallible=true, images)
