music = playlist("~/Music")

now_playing = ref({title = "", artist = ""})

def remember(m) =
  now_playing := {title = m["title"], artist = m["artist"]}
end

music.on_metadata(synchronous=true, remember)

thread.run(
  every=60.,
  {
    print(
      "#{now_playing().title} - #{now_playing().artist}"
    )
  }
)
output.dummy(music)
