music = playlist("/path/to/music")

# BEGIN
live =
  blank.strip(
    max_blank=10.,
    threshold=-50.,
    input.harbor("live", port=8000, password="hackme")
  )

interlude = single("/path/to/interlude.mp3")

def fade_in_on_select({starting, ..._}) =
  fade.in(starting)
end

radio =
  fallback(
    [
      live.{on_select = fade_in_on_select},
      music.{on_select = fade_in_on_select},
      interlude
    ]
  )

# END
output.dummy(radio)
