music = playlist("~/Music")

def print_title(m) =
  log(
    "Now playing: #{m["title"]}"
  )
end

announce = music.on_metadata(synchronous=true, print_title)

# Takes the callback back off `music`.
announce.release()
