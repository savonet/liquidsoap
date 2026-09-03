music = playlist("~/Music")

current_title = ref("")
current_artist = ref("")

def remember(m) =
  # Both writes land before any other atomic section runs.
  atomic(
    {
      current_title := m["title"]
      current_artist := m["artist"]
    }
  )
end

music.on_metadata(synchronous=true, remember)

# The two reads are grouped too. Read one at a time outside a section, and
# a title may be paired with the previous track's artist.
def now_playing() =
  atomic(
    {
      "#{current_title()} - #{current_artist()}"
    }
  )
end

thread.run(every=60., {print(now_playing())})
output.dummy(music)
