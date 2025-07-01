BEET = "/usr/bin/beet"

# BEGIN
def beets(id, query) =
  beets_src =
    request.dynamic(
      id=id,
      retry_delay=1.,
      {
        request.create(
          string.trim(
            process.read(
              "#{BEET} random -f '$path' #{query}"
            )
          )
        )
      }
    )
  (beets_src : source)
end

all_music = beets("all_music", "")
recent_music = beets("recent_music", "added:-1m..")
rock_music = beets("rock_music", "genre:Rock")

# END
output.dummy(fallible=true, all_music)
output.dummy(fallible=true, recent_music)
output.dummy(fallible=true, rock_music)
