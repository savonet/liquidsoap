music = playlist("~/Music")

announce =
  music.on_metadata(
    synchronous=true,
    fun (m) ->
      log(
        "Now playing: #{m["title"]}"
      )
  )

# Takes the callback back off `music`.
announce.release()
