# The live source. We use a short buffer to switch more quickly to the source
# when reconnecting.
live_source = input.harbor("mount-point-name", buffer=3.)

# A playlist source.
playlist_source = playlist("/path/to/playlist")

# Set to `true` when we should be adding silence.
should_append = ref(false)

# Append 5. of silence when needed.
fallback_source =
  append(
    playlist_source,
    fun (_) ->
      if
        should_append()
      then
        should_append := false
        blank(duration=5.)
      else
        source.fail()
      end
  )

# When leaving the playlist, prepare the silence insertion immediately so it
# is ready when we return from live.
fallback_source =
  fallback_source.{
    on_leave =
      fun (_) ->
        begin
          should_append := true
          fallback_source.skip()
        end
  }

radio = fallback([live_source, fallback_source])

# END
output.dummy(fallible=true, radio)
