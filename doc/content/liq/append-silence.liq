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

# Transition to live
def to_live(playlist, live) =
  sequence([playlist, live])
end

# Transition back to playlist
def to_playlist(live, playlist) =
  # Ask to insert a silent track.
  should_append := true

  # Cancel current track. This will also set the playlist to play a new
  # track. If needed, `cancel_pending` can be used to for a new silent track
  # without skipping the playlist current track.
  fallback_source.skip()

  sequence([live, playlist])
end

radio =
  fallback(
    track_sensitive=false,
    transitions=[to_live, to_playlist],
    [live_source, fallback_source]
  )

# END
output.dummy(fallible=true, radio)
