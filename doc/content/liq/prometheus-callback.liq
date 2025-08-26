# A0
is_playing_metric =
  prometheus.gauge(
    labels=["source"],
    help="Whether source is playing.",
    "liquidsoap_is_playing"
  )

# A1
# B0
playlist = playlist(id="playlist", "my-playlist")
set_playlist_is_playing = is_playing_metric(label_values=["radio"])

# B1
# C0
def check_if_ready(set_is_ready, s) =
  def callback() =
    if source.is_ready(s) then set_is_ready(1.) else set_is_ready(0.) end
  end
  callback
end

thread.run(every=1., check_if_ready(set_playlist_is_playing, playlist))
