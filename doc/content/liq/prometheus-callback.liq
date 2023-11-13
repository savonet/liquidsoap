is_playing_metric = prometheus.gauge(labels=["source"], "liquidsoap_is_playing")
playlist = playlist(id="playlist", "my-playlist")
set_playlist_is_playing = is_playing_metric(label_values=["radio"])
def check_if_ready(set_is_ready, s) =
  def callback() =
    if source.is_ready(s) then set_is_ready(1.) else set_is_ready(0.) end
    0.1
  end
  callback
end

thread.run.recurrent(
  delay=0., check_if_ready(set_playlist_is_playing, playlist)
)
