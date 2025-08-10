stream = input.http(start=false, "https://wwoz-sc.streamguys1.com/wwoz-hi.mp3")

listeners_count = ref(0)

def on_connect(_) =
  listeners_count := listeners_count() + 1
  if
    listeners_count() > 0 and not stream.is_started()
  then
    log(
      "Starting input"
    )
    stream.start()
  end
end

def on_disconnect(_) =
  listeners_count := listeners_count() - 1
  if
    listeners_count() == 0 and stream.is_started()
  then
    log(
      "Stopping input"
    )
    stream.stop()
  end
end

blank = single("/tmp/blank.mp3")

stream = fallback(track_sensitive=false, [stream, blank])

o =
  output.harbor(
    %ffmpeg(format = "mp3", %audio.copy),
    format="audio/mpeg",
    mount="relay",
    stream
  )

o.on_connect(synchronous=true, on_connect)
o.on_disconnect(synchronous=true, on_disconnect)
