test.skip()

def f() =
  has_source = ref(false)
  last_connect = ref(false)
  error_seen = ref(false)

  shutdown = ref(fun () -> ())

  def on_start() =
    if
      last_connect()
    then
      test.pass()
    else
      print(
        "Shutting down initial source"
      )
      fn = shutdown()
      fn()
    end
  end

  def on_error(_) =
    if
      not error_seen()
    then
      error_seen := true
    elsif
      not has_source()
    then
      s = input.harbor(port=8005, password="hackme", "test")
      shutdown := s.shutdown
      output.dummy(fallible=true, s)
      has_source := true
    else
      last_connect := true
      error_seen := false
      has_source := false
    end
  end

  s = sine()
  o = output.url(
    on_error=on_error,
    restart_delay=0.5,
    url="icecast://source:hackme@localhost:8005/test",
    %ffmpeg(
      format = "ogg",
      content_type = "application/ogg",
      %audio(codec = "flac")
    ),
    s
  )
  o.on_start(synchronous=true, on_start)
end

test.check(f)
