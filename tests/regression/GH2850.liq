
def f() =
  has_connected = ref(false)

  def on_start() =
    has_connected := true
  end

  error_count = ref(0)

  def on_error(_) =
    error_count := error_count() + 1
  end

  s = sine()

  output.url(
    on_error=on_error,
    on_start=on_start,
    restart_delay=0.5,
    url="icecast://source:hackme@localhost:8005/test",
    %ffmpeg(
      format="ogg",
      content_type="application/ogg",
      %audio(codec="flac")),
    s
  )

  thread.run(delay=1., {begin
    if has_connected() or error_count() == 0 then
      test.fail()
    end

    s = input.harbor(port=8005,password="hackme","test")

    thread.run(delay=1., {begin
      if not !has_connected then
        test.fail()
      end

      print("Shutting down initial source")
      s.shutdown()
      error_count := 0

      thread.run(delay=3., {begin
        if error_count() == 0 then
          test.fail()
        end

        has_connected := false
        print("Starting new source")
        input.harbor(port=8005,password="hackme","test")

        thread.run(delay=1., {begin
          if not has_connected() then
            test.fail()
          end

          test.pass()
        end})
      end})
    end})
  end})
end

test.check(f)
