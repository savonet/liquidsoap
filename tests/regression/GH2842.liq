log.level := 5

def f() =
  was_called = ref(false)
  filename = file.temp("bla", "blo")
  on_cleanup({file.remove(filename)})

  def filename()
    if was_called() then
      test.fail()
    end
    was_called := true
    "bla"
  end

  output.file(
    fallible=true,
    on_stop=test.pass,
    %ffmpeg(format="mp3",
      %audio(codec="libmp3lame", samplerate=48000, b="320k")
    ),
    filename,
    once(sine(duration=1.,480.))
  )
end

test.check(f)
