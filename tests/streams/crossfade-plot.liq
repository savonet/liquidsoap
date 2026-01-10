log.level := 5

a = sine(duration=20., 440.)
a =
  metadata.map(
    insert_missing=true,
    update=false,
    fun (_) ->
      [
        ("liq_cross_duration", "5."),
        ("liq_fade_out_type", "exp"),
        ("liq_fade_out_curve", "10"),
        ("liq_fade_out_delay", "2"),
        ("liq_fade_out", "3.")
      ],
    a
  )

b = sine(duration=20., 880.)
b =
  metadata.map(
    insert_missing=true,
    update=false,
    fun (_) ->
      [
        ("liq_fade_out_type", "lin"),
        ("liq_fade_in_delay", "2"),
        ("liq_fade_in", "2.")
      ],
    b
  )

b = once(b)

s = sequence([a, b])

dir = file.temp_dir("plot")
on_cleanup({file.rmdir(dir)})

s = cross.plot(dir=dir, png="crossfade-plot.png.gen", s)

clock.assign_new(sync='none', [s])

base_dir = path.dirname(argv(0))

def on_stop() =
  file.copy(
    path.concat(dir, "old.txt"),
    path.concat(base_dir, "crossfade-plot.old.txt.gen")
  )
  file.copy(
    path.concat(dir, "new.txt"),
    path.concat(base_dir, "crossfade-plot.new.txt.gen")
  )
  test.pass()
end

o = output.dummy(fallible=true, s)
o.on_stop(synchronous=true, on_stop)
