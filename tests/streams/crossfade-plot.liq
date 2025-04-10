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

s = cross.plot(dir=dir, s)

clock.assign_new(sync='none', [s])

base_dir = path.dirname(argv(0))

def on_stop() =
  test.equal(
    file.contents(path.concat(dir, "old.txt")),
    file.contents(path.concat(base_dir, "crossfade-plot.old.txt"))
  )
  test.equal(
    file.contents(path.concat(dir, "new.txt")),
    file.contents(path.concat(base_dir, "crossfade-plot.new.txt"))
  )
  test.pass()
end

output.dummy(fallible=true, on_stop=on_stop, s)
