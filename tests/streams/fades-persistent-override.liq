log.level.set(4)

a = sine(duration=5.,440.)
a = metadata.map(update=false, (fun (_) -> [("source", "a")]), a)

b = sine(duration=5.,880.)
b = metadata.map(update=false, (fun (_) -> [("source", "b"), ("liq_fade_out","1.1"), ("liq_fade_type","lin"), ("liq_fade_in","1.")]), b)

c = sine(duration=5.,440.)
c = metadata.map(update=false, (fun (_) -> [("source", "c")]), c)

s = sequence([a,b,c])

fade_in = fade.in(persist_overrides=true, duration=5., type="sin", s)
fade_out = fade.out(persist_overrides=true, duration=5., type="sin", fade_in)

track_count = ref(0)

def on_track(m) =
  ref.incr(track_count)

  if !track_count == 2 then
    if m["source"] != "b" then test.fail() end
    if fade_in.fade_duration() != 1. then test.fail() end
    if fade_out.fade_duration() != 1.1 then test.fail() end
    if fade_in.fade_type() != "lin" then test.fail() end
    if fade_out.fade_type() != "lin" then test.fail() end
  end

  if !track_count == 3 then
    if m["source"] != "c" then test.fail() end
    if fade_in.fade_duration() != 1. then test.fail() end
    if fade_out.fade_duration() != 1.1 then test.fail() end
    if fade_in.fade_type() != "lin" then test.fail() end
    if fade_out.fade_type() != "lin" then test.fail() end
    test.pass()
  end
end

fade_out.on_track(on_track)

clock.assign_new(sync="none",[fade_out])

output.dummy(fallible=true, fade_out)
