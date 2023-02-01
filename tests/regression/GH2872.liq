def f() =
  track_count = ref(0)
  s = input.external.rawaudio("ffmpeg -re -f lavfi -i sine=frequency=1000 -t 1 -ac 2 -f s16le -", restart=true, buffer=1.)
  s.on_track(fun (_) -> begin
    track_count := !track_count + 1
    if !track_count > 3 then test.pass() end
  end)
  output.dummy(fallible=true,s)
end

test.check(f)
