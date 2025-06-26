music = chop(every=1., metadata=[("source", "s1")], sine(amplitude=0.1, 440.))

def next(_) =
  sine(amplitude=0.1, duration=.5, 880.)
end

s = append(music, next)

count_s1 = ref(0)
count_s2 = ref(0)

s.on_track(
synchronous=true,
  fun (m) ->
    begin
      s = m["source"]
      if
        s == "s1"
      then
        ref.incr(count_s1)
      else
        test.equal(m["source"], "")
        ref.incr(count_s2)
      end

      if count_s1() > 2 and count_s2() > 2 then test.pass() end
    end
)

output.dummy(s)
