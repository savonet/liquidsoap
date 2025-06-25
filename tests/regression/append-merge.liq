music = chop(every=1., metadata=[("source", "s1")], sine(amplitude=0.1, 440.))

def next(_) =
  s = sine(amplitude=0.1, duration=.5, 880.)
  metadata.map(
    insert_missing=true, fun (_) -> [("source", "s2")], s
  )
end

s = append(merge=true, music, next)

count_s1 = ref(0)
count_s2 = ref(0)

s.on_metadata(
  fast=true,
  fun (m) ->
    begin
      s = m["source"]
      if
        s == "s1"
      then
        ref.incr(count_s1)
      elsif s == "s2" then ref.incr(count_s2)
      end

      if count_s1() > 2 and count_s2() > 2 then test.pass() end
    end
)

s.on_track(fun (m) -> if m["source"] == "s2" then test.fail() end)

output.dummy(s)
