d = sequence([sine(duration=3.), sine(duration=3.)])
d = metadata.map(insert_missing=true,update=false, (fun (_) -> [("title", "delay")]), d)

f = sequence([sine(duration=3.), sine(duration=3.)])
f = metadata.map(insert_missing=true,update=false, (fun (_) -> [("title", "fallback")]), f)

s = fallback([delay(initial=true, 1., d), f])

meta = ref([])

s.on_metadata(
  fun (m) ->
    begin
      meta := [...meta(), m["title"]]
      if
        meta() == ["fallback", "delay", "fallback", "delay"]
      then
        test.pass()
      end
    end
)

output.dummy(fallible=true, s)
