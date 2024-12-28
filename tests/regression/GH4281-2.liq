d = sine()
d = metadata.map(update=false, (fun (_) -> [("title", "delay")]), d)

f = sine()
f = metadata.map(update=false, (fun (_) -> [("title", "fallback")]), f)

s = fallback([delay(initial=true, 60.0, d), f])

is_done = ref(false)

s.on_metadata(
  fun (m) ->
    begin
      if
        not is_done()
      then
        if
          m["title"] == "fallback"
        then
          begin
            is_done := true
            test.pass()
          end
        else
          test.fail()
        end
      end
    end
)

output.dummy(s)
