last_source = sine()

last_source = metadata.map(insert_missing=true, fun (_) -> [("last","true")], last_source)

s = sequence([sine(duration=5.), blank(duration=5.), last_source])

s = blank.eat(max_blank=1., s)

s = source.on_metadata(s, fun (m) -> if m["last"] == "true" then test.pass() end)

clock.assign_new(sync="none",[s])

output.dummy(fallible=true, s)
