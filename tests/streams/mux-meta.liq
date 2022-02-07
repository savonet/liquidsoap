#!../../src/liquidsoap ../../libs/stdlib.liq

# Ensure that muxing video preserves metadata

%include "test.liq"

log.level.set(4)

s = sine()
s = insert_metadata(s)
thread.run(delay=1., {s.insert_metadata([("a","b")])})
s = mux_video(video=blank(), s)
s = source.on_metadata(s, fun(_) -> begin test.pass(); shutdown() end)
output.dummy(s)
