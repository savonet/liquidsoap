#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

set("log.level", 4)

# s = video.external.testsrc()
s = single("../../src/test.mp4")
source.set.video.size(500,100,s)
# s = mksafe(s)

output.graphics(fallible=true, s)

# thread.run(delay=1., {test.pass(); shutdown()})
