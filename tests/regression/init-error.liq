#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Make sure that we properly shutdown on init error

%include "test.liq"

set("log.level",5)

set("clock.allow_streaming_errors", true)

s = source.fail.init()

s.on_shutdown(test.pass)

s = fallback([blank(), blank(), s])

output.dummy(fallible=true, s)
