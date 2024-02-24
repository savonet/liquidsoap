#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Make sure that we properly shutdown on init error
log.level.set(5)
settings.clock.allow_streaming_errors.set(true)

s = source.fail.init()

s.on_shutdown(test.pass)

s = sequence([blank(duration=0.1), blank(duration=0.1), s])

output.dummy(fallible=true, s)
