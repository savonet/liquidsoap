#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Make sure that we properly shutdown on init error
log.level.set(5)
s = source.fail.init()
s = fallback([blank(), blank(), s])
o = output.dummy(fallible=true, s)
o.on_shutdown(test.pass)
