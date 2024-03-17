#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Make sure that we properly shutdown on init error
log.level.set(5)

o = output.dummy(blank())
o.on_shutdown(test.pass)

o.on_wake_up({output.dummy(fallible=true, source.fail.init())})
