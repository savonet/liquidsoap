#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

output.dummy(input.rtmp("rtmp://127.0.0.1:53163/share"), fallible=true)

thread.run(test.pass)
