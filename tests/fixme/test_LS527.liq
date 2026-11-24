# In LS-527, an exception was raised while initializing
# the sources. Because the clock was delegated to one of those
# sources and only the failed source was properly terminated,
# liquidsoap refused to shutdown..

# By the way, this one is a more complicated instance of LS503..

%include "test.liq"

skipped = true

%ifdef input.alsa
# Reopen stderr to /dev/null to
# disable printing expected exception
reopen.stderr("/dev/null")

skipped = false

on_shutdown(test.pass)

s = input.alsa()

o = output.icecast(%wav, host="nonexistent", mount="foo", s)
o.on_stop(synchronous=true, shutdown)
%endif

if skipped then test.skip() end
