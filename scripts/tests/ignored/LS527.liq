# In LS-527, an exception was raised while initialization
# the sources. Because the clock was delegated to one of those
# sources and only the failed source was properly terminated, 
# liquidsoap refused to shutdown..

# By the way, this one is a more complicated instance of LS503..

# Reopen stderr to /dev/null to
# disable printing expected exception
reopen.stderr("/dev/null")

on_shutdown({ print("TEST PASSED") })

s = input.alsa()

output.icecast(%wav, host="nonexistant", 
               mount="foo", on_stop=shutdown,
               s)

