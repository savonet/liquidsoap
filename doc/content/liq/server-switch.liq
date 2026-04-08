#!./liquidsoap
settings.server.telnet := true

s1 = playlist("~/Music")
s2 = sine()

b = interactive.bool("button", true)

s = switch([(b, s1), ({true}, s2)])

output.pulseaudio(s)
