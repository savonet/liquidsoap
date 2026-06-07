#!/usr/bin/liquidsoap
# Send a sine wave to JACK and loop it back through a second JACK input,
# wiring the ports programmatically instead of using an external patchbay.

sine = sine(440.)

out = output.jack(id="out", sine)
re_in = input.jack(id="re-in")

# Wire out's output ports to re-in's input ports.
# The connection is deferred until both JACK clients have registered
# their ports (i.e. both operators have woken up).
out.connect(re_in)
