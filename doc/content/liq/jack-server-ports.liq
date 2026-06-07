#!/usr/bin/liquidsoap
# Route audio from the system capture ports through Liquidsoap and back
# out to the system playback ports, wiring everything programmatically.

s = input.jack(id="vocals")
o = output.jack(id="main", fallible=true, s)

# Connect system:capture_* → vocals JACK input ports.
# jack.server.capture() discovers the physical IsOutput capture ports
# and defers the connection until the input.jack client has woken up.
jack.server.capture().connect(s)

# Connect main JACK output ports → system:playback_*.
# jack.server.playback() discovers the physical IsInput playback ports
# and defers the connection until the output.jack client has woken up.
jack.server.playback().connect(o)
