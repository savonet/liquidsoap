#!./liquidsoap
source = sine()
# Register a telnet variable named volume with 1 as initial value
v = interactive.float("volume", 1.)

# Change the volume accordingly
source = amplify(v, source)
ignore(source)
