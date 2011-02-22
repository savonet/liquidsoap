# This is the test for bug #403 from our old trac.
#
# Make a switch() declare itself ready and arrange to use it for the next
# frame where it isn't ready anymore.
#
# Two switches A and B
# A is only ready for a short period of time due to its predicates.
# B reselects at the end of a frame just before A becomes unavailable
# as a result, B has selected = A, attempts to stream it
# but A finds itself not ready anymore.
# In other words, B committed but A did not.

r = ref false
pred = { v=!r ; r:=false ; v }
add_timeout(2.,{ r := true ; (-1.) })

mixer = fallback(id="mixer", track_sensitive=false,
  [at(pred, sine(duration=3.)), blank()])

output.dummy(mixer)
add_timeout(3.,{ print("TEST PASSED") ; shutdown() ; (-1.) })
