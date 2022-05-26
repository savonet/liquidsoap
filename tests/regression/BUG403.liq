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

r = ref(false)
def pred () =
  v = !r
  r := false
  v
end
thread.run(delay=2., { r := true })

mixer = fallback(id="mixer", track_sensitive=false,
  [source.available(sine(duration=3.), pred), blank()])

output.dummy(mixer)
thread.run(delay=3., { test.pass() ; shutdown()})
