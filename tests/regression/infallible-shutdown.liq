#!../../liquidsoap

%include "../test.liq"

# Make sure infallible output can shutdown properly.
output.dummy(blank())
thread.run(delay=1., test.pass)
