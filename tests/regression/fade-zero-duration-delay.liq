# A zero-duration fade with a pending delay must not divide by zero.
s = sine()

# Delay pending: returns start.
f = mkfade(start=1., stop=0., delay=1., duration=0., s)
if f() != 1. then test.fail() end

# No delay, zero duration: returns stop on the first frame (#4887).
f = mkfade(start=0., stop=1., delay=0., duration=0., s)
if f() != 1. then test.fail() end

# Fade start with a positive duration still returns start.
f = mkfade(start=1., stop=0., delay=0., duration=3., s)
if f() != 1. then test.fail() end

test.pass()
