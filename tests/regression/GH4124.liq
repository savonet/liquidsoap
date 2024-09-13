s = sine()
s = filter.iir.butterworth.high(s, frequency=250., order=2)
output.dummy(on_start=test.pass, s)
