s = sine()
s = filter.iir.butterworth.high(s, frequency=250., order=2)
o = output.dummy(s)
o.on_start(synchronous=true, test.pass)
