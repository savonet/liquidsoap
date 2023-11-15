night=sine()
day=sine()
# BEGIN
# A scheduler, assuming you have defined the night and day sources
s = switch([ ({0h-7h}, night), ({7h-24h}, day) ])
# END
output.dummy(fallible=true,s)
