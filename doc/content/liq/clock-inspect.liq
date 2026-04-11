s = sine()

# BEGIN
s = source.methods(s)
c = clock(s.clock)
print(
  "source #{s.id()} belongs to clock: #{c.id()}"
)

# END

output.dummy(s)
