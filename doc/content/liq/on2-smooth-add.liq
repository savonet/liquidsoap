s = sine()

# BEGIN
# A special source
special = request.queue(id="special")
# Smooth_add the special source
s = smooth_add(normal=s, special=special)
# END

output.dummy(fallible=true, s)
