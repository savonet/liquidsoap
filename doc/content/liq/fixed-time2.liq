jingles = sine()
playlist = sine()

# BEGIN
radio = switch([(predicate.due({0m-5m}, jingles), jingles), ({true}, playlist)])

# END
output.dummy(fallible=true, radio)
