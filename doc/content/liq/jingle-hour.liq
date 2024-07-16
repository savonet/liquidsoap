normal = sine()
jingle = sine()

# BEGIN
s = add([normal, switch([({0m}, jingle)])])
# END

output.dummy(fallible=true, s)
