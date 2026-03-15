# Testing invalid record members. The default error was
# At line 4, char 4:
# Error 5: this value has type
#   unit (inferred at line 3, char 4-6)
# but it should be a subtype of the type of the value at line 4, char 4-10
#   {{ _ | f = _ }}
# It should be replaced with something more readable.

r = ()
_ = r.f(3)
