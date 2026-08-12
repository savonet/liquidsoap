# A source of unknown content cannot be used as a source of any given content:
# nothing is known about what it streams. The other direction is fine and is
# covered by tests/language/abstract_source.liq.
#
# Expected:
# Error 5: this value has type
#   source(_) (inferred at line 11, char 4-24)
# but it should be a subtype of
#   source('A)

s = (sine() : source(_))
output.dummy(s)
