# Regression test: ?? with two nullable values should not create double nullable type
a = {x=(null : string?)}
b = {x=(null : string?)}

# a.x is string?, b.x is string?
# result should be string?, not string??
result = a.x ?? b.x

# This should typecheck: string? is subtype of string?
ignore((result : string?))

test.pass()
