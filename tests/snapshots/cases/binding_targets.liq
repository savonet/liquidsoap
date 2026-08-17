# A bare binding takes the same targets as `let`: a variable, a field path, a
# type annotation, and destructuring patterns. The left-hand side is parsed as
# an expression and reinterpreted as a pattern.
x = {a = 1}
x.b = 2

(p, q) = (1, 2)
[r, s] = [3, 4]
(n : int) = 5
_ = 6

(x.b, p, q, r, s, n)
