# `x = e`, `let x = e` and `def x = e end` are three grammar productions
# (`binding` / `explicit_binding` in parser.mly) that reduce to the same `let`.
c = 1
let d = 1
def e = 1 end
(c, d, e)
