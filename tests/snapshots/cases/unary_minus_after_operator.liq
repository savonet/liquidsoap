# A "-" right after an operator is a sign, whatever came before the operator.
b = true
y = 2
n = null
a = b ? -y : 0
c = (b) ? -y : 0
d = n ?? -y
e = fun (z) -> -z
f = b ? -1 : -2
