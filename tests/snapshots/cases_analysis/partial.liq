# Each type error is replaced with a placeholder, so the code around it is
# still typed and every error is reported.
x = 1 + "a"
s = sine()
y = undefined_name
r = {alpha = 1, beta = "b"}
z = r.alpha + y
print((x, s, z))

#? type 3:0
#? type 5:0
#? type 7:0
#? type 7:4
#? methods 7:5
#? locals 8:0
#? scope 8:0
