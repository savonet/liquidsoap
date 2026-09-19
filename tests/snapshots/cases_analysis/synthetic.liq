let (a, b) = (1, "x")
let {c, d} = {c = 1, d = 2}
s = "v=#{a}"
for i = 1 to 3 do
  print(i)
  print(a)

end
y = a + c

#? locals 9:4
#? type 4:1
#? type 3:5
#? type 9:6
