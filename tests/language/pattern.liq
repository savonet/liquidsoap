def c(x, y) =
  if x != y then
    print("#{x} != #{y}")
    test.fail()
  end
end

def f() =
  # Tuples
  let tuple = (123, "aabbcc", true, {foo=1})
  let (x, y, z, t) = tuple
  c(x, 123)
  c(y, "aabbcc")
  c(z, true)
  c(t, {foo=1})

  # List
  let l =  [1, 2, 3, 4, 5, 6, 7, 8, 9]
  let [x, y, ...z, t, u, v] = l
  c(x, 1)
  c(y, 2)
  c(z, [3, 4, 5, 6])
  c(t, 7)
  c(u, 8)
  c(v, 9)

  let [x,y] = [1,2,3]
  c(x, 1)
  c(y, 2)

  let [..._,x,y] = [1,2,3]
  c(x, 2)
  c(y, 3)

  let [...,x,y] = [1,2,3]
  c(x, 2)
  c(y, 3)

  let [x,_,...z] = [1,2,3]
  c(x, 1)
  c(z, [3])

  # Record
  record = {foo=123, gni="gno", bla=3.24}
  let {foo, gni, bla} = record
  c(foo, 123)
  c(gni, "gno")
  c(bla, 3.24)

  # Values with methods
  record = 123.{foo=123, gni="gno", bla=3.24}
  let v.{foo, gni, bla} = record
  c(v, 123)
  c(foo, 123)
  c(gni, "gno")
  c(bla, 3.24)

  record = 123.{foo=321, gni="gnu", bla=4.12}
  let _.{foo, gni, bla} = record
  c(foo, 321)
  c(gni, "gnu")
  c(bla, 4.12)

  # Combined patterns
  # Patterns inside records
  record' = {foo=tuple, gni=l, r=record}
  let {foo = (x,y,z,t), gni=[a,b,...s,d], r=v.{gni}} = record'
  c(foo, tuple)
  c(x, 123)
  c(y, "aabbcc")
  c(z, true)
  c(t, {foo=1})
  c(a, 1)
  c(b, 2)
  c(s, [3, 4, 5, 6, 7, 8])
  c(d, 9)
  c(r, record)
  c(v, 123)
  c(gni, "gnu")

  x = {foo=123, gni="gno", bla=3.24}
  y = {foo=123, gni="gnu", bla=3.24}
  z = {foo=123, gni="gno", bla=3.24}
  t = {foo=123, gni="gno", bla=4.24}
  l = [x, y, z, t]

  let [{foo}, {gni}, ..._, {bla}] = l
  c(foo, 123)
  c(gni, "gnu")
  c(bla, 4.24)

  let ({foo = [{foo}, {gni}, ..._, {bla}]}, z) = ({foo = l}, "the letter z")
  c(foo, 123)
  c(gni, "gnu")
  c(bla, 4.24)
  c(z, "the letter z")

  test.pass()
end

test.check(f)
