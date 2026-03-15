s = sine()
a = ref([])

def test1() =
  o = output.dummy(fallible=true, s)
  a := [("test", o)]
end

def test2() =
  if
    list.assoc.mem("test", a())
  then
    o = list.assoc("test", a())
    o.shutdown()
    a := list.assoc.remove("test", a())
    test.equal(list.length(a()), 0)
    test.pass()
  end
end

thread.run(test1)
thread.run(delay=1., test2)
