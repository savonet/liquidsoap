s = sine()
output.dummy(s)

output_list = ref([])

def check() =
  runtime.gc.compact()
  runtime.gc.compact()
  runtime.gc.compact()
  if
    list.length(clock(s.clock).sources()) == 2
  then
    test.pass()
  else
    test.fail()
  end
end

def cleanup() =
  l = output_list()
  output_list := []
  list.iter(fun (o) -> o.shutdown(), l)
  thread.run(delay=2., check)
end

def create() =
  for i = 0 to
    49
  do
    s = switch([({true}, sine()), ({true}, s)])
    o = output.dummy(s)
    output_list := [o, ...!output_list]
  end
  thread.run(delay=2., cleanup)
end

thread.run(delay=0., create)
