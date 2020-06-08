#!../src/liquidsoap ../libs/pervasives.liq --force-start

def time(name, f)
  print("Computing #{name}...")
  t = gettimeofday()
  f()
  t' = gettimeofday()
  print("Computing #{name} took #{t'-t} seconds.")
end

def erathostenes(n)
  l = list.init(n-2, fun (i) -> i+2)
  l = ref(l)
  p = ref([])
  while({not list.is_empty(!l)}, fun () -> begin
    i = list.hd(default=0, !l)
    p := list.add(i, !p)
    l := list.filter(fun (j) -> j mod i != 0, !l)
  end)
  list.rev(!p)
end

time("Erathostenes (imperative)", {erathostenes(10000)})

def erathostenes(n)
  def rec aux(p, l)
    list.case(l, p, fun (i, l) -> aux(list.add(i, p), list.filter(fun (j) -> j mod i != 0, l)))
  end
  l = list.init(n-2, fun (i) -> i+2)
  list.rev(aux([], l))
end

time("Erathostenes (recursive)", {erathostenes(10000)})

exit(0)
