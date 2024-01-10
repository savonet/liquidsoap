def f() =
  n = ref(1)
  while n() < 10 do n := n() * 2 end
  test.equals(n(), 16)
  n = ref(0)
  for i = 0 to 10 do n := n() + i end
  test.equals(n(), 55)
  n = ref(0)
  for i = list.iterator([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) do n := n() + i end
  test.equals(n(), 55)
  s = ref("")
  for i = list.iterator(["a", "b", "c"]) do s := s() ^ i end
  test.equals(s(), "abc")

  test.pass()
end

test.check(f)
