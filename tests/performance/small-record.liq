#!../../liquidsoap ../test.liq performance.liq

def r2()
  r = {a0 = 0, a1 = 1}
  x = ref(0)
  for _ = 1 to 200000 do
    x := x() + r.a0 + r.a1
  end
end

def r5()
  r = {a0 = 0, a1 = 1, a2 = 2, a3 = 3, a4 = 4}
  x = ref(0)
  for _ = 1 to 200000 do
    x := x() + r.a0 + r.a1 + r.a2 + r.a3 + r.a4
  end
end

def r10()
  r = {a0 = 0, a1 = 1, a2 = 2, a3 = 3, a4 = 4, a5 = 5, a6 = 6, a7 = 7, a8 = 8, a9 = 9}
  x = ref(0)
  for _ = 1 to 200000 do
    x := x() + r.a0 + r.a1 + r.a2 + r.a3 + r.a4 + r.a5 + r.a6 + r.a7 + r.a8 + r.a9
  end
end

def r20()
  r = {a00 = 0, a01 = 1, a02 = 2, a03 = 3, a04 = 4, a05 = 5, a06 = 6, a07 = 7, a08 = 8, a09 = 9,
       a10 = 0, a11 = 1, a12 = 2, a13 = 3, a14 = 4, a15 = 5, a16 = 6, a17 = 7, a18 = 8, a19 = 9}
  x = ref(0)
  for _ = 1 to 200000 do
    x := x()
         + r.a00 + r.a01 + r.a02 + r.a03 + r.a04 + r.a05 + r.a06 + r.a07 + r.a08 + r.a09
         + r.a10 + r.a11 + r.a12 + r.a13 + r.a14 + r.a15 + r.a16 + r.a17 + r.a18 + r.a19
  end
end

time("record with  2 fields", r2)
time("record with  5 fields", r5)
time("record with 10 fields", r10)
time("record with 20 fields", r20)

exit(0)
