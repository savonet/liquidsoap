#!../../liquidsoap ../test.liq

success = ref(true)

def f() =
  mu = runtime.mem_usage()
  print("Memory usage:\n#{mu}")

  # See #2427
  thread.run({ignore(runtime.mem_usage())})

  test.pass()
end

test.check(f)
