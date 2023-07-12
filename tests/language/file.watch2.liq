success = ref(false)

fname = file.temp("liq","test")

def cleanup() =
  file.remove(fname)
end

on_cleanup(cleanup)

file.write(data="abc", fname)
file.watch(fname, {success := true})

def write() =
  file.write(data="xxx", fname)
end

thread.run(delay=1., write)

def check() =
  if success() then test.pass() else test.fail() end
end

thread.run(delay=3., check)

output.dummy(blank())
