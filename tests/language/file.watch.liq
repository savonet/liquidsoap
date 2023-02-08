uname_s = string.trim(process.run("uname -s").stdout)

if uname_s == "Darwin" then
  test.skip()
end

success = ref(false)

def f() =
  fname = "/tmp/fw"
  file.write(data="abc", fname)
  file.watch(fname, {success := true})
  file.write(data="xxx", fname)
  sleep(1.)

  if success() then test.pass() else test.fail() end
end

test.check(f)
