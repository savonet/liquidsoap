# Regression test for #3343.
#
# An inotify watch follows the inode, so watching the file itself stopped
# reporting as soon as a writer replaced it instead of writing in place --
# which is what the usual write-to-temporary-then-rename does.

uname_s =
  string.trim(
    process.run(
      "uname -s"
    ).stdout
  )

if uname_s == "Darwin" then test.skip() end

success = ref(false)
fname = file.temp("liq", "test")
tmp = "#{fname}.tmp"

on_cleanup({file.remove(fname)})

file.write(data="abc", fname)
file.watch(fname, {success := true})

def replace() =
  file.write(data="xxx", tmp)
  file.move(force=true, tmp, fname)
end

thread.run(delay=1., replace)

def check() =
  if success() then test.pass() else test.fail() end
end

thread.run(delay=3., check)

output.dummy(blank())
