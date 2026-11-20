%include "../test.liq"

# runtime.file_descriptors counts open descriptors and reads their limit (#3953).

def f() =
  fds = runtime.file_descriptors()
  print(
    "count: #{fds.count}, max: #{fds.max}"
  )
  if
    os.type == "Win32"
  then
    test.pass()
  else
    n = null.get(fds.count)

    # stdin, stdout and stderr at least.
    if n >= 3 and (fds.max ?? n) >= n then test.pass() else test.fail() end
  end
end

test.check(f)
