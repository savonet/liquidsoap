# Failing to open the file leaves the output in its `Idle` state, so the delay
# returned by `reopen_on_error` used to be ignored and the output retried on
# every streaming cycle.

dir = file.temp_dir("liq", "reopen")

# A regular file where the output expects a directory: creating the output's
# parent directory then always fails.
blocker = path.concat(dir, "blocker")
file.write(data="", blocker)
target = path.concat(blocker, "out.wav")

attempts = ref(0)

def on_error(_) =
  ref.incr(attempts)
  1.
end

output.file(%wav, target, sine(), fallible=true, reopen_on_error=on_error)

def check() =
  # One immediate attempt plus roughly one per second: a handful, not one per
  # streaming cycle.
  if
    attempts() == 0
  then
    print(
      "reopen_on_error was never called"
    )
    test.fail()
  elsif
    attempts() > 10
  then
    print(
      "reopen_on_error was called #{attempts()} times in 3s, delay ignored"
    )
    test.fail()
  else
    test.pass()
  end
end

thread.run(delay=3., check)
