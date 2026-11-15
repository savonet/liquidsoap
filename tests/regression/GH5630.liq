# Commands cancelled by `process.run`'s timeout must be terminated, not left
# running in the background.
def pid_file() =
  f = file.temp("liq-gh5630", ".pid")
  file.remove(f)
  f
end

def is_running(f) =
  pid = string.trim(file.contents(f))
  process.run(
    "kill -0 #{pid}"
  ).status.code ==
    0
end

def check_terminated(~timeout_status, ~delay, f, cont) =
  if timeout_status != "timeout" then test.fail() end
  thread.run(
    delay=delay,
    {
      if is_running(f) then test.fail() end
      cont()
    }
  )
end

# Ignores SIGTERM: killed by the SIGKILL that follows the grace period.
def ignores_sigterm() =
  f = pid_file()
  p =
    process.run(
      timeout=0.5,
      "trap '' TERM; echo $$ > #{process.quote(f)}; sleep 10"
    )
  check_terminated(timeout_status=p.status, delay=7., f, test.pass)
end

# The shell execs into the command: the command itself gets the signal.
def execs() =
  f = pid_file()
  p =
    process.run(
      timeout=0.5,
      "echo $$ > #{process.quote(f)}; exec sleep 10"
    )
  check_terminated(timeout_status=p.status, delay=1., f, ignores_sigterm)
end

output.dummy(blank())
thread.run(delay=0.5, execs)
