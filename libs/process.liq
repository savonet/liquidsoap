# Perform a shell call and return the list of its output lines.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to run
def process.read(~timeout=(-1.),~env=[],~inherit_env=true,command)
  p = process.run(timeout=timeout, env=env, inherit_env=inherit_env, command)
  p.stdout
end

# Perform a shell call and return the list of its output lines.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to run
def process.read.lines(~timeout=(-1.),~env=[],~inherit_env=true,command)
  s = process.read(timeout=timeout, env=env, inherit_env=inherit_env, command)
  string.split(separator="\r?\n", s)
end

# Return true if process exited with 0 code.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to test
def process.test(~timeout=(-1.),~env=[],~inherit_env=true,command)
  p = process.read(timeout=timeout, env=env, inherit_env=inherit_env, command)
  p.status == "exit"
end

# Read some value from standard input (console).
# @category System
# @param ~hide Hide typed characters (for passwords).
def read(~hide=false)
  if hide then
    process.run("stty -echo")
  end
  s = list.hd(default="", process.read.lines("read BLA && echo $BLA"))
  if hide then
    process.run("stty echo")
  end
  print("")
  s
end
