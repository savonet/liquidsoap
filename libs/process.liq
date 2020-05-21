# Perform a shell call and return its output.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to run
def get_process_output(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  let (stdout, _, _) = run_process(timeout=timeout,env=env,inherit_env=inherit_env,command)
  stdout
end

# Perform a shell call and return the list of its output lines.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to run
def get_process_lines(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  string.split(separator="\r?\n",get_process_output(timeout=timeout,env=env,inherit_env=inherit_env,command))
end

# Return true if process exited with 0 code.
# @category System
# @param ~timeout Cancel process after `timeout` has elapsed. Ignored if negative.
# @param ~env Process environment
# @param ~inherit_env Inherit calling process's environment when `env` parameter is empty.
# @param command Command to test
def test_process(~timeout=(-1.),~env=[],~inherit_env=true,command) =
  let (_, _, status) = run_process(timeout=timeout,env=env,command)
  status == ("exit","0")
end

# Read some value from standard input (console).
# @category System
# @param ~hide Hide typed characters (for passwords).
def read(~hide=false)
  if hide then
    system("stty -echo")
  end
  s = list.hd(default="",get_process_lines("read BLA && echo $BLA"))
  if hide then
    system("stty echo")
  end
  print("")
  s
end
