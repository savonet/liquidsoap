# In LS-503 we realized that a source may throw an
# exception during output_get_ready call in the initial
# main phase. This code reproduces the issue by throwing
# an exception in output.icecast.

# Reopen stderr to /dev/null to
# disable printing expected exception
#reopen.stderr("/dev/null")
on_cleanup(test.pass)

def on_error(~restart_in:_, _) =
  test.pass()
end

p = noise()
o = output.icecast(%wav, fallible=true, host="nonexistent", mount="test", p)

o.on_error(on_error)
