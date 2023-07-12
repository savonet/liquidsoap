# In LS-503 we realized that a source may throw an
# exception during output_get_ready call in the initial
# main phase. This code reproduces the issue by throwing
# an exception in output.icecast.

# Reopen stderr to /dev/null to
# disable printing expected exception
#reopen.stderr("/dev/null")

on_cleanup(test.pass)

def on_error(_) =
  shutdown()
  3.
end

p = noise()
o = output.icecast(%wav,fallible=true,host="nonexistent",
                    mount="test",on_error=on_error,p)
