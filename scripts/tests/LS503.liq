# In LS-503 we realized that a source may throw an
# exception during output_get_ready call in the initial
# main phase. This code reproduces the issue by throwing
# an exception in output.icecast.

# Reopen stderr to /dev/null to
# disable printing expected exception
reopen.stderr("/dev/null")

p = input.http("http://localhost:8000/nonexistent")
o = output.icecast(%wav,fallible=true,host="nonexistent",
                    mount="test",p)

