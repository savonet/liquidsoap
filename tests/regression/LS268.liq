# In LS-268 we realized that an incorrect assumption had
# been made in code from LS-394, resulting in a crash in
# case of source re-awakening.

p = input.http("http://localhost:8000/nonexistent")
o = output.dummy(fallible=true,p)

on_cleanup(test.pass)

thread.run(delay=2., { o.shutdown() })
thread.run(delay=3., { ignore(output.dummy(fallible=true,p)) })
thread.run(delay=4., { shutdown() })
