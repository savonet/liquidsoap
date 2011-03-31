# In LS-268 we realized that an incorrect assumption had
# been made in code from LS-394, resulting in a crash in
# case of source re-awakening.

p = input.http("http://localhost:8000/nonexistent")
o = output.dummy(fallible=true,p)

add_timeout(2.,{ source.shutdown(o) ; (-1.) })
add_timeout(3.,{ output.dummy(fallible=true,p) ; (-1.) })
add_timeout(4.,{ shutdown() ; (-1.) })
