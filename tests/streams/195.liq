#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

s = playlist(mode="randomize",reload=1,reload_mode="rounds","playlist")
s = test.check_non_repeating(nb_files=3,nb_rounds=10,s)
clock(sync="none", output.dummy(fallible=true,s))
