#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

s = playlist(mode="randomize",reload=1,reload_mode="rounds","files/playlist")
s = test.check_non_repeating(nb_files=3,nb_rounds=10,s)
clock(sync="none", s)
output.dummy(fallible=true,s)
