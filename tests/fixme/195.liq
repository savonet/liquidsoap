#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

s = playlist(mime_type="audio/mpegurl", mode="randomize",reload=1,reload_mode="rounds","threesines")

s = test.check_non_repeating(nb_files=3,nb_rounds=10,s)

clock.assign_new(sync="none", [output.dummy(fallible=true,s)])
