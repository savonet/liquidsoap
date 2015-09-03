%include "test.liq"

s = playlist(mode="normal",reload=1,reload_mode="rounds","threesines")

s = test.check_non_repeating(nb_files=3,nb_rounds=10,s)

clock.assign_new(sync=false,[output.dummy(fallible=true,s)])
