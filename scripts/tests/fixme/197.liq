%include "test.liq"

first = ref("")
def filter(r)
  m = request.metadata(r)
  if !first == "" then
    first := m["filename"]
    false
  else
    m["filename"] != !first
  end
end

s = playlist(check_next=filter,mode="randomize",reload=1,reload_mode="rounds","threesines")

s = test.check_non_repeating(nb_files=2,nb_rounds=10,s)

clock.assign_new(sync=false,[output.dummy(fallible=true,s)])
