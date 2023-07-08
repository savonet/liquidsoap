log.level.set(3)

first = ref("")
def filter(r)
  m = request.metadata(r)
  if first() == "" then
    first := m["filename"]
    false
  else
    m["filename"] != first()
  end
end

s = playlist(check_next=filter, mode="randomize", reload_mode="rounds", "playlist")
s = test.check_non_repeating(nb_files=2, nb_rounds=10, s)
clock.assign_new(sync="none",[s])
output.dummy(fallible=true,s)
