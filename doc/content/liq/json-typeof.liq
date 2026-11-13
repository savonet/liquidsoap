history = ref([])

try
  let json.parse (saved : %typeof(history())) = file.contents("history.json")

  history := saved
catch _ do
  ()
end

def save_track(m) =
  history := [{artist = m["artist"], title = m["title"]}, ...history()]
  file.write(data=json.stringify(history()), "history.json")
end
