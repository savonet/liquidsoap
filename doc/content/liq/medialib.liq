m = medialib(persistency="/tmp/medialib.json", "~/Music/")
l = m.find(artist_contains="Brassens")
l = list.shuffle(l)
output(playlist.list(l))
