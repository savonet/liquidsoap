m = medialib(persistency="/tmp/medialib.json", "~/music/")
l = m.find(artist_contains="Brassens")
l = list.shuffle(l)
output(playlist.list(l))
