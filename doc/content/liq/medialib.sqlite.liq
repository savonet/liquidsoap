m = medialib.sqlite(database="/tmp/medialib.sql", "~/music/")
l = m.find(artist_contains="Brassens")
l = list.shuffle(l)
output(playlist.list(l))
