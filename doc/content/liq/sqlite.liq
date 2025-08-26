#!../../../liquidsoap

%ifdef sqlite
# open-begin
db = sqlite("/tmp/database.sql")

# open-end
# drop-begin
db.table.drop("metadata")

# drop-end
# create-begin
db.table.create(
  "metadata",
  preserve=true,
  [
    (
      "filename",
      "STRING PRIMARY KEY"
    ),
    ("artist", "STRING"),
    ("title", "STRING"),
    ("year", "INT")
  ]
)

# create-end
# insert-begin
db.insert(
  table="metadata",
  {
    artist="Naps",
    title=
      "Best life",
    year=2021,
    filename="naps.mp3"
  }
)
db.insert(
  table="metadata",
  {
    artist="Orelsan",
    title=
      "L'odeur de l'essence",
    year=2021,
    filename="orelsan.mp3"
  }
)

# insert-end

# count-begin
n = db.count(table="metadata", where="year=2023")

# count-end
ignore(n)

# select-begin
l =
  db.select(
    table="metadata",
    where="year >= 2000"
  )

# select-end
# select2-begin
find_artist = "Brassens"
l' =
  db.select(
    table="metadata",
    where="artist = #{sqlite.escape(find_artist)}"
  )

# select2-end
ignore(l')

# query-begin
l'' =
  db.query(
    "SELECT * FROM metadata WHERE artist = 'bla'"
  )

# query-end
ignore(l'')

# play-begin
files =
  list.map(fun (row) -> null.get(list.assoc("filename", row.to_list())), l)
s = playlist.list(files)
output(s)

# play-end
# play2-begin
def f(row) =
  let sqlite.row (r :
    {filename: string, artist: string, title: string, year: int}
  ) = row
  r.filename
end
files = list.map(f, l)
s = playlist.list(files)
output(s)

# play2-end
# delete-begin
db.delete(
  table="metadata",
  where="year < 1900"
)

# delete-end
# exec-begin
db.exec(
  "DROP TABLE IF EXISTS metadata"
)
# exec-end
%endif
