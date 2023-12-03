#!../../../liquidsoap

%ifdef sqlite
# open-begin
db = sqlite("/tmp/database.sql")

# open-end
# create-begin
db.table.create(
  "metadata",
  preserve=true,
  [
    (
      "filename",
      "STRING UNIQUE"
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
%endif
