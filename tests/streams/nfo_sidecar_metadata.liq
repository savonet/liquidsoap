def f() =
  media = file.temp("nfo-sidecar", ".nfotest", cleanup=true)
  file.write(data="x", media)

  nfo_file = path.remove_extension(media) ^ ".nfo"
  nfo =
    '<movie>
      <title>  Robot Movie  </title>
      <year>2024</year>
      <plot>A test plot.</plot>
      <genre>Action</genre>
      <genre>Drama</genre>
      <uniqueid type="IMDB">tt1234567</uniqueid>
      <uniqueid type="tmdb">98765</uniqueid>
      <actor>
        <name>Someone</name>
      </actor>
    </movie>'

  file.write(data=nfo, nfo_file)

  nfo_md = file.nfo.metadata(nfo_file)
  test.equal(list.assoc("nfo.root", nfo_md), "movie")
  test.equal(list.assoc("nfo.title", nfo_md), "Robot Movie")
  test.equal(list.assoc("nfo.genre", nfo_md), "Action;Drama")
  test.equal(list.assoc("nfo.uniqueid.imdb", nfo_md), "tt1234567")

  enable_nfo_metadata(file_extensions=["nfotest"], populate_standard=true)

  m = file.metadata(media)

  test.equal(list.assoc("nfo.root", m), "movie")
  test.equal(list.assoc("nfo.title", m), "Robot Movie")
  test.equal(list.assoc("nfo.year", m), "2024")
  test.equal(list.assoc("nfo.plot", m), "A test plot.")
  test.equal(list.assoc("nfo.genre", m), "Action;Drama")
  test.equal(list.assoc("nfo.uniqueid.imdb", m), "tt1234567")
  test.equal(list.assoc("nfo.uniqueid.tmdb", m), "98765")

  # Populated standard keys
  test.equal(list.assoc("title", m), "Robot Movie")
  test.equal(list.assoc("year", m), "2024")
  test.equal(list.assoc("genre", m), "Action;Drama")

  file.remove(nfo_file)
  test.pass()
end

test.check(f)
