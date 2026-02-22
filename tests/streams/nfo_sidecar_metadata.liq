def f() =
  test.equal(file.nfo.metadata("./does-not-exist.nfo"), [])

  dir = file.temp_dir(cleanup=true, "nfo-sidecar")
  media = "#{dir}/file1.mp3"
  file.copy("file1.mp3", media)

  nfo_file = path.remove_extension(media) ^ ".nfo"
  nfo =
    '<kodi:Movie xmlns:kodi="urn:kodi">
      <kodi:Title>  Robot Movie  </kodi:Title>
      <YEAR>2024</YEAR>
      <plot>A test plot.</plot>
      <genre>Action</genre>
      <genre>Drama</genre>
      <kodi:genre>Comedy</kodi:genre>
      <uniqueid type="tmdb" default="true">98765</uniqueid>
      <uniqueid type="IMDB" default="false">tt1234567</uniqueid>
      <uniqueid type=" imdb ">tt7654321</uniqueid>
      <actor>
        <name>Someone</name>
      </actor>
    </kodi:Movie>'

  file.write(data=nfo, nfo_file)

  nfo_md = file.nfo.metadata(nfo_file)
  test.equal(list.assoc("nfo.root", nfo_md), "movie")
  test.equal(list.assoc("nfo.title", nfo_md), "Robot Movie")
  test.equal(list.assoc("nfo.genre", nfo_md), "Action;Drama;Comedy")
  test.equal(list.assoc("nfo.uniqueid.tmdb", nfo_md), "98765")
  test.equal(list.assoc("nfo.uniqueid.imdb", nfo_md), "tt1234567;tt7654321")
  test.equal(list.assoc("nfo.uniqueid.default", nfo_md), "98765")
  test.equal(list.assoc("nfo.uniqueid.default.type", nfo_md), "tmdb")

  nfo_md_custom =
    file.nfo.metadata(
      extra_rules=[
        (
          "YeAr",
          fun (_, txt) -> [("year", txt), ("release_year", txt)]
        )
      ],
      nfo_file
    )
  test.equal(list.assoc("nfo.release_year", nfo_md_custom), "2024")

  enable_nfo_metadata(file_extensions=["mp3"])

  m = file.metadata(media)

  test.equal(list.assoc("nfo.root", m), "movie")
  test.equal(list.assoc("nfo.title", m), "Robot Movie")
  test.equal(list.assoc("nfo.year", m), "2024")
  test.equal(list.assoc("nfo.plot", m), "A test plot.")
  test.equal(list.assoc("nfo.genre", m), "Action;Drama;Comedy")
  test.equal(list.assoc("nfo.uniqueid.imdb", m), "tt1234567;tt7654321")
  test.equal(list.assoc("nfo.uniqueid.tmdb", m), "98765")
  test.equal(list.assoc("nfo.uniqueid.default", m), "98765")
  test.equal(list.assoc("nfo.uniqueid.default.type", m), "tmdb")
  test.equal(list.assoc("title", m), "Test Title")
  test.pass()
end

test.check(f)
