port = 6723
settings.icecast.prefer_address := "ipv4"

s = sine()

insert_metadata = s.insert_metadata

output.dummy(s)

thread.run(
  delay=0.1,
  {
    insert_metadata(
      [
        (
          "title",
          "some title"
        )
      ]
    )
  }
)

thread.run(
  delay=0.3,
  {output.icecast(port=port, mount="metadata_test", %mp3, s)}
)

i = input.harbor(buffer=2., port=port, "metadata_test")

i.on_metadata(
  synchronous=true,
  fun (m) ->
    if
      m["title"] ==
        "some title"
    then
      test.pass()
    end
)

output.dummy(fallible=true, i)
