%include "../test.liq"

# The native ogg decoder stopped with Invalid_stream at the second link of a
# chained ogg/flac stream: the flac decoder kept reading pages after its own
# stream ended, and got those of the next link (#3010). This writes a chained
# ogg/flac file of three tracks and decodes it back with the ogg decoder.

settings.decoder.decoders := ["ogg"]

file = file.temp(cleanup=true, "ogg_flac_chained", ".ogg")

def track(n) =
  metadata.map(
    insert_missing=true,
    fun (_) ->
      [
        (
          "title",
          "Track #{n}"
        )
      ],
    once(sine(duration=1., 300. + 100. * float(n)))
  )
end

encoded = sequence([track(1), track(2), track(3)])
clock.assign_new(sync="none", [encoded])

def decode(_) =
  titles = ref([])
  duration = ref(0.)
  s = once(single(file))
  s.on_frame(synchronous=true, fun () -> duration := s.time())
  s.on_metadata(
    synchronous=true,
    fun (m) -> if m["title"] != "" then titles := [...titles(), m["title"]] end
  )

  def check() =
    print(
      "titles: #{titles()}, duration: #{duration()}"
    )
    if
      titles() ==
        [
          "Track 1",
          "Track 2",
          "Track 3"
        ]
      and duration() > 2.9
    then
      test.pass()
    else
      test.fail()
    end
  end

  clock.assign_new(sync="none", [s])
  o = output.dummy(fallible=true, s)
  o.on_stop(synchronous=true, check)
end

output.file(fallible=true, on_close=decode, %ogg(%flac), file, encoded)
