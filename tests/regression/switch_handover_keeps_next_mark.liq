%include "../test.liq"

# A fallback hands over from a live source to a file source just as the file
# source begins a track. The default handover fades the live source out, then
# switches to the file source without starting a new track there. Every track
# of the file source is still announced, the second one included.

def tagged(title) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=1.5)
  )
end

available = ref(false)
music =
  source.available(
    sequence([tagged("m1"), tagged("m2"), tagged("m3")]),
    {available()}
  )

radio = fallback([music, sine(330.)])

announced = ref([])
radio.on_track(
  synchronous=true,
  fun (m) ->
    if m["title"] != "" then announced := [...announced(), m["title"]] end
)

# The file source has played out by 6 s of stream.
radio.on_frame(
  synchronous=true,
  fun () ->
    begin
      if 1.2 <= source.time(radio) then available := true end
      if
        6. <= source.time(radio)
      then
        if
          announced() != ["m1", "m2", "m3"]
        then
          test.fail(
            "announced #{announced()}"
          )
        else
          test.pass()
        end
      end
    end
)

clock.assign_new(sync="none", [radio])
output.dummy(fallible=true, radio)
