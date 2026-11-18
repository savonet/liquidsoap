%include "../test.liq"

# A merged sequence is one track: reported once, at the start of the stream,
# with the first item's metadata. The later item's metadata is announced at
# the join, with no boundary. The same holds whether the join falls inside a
# frame or on a frame edge, and skipping the sequence still gives a boundary.

def tagged(title, duration) =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("title", title)],
    sine(duration=duration)
  )
end

pending = ref(3)
def done() =
  pending := pending() - 1
  if pending() == 0 then test.pass() end
end

def check(~skip_at=null, ~tracks_expected, first_duration) =
  s =
    sequence(merge=true, [tagged("first", first_duration), tagged("last", 0.5)])

  tracks = ref([])
  titles = ref([])
  s.on_track(synchronous=true, fun (m) -> tracks := [...tracks(), m["title"]])
  s.on_metadata(
    synchronous=true,
    fun (m) -> titles := [...titles(), m["title"]]
  )

  skipped = ref(false)
  if
    null.defined(skip_at)
  then
    s.on_position(
      synchronous=true,
      position=null.get(skip_at),
      fun (_, _) ->
        if
          not skipped()
        then
          skipped := true
          s.skip()
        end
    )
  end

  clock.assign_new(sync="none", [s])
  o = output.dummy(fallible=true, s)
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        test.equal(tracks(), tracks_expected)
        test.equal(titles(), ["first", "last"])
        done()
      end
  )
end

# Join on a frame edge, then inside a frame.
check(tracks_expected=["first"], 0.2)
check(tracks_expected=["first"], 0.213)

# Skipped during the first item: a boundary, and the last item from its start.
check(skip_at=0.1, tracks_expected=["first", "last"], 0.4)
