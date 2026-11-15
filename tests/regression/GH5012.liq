%include "../test.liq"

# A live source taking over through the default transition keeps the track mark
# and the metadata it starts with. input.harbor keeps producing whether the
# switch reads it or not, so anything not read right away is lost.

h = input.harbor("live", port=5428, password="hackme")

def fallback_title(m) =
  if m == [] then [("title", "live")] else m end
end

live = metadata.map(insert_missing=true, fallback_title, h)
live = blank.strip(max_blank=10., live)

music =
  metadata.map(insert_missing=true, fun (_) -> [("title", "music")], sine())

s = fallback([live.{track_sensitive = false}, music])

tracks = ref([])
live_at = ref(null)

s.on_track(
  synchronous=true,
  fun (m) ->
    begin
      tracks := [...tracks(), m["title"]]
      if m["title"] == "live" then live_at := time() end
    end
)

output.dummy(s)

# The client sends no metadata.
output.icecast(
  %mp3,
  host="localhost",
  port=5428,
  password="hackme",
  mount="live",
  mksafe(sine(880.))
)

# The first icecast connection can race the harbor port opening and wait out
# the 3s reconnect delay, so poll instead of checking at a fixed time.
deadline = time() + 20.

def check() =
  if
    null.defined(live_at()) and time() > null.get(live_at()) + 2.
  then
    if
      tracks() == ["music", "live"]
    then
      test.pass()
    else
      test.fail(
        "unexpected tracks: #{tracks()}"
      )
    end
    (-1.)
  elsif
    time() > deadline
  then
    test.fail(
      "live never took over with its metadata: #{tracks()}"
    )
    (-1.)
  else
    0.1
  end
end

thread.run.recurrent(delay=1., check)
