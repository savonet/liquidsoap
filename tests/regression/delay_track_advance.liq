# Regression test: delay(d, s) must let s advance through successive tracks and
# preserve each track's metadata.
#
# delay used to reset its timer on the leading track mark of every new track,
# not just the first, so a delayed source got stuck on its first track forever.
# It also returned the pre-mark content at each boundary and dropped the
# post-mark content, losing the next track's metadata. Here we chop a sine into
# tracks with distinct, incrementing titles and check that at least three
# distinct titles make it through the delay.

count = ref(0)
def next_metadata() =
  i = count()
  ref.incr(count)
  [("title", "t#{i}")]
end

s = sine()
s = chop(every=0.5, metadata=next_metadata, s)
s = delay(0.3, s)

seen = ref([])
s.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      title = m["title"]
      if
        title != "" and not list.mem(title, seen())
      then
        seen := [...seen(), title]
      end
      if list.length(seen()) >= 3 then test.pass() end
    end
)

output.dummy(fallible=true, s)

thread.run(delay=15., {test.fail()})
