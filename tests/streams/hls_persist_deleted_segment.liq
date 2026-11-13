%include "../test.liq"

# Poll `cond` until it holds, or fail the test naming `what`.
def wait_for(~timeout=20., what, cond) =
  deadline = time() + timeout
  def rec loop() =
    if
      not cond()
    then
      if
        time() > deadline
      then
        test.fail(
          "timeout waiting for #{what}"
        )
      end
      thread.pause(0.02)
      loop()
    end
  end
  loop()
end

# Parse a media playlist into its header numbers and segments.
def hls_parse(content) =
  lines = string.split(separator="\r\n", content)
  media_sequence = ref(-1)
  discontinuity_sequence = ref(-1)
  segments = ref([])
  discontinuous = ref(false)
  tags = ref([])
  duration = ref(0.)
  header =
    [
      "#EXTM3U",
      "#EXT-X-TARGETDURATION:",
      "#EXT-X-VERSION:",
      "#EXT-X-INDEPENDENT-SEGMENTS"
    ]

  def is_header(l) =
    list.exists(fun (h) -> string.contains(prefix=h, l), header)
  end

  def value(prefix, l) =
    null.get(string.residual(prefix=prefix, l))
  end

  def handle(l) =
    if
      l == "" or is_header(l)
    then
      ()
    elsif
      string.contains(prefix="#EXT-X-MEDIA-SEQUENCE:", l)
    then
      media_sequence := int_of_string(value("#EXT-X-MEDIA-SEQUENCE:", l))
    elsif
      string.contains(prefix="#EXT-X-DISCONTINUITY-SEQUENCE:", l)
    then
      discontinuity_sequence :=
        int_of_string(value("#EXT-X-DISCONTINUITY-SEQUENCE:", l))
    elsif
      l == "#EXT-X-DISCONTINUITY"
    then
      discontinuous := true
    elsif
      string.contains(prefix="#EXT-X-MAP:", l)
    then
      ()
    elsif
      string.contains(prefix="#EXTINF:", l)
    then
      d = value("#EXTINF:", l)
      duration :=
        float_of_string(string.sub(d, start=0, length=string.length(d) - 1))
    elsif
      string.contains(prefix="#", l)
    then
      tags := [...tags(), l]
    else
      segments :=
        [
          ...segments(),
          {
            uri = l,
            duration = duration(),
            discontinuous = discontinuous(),
            tags = tags()
          }
        ]
      discontinuous := false
      tags := []
    end
  end

  list.iter(handle, lines)
  {
    media_sequence = media_sequence(),
    discontinuity_sequence = discontinuity_sequence(),
    segments = segments()
  }
end

# Every version of a variant's media playlist, parsed at each file-change
# event, and every segment it listed, by position number. A position listed
# under two URIs fails the test.
def hls_recorder(dir, name) =
  history = ref([])
  seen = ref([])

  def on_change(e) =
    if
      e.state != "deleted" and e.path == "#{dir}/#{name}.m3u8"
    then
      p = hls_parse(file.contents(e.path))
      history := [...history(), p]

      def add(i, seg) =
        pos = p.media_sequence + 1 + i
        if
          list.assoc.mem(pos, seen())
        then
          let old = list.assoc(pos, seen())
          if
            old.uri != seg.uri
          then
            test.fail(
              "#{name}: position #{pos} listed as #{old.uri} then #{seg.uri}"
            )
          end
        else
          seen := [...seen(), (pos, seg)]
        end
      end

      list.iteri(add, p.segments)
    end
  end

  {
    on_change = on_change,
    history = fun () -> history(),
    segments = fun () -> seen(),
    last_position = fun () -> list.fold(fun (m, x) -> max(m, fst(x)), 0, seen())
  }
end

# Start a stopped clock again, until `wake_ups` counts one more wake-up. A
# start sent while the clock is still winding down is refused.
def restart_clock(c, wake_ups) =
  deadline = time() + 20.
  before = wake_ups()
  def rec loop() =
    if
      wake_ups() == before
    then
      if
        time() > deadline
      then
        test.fail(
          "timeout waiting for the output to wake up"
        )
      end
      try
        clock(c).start()
      catch _ do
        ()
      end
      thread.pause(0.05)
      loop()
    end
  end
  loop()
end

# A state naming a segment deleted by hand: the restore keeps the newer
# segments, drops it and the older ones, and keeps the counters.
dir = file.temp_dir("hls_persist_deleted_segment")
on_cleanup({file.rmdir(dir)})

names = ["a", "b"]
s = sine()
o =
  output.file.hls(
    segment_duration=1.,
    segments=4,
    persist_at="state.json",
    dir,
    [("a", %mp3), ("b", %fdkaac)],
    s
  )

clock.assign_new(sync="none", [o])

awake = ref(true)
o.on_shutdown(synchronous=true, {awake := false})
wake_ups = ref(0)
o.on_wake_up(
  synchronous=true,
  {
    awake := true
    wake_ups := wake_ups() + 1
  }
)

recorders = list.map(fun (n) -> (n, hls_recorder(dir, n)), names)
list.iter(
  fun (r) -> o.on_file_change(synchronous=true, snd(r).on_change),
  recorders
)
ra = list.assoc("a", recorders)
rb = list.assoc("b", recorders)

def check() =
  wait_for(
    "8 segments in every variant",
    {list.for_all(fun (r) -> snd(r).last_position() >= 8, recorders)}
  )
  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )

  pa = hls_parse(file.contents("#{dir}/a.m3u8"))
  pb = hls_parse(file.contents("#{dir}/b.m3u8"))
  if
    list.length(pa.segments) < 4
  then
    test.fail(
      "window of a too short: #{pa.segments}"
    )
  end
  last_a = pa.media_sequence + list.length(pa.segments)
  last_b = pb.media_sequence + list.length(pb.segments)
  deleted = list.nth(pa.segments, 1)
  kept = list.tl(list.tl(pa.segments))
  file.remove("#{dir}/#{deleted.uri}")
  rewrites_a = list.length(ra.history())
  rewrites_b = list.length(rb.history())

  restart_clock(o.clock, wake_ups)
  wait_for(
    "a new segment in every variant",
    {ra.last_position() > last_a and rb.last_position() > last_b}
  )

  # The first playlist of a after the restore starts after the deleted
  # segment, at the same position numbers.
  restored = list.nth(ra.history(), rewrites_a)
  first =
    list.hd(
      default={uri = "", duration = 0., discontinuous = false, tags = []},
      restored.segments
    )
  if
    first.uri != list.hd(kept).uri
  then
    test.fail(
      "a: first segment after restore is #{first.uri}, expected #{
        list.hd(kept).uri
      } (deleted #{deleted.uri})"
    )
  end
  if
    restored.media_sequence != pa.media_sequence + 2
  then
    test.fail(
      "a: media sequence #{restored.media_sequence} after restore, expected #{
        pa.media_sequence + 2
      }"
    )
  end
  if
    list.exists(
      fun (seg) ->
        seg.uri == deleted.uri or seg.uri == list.hd(pa.segments).uri,
      restored.segments
    )
  then
    test.fail(
      "a: a dropped segment is still listed: #{restored.segments}"
    )
  end
  list.iteri(
    fun (i, seg) ->
      if
        i < list.length(kept) and seg.uri != list.nth(kept, i).uri
      then
        test.fail(
          "a: restored segment #{i} is #{seg.uri}, expected #{
            list.nth(kept, i).uri
          }"
        )
      end,
    restored.segments
  )

  # Counters kept: numbering continues in both variants.
  new_a =
    list.assoc(
      default={uri = "", duration = 0., discontinuous = false, tags = []},
      last_a + 1,
      ra.segments()
    )
  new_b =
    list.assoc(
      default={uri = "", duration = 0., discontinuous = false, tags = []},
      last_b + 1,
      rb.segments()
    )
  if
    new_a.uri == "" or not new_a.discontinuous
  then
    test.fail(
      "a: no discontinuous segment #{last_a + 1} after restore"
    )
  end
  if
    new_b.uri == "" or not new_b.discontinuous
  then
    test.fail(
      "b: no discontinuous segment #{last_b + 1} after restore"
    )
  end

  # Variant b is untouched.
  restored_b = list.nth(rb.history(), rewrites_b)
  if
    restored_b.media_sequence != pb.media_sequence
  then
    test.fail(
      "b: media sequence #{restored_b.media_sequence} after restore, expected #{
        pb.media_sequence
      }"
    )
  end
  test.pass()
end

thread.run(check)
