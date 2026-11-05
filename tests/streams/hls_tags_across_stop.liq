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

# Tags across a stop: a tag inserted through a streams() entry kept from
# before a stop reaches the next run, and a tag pending at the stop before a
# sleep appears after the restore (KC20).
dir = file.temp_dir("hls_tags_across_stop")
on_cleanup({file.rmdir(dir)})

names = ["a", "b"]
s = sine()
o =
  output.file.hls(
    segment_duration=1.,
    segments=3,
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
stops = ref(0)
o.on_stop(synchronous=true, {stops := stops() + 1})

recorders = list.map(fun (n) -> (n, hls_recorder(dir, n)), names)
list.iter(
  fun (r) -> o.on_file_change(synchronous=true, snd(r).on_change),
  recorders
)
ra = list.assoc("a", recorders)
rb = list.assoc("b", recorders)

entry_b = list.find(fun (v) -> v.name == "b", o.streams())

no_segment = {uri = "", duration = 0., discontinuous = false, tags = []}

def tags_after(r, last) =
  list.filter(fun (x) -> fst(x) > last and snd(x).tags != [], r.segments())
end

def check() =
  wait_for(
    "4 segments",
    {ra.last_position() >= 4 and rb.last_position() >= 4}
  )

  o.stop()
  wait_for(
    "the output to stop",
    {stops() == 1}
  )
  last_a = ra.last_position()
  last_b = rb.last_position()
  entry_b.insert_tag("#EXT-X-KEPT-ENTRY")
  o.start()
  wait_for(
    "4 segments of the next run",
    {ra.last_position() >= last_a + 4 and rb.last_position() >= last_b + 4}
  )

  first_b = list.assoc(default=no_segment, last_b + 1, rb.segments())
  if
    first_b.tags != ["#EXT-X-KEPT-ENTRY"]
  then
    test.fail(
      "b: first segment of the next run has tags #{first_b.tags}, expected the \
       tag inserted through the kept entry"
    )
  end
  if
    list.length(tags_after(rb, last_b)) != 1
  then
    test.fail(
      "b: tagged segments #{tags_after(rb, last_b)}"
    )
  end
  if
    tags_after(ra, 0) != []
  then
    test.fail(
      "a: carries a tag inserted on b: #{tags_after(ra, 0)}"
    )
  end

  o.stop()
  wait_for(
    "the output to stop",
    {stops() == 2}
  )
  o.insert_tag("#EXT-X-PENDING-AT-SLEEP")
  last_a = ra.last_position()
  last_b = rb.last_position()
  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
  state = file.contents("#{dir}/state.json")
  if
    not string.contains(substring="EXT-X-PENDING-AT-SLEEP", state)
  then
    test.fail(
      "pending tag not in the saved state: #{state}"
    )
  end
  restart_clock(o.clock, wake_ups)
  if not o.is_started() then o.start() end
  wait_for(
    "a segment after the restore",
    {ra.last_position() > last_a and rb.last_position() > last_b}
  )

  list.iter(
    fun (x) ->
      begin
        let (name, r, last) = x
        first = list.assoc(default=no_segment, last + 1, r.segments())
        if
          first.tags != ["#EXT-X-PENDING-AT-SLEEP"]
        then
          test.fail(
            "#{name}: first segment after the restore has tags #{first.tags}"
          )
        end
      end,
    [("a", ra, last_a), ("b", rb, last_b)]
  )
  test.pass()
end

thread.run(check)
