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

# Cuts on metadata and inserted tags with only free-split variants (KC6): a
# new title or an output-wide tag starts a new segment at its media time in
# every variant, the grid after it is unchanged, and the same metadata sent
# again causes no cut.
dir = file.temp_dir("hls_metadata_cuts")
on_cleanup({file.rmdir(dir)})

names = ["a", "b"]
s = sine()
o =
  output.file.hls(
    segment_duration=2.,
    segments=20,
    segments_overhead=null,
    dir,
    [("a", %mp3), ("b", %fdkaac)],
    s
  )

clock.assign_new(sync="none", [o])

recorders = list.map(fun (n) -> (n, hls_recorder(dir, n)), names)
list.iter(
  fun (r) -> o.on_file_change(synchronous=true, snd(r).on_change),
  recorders
)

frame = settings.frame.duration()
def frame_of(t) =
  int_of_float(t / frame + 0.5)
end

new_titles = [(1.64, "b"), (15.38, "c")]
repeats = [2.5, 4.3, 5.5, 6.7, 7.3, 8.5, 10.3, 11.5, 12.7, 13.3]
tag_time = 9.18

# Media time of each event, at the start of the frame that carries it.
events = ref([])
frames = ref(0)
s.on_frame(
  before=true,
  synchronous=true,
  fun () ->
    begin
      n = frames()
      frames := n + 1
      t = float(n) * frame
      list.iter(
        fun (x) ->
          if
            frame_of(fst(x)) == n
          then
            begin
              events := [...events(), t]
              s.insert_metadata([("title", snd(x))])
            end
          end,
        new_titles
      )
      if
        list.exists(fun (r) -> frame_of(r) == n, repeats)
      then
        s.insert_metadata([("title", "b")])
      end
      if
        frame_of(tag_time) == n
      then
        events := [...events(), t]
        o.insert_tag("#EXT-X-CUE")
      end
    end
)

# One codec frame (1152 samples at 44.1 kHz) plus EXTINF rounding.
tolerance = 0.03

def check() =
  def recorded(r) =
    list.fold(fun (t, x) -> t + snd(x).duration, 0., snd(r).segments())
  end
  wait_for(
    "24 s in every variant",
    {list.for_all(fun (r) -> recorded(r) >= 24., recorders)}
  )

  # Grid boundaries, plus one cut per new title and per tag.
  grid = list.init(12, fun (k) -> float(2 * k))
  expected =
    list.sort(
      fun (x, y) -> if x < y then -1 elsif x > y then 1 else 0 end,
      [...grid, ...events()]
    )
  if
    list.length(events()) != 3
  then
    test.fail(
      "only #{list.length(events())} events sent"
    )
  end

  counts = ref([])
  checks = ref(0)
  def check_variant(r) =
    let (name, r) = r
    segments = list.sort(fun (x, y) -> fst(x) - fst(y), r.segments())
    starts = ref([])
    t = ref(0.)
    list.iter(
      fun (x) ->
        begin
          starts := [...starts(), (t(), snd(x))]
          t := t() + snd(x).duration
        end,
      segments
    )
    starts = list.filter(fun (x) -> fst(x) < 24. - tolerance, starts())
    counts := [...counts(), list.length(starts)]
    if
      list.length(starts) != list.length(expected)
    then
      test.fail(
        "#{name}: #{list.length(starts)} segments started before 24 s, expected #{
          list.length(expected)
        }: starts #{list.map(fst, starts)}, expected #{expected}"
      )
    end
    list.iteri(
      fun (i, x) ->
        begin
          e = list.nth(expected, i)
          if
            abs(fst(x) - e) > tolerance
          then
            test.fail(
              "#{name}: segment #{i + 1} starts at #{fst(x)}, expected #{e}"
            )
          end
          tagged = abs(e - list.nth(events(), 1)) < tolerance
          if
            tagged and snd(x).tags != ["#EXT-X-CUE"]
          then
            test.fail(
              "#{name}: segment starting at #{fst(x)} has tags #{snd(x).tags}, \
               expected the inserted tag"
            )
          end
          if
            not tagged and snd(x).tags != []
          then
            test.fail(
              "#{name}: segment starting at #{fst(x)} has tags #{snd(x).tags}"
            )
          end
          checks := checks() + 1
        end,
      starts
    )
  end
  list.iter(check_variant, recorders)

  if
    list.nth(counts(), 0) != list.nth(counts(), 1)
  then
    test.fail(
      "segment counts differ: #{counts()}"
    )
  end
  if
    checks() < 30
  then
    test.fail(
      "only #{checks()} segment checks ran"
    )
  end
  test.pass()
end

thread.run(check)
