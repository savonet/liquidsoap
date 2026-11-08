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

# A truncated or corrupt state file gives a fresh start, and nothing of it is
# applied. Each sleep drops the in-memory state, so the restore reads the file.
dir = file.temp_dir("hls_persist_corrupt")
on_cleanup({file.rmdir(dir)})

state = "#{dir}/state.json"
names = ["a", "b"]

# Names unique to each run keep this test apart from the files of earlier
# runs, which hls_persist_state_removed covers.
run = ref(0)
s = sine()
o =
  output.file.hls(
    segment_duration=1.,
    segments=3,
    segment_name=fun (s) ->
      "#{s.stream_name}_r#{run()}_#{s.position}.#{s.extname}",
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

recorders = ref([])
def new_recorders() =
  recorders := list.map(fun (n) -> (n, hls_recorder(dir, n)), names)
end
new_recorders()
o.on_file_change(
  synchronous=true,
  fun (e) -> list.iter(fun (r) -> snd(r).on_change(e), recorders())
)

def run_until(n) =
  wait_for(
    "#{n} segments in every variant",
    {list.for_all(fun (r) -> snd(r).last_position() >= n, recorders())}
  )
end

def sleep() =
  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
end

def wake_up() =
  run := run() + 1
  new_recorders()
  restart_clock(o.clock, wake_ups)
end

checks = ref(0)

# A fresh start: numbering from 1, no discontinuity.
def fresh(what) =
  run_until(3)
  list.iter(
    fun (r) ->
      begin
        let (name, r) = r
        p = list.hd(r.history())
        if
          p.media_sequence != 0
        then
          test.fail(
            "#{what}, #{name}: first playlist has media sequence #{
              p.media_sequence
            }, expected a fresh start at 0"
          )
        end
        first =
          list.assoc(
            default={uri = "", duration = 0., discontinuous = true, tags = []},
            1,
            r.segments()
          )
        if
          first.uri == ""
        then
          test.fail(
            "#{what}, #{name}: no segment 1 after a fresh start"
          )
        end
        if
          first.discontinuous
        then
          test.fail(
            "#{what}, #{name}: first segment of a fresh start is discontinuous"
          )
        end
        if
          list.exists(fun (p) -> p.discontinuity_sequence != 0, r.history())
        then
          test.fail(
            "#{what}, #{name}: discontinuity sequence is not 0 after a fresh \
             start"
          )
        end
        checks := checks() + 1
      end,
    recorders()
  )
end

def check() =
  run_until(8)
  sleep()
  contents = file.contents(state)
  if
    string.length(contents) < 10
  then
    test.fail(
      "state file too short: #{contents}"
    )
  end

  # Truncated: half of a valid file.
  file.write(
    data=string.sub(contents, start=0, length=string.length(contents) / 2),
    state
  )
  wake_up()
  fresh(
    "truncated state"
  )
  sleep()

  # Valid for variant a, invalid for variant b (positions go back): neither
  # is applied, so a does not continue at 50.
  file.write(
    data='{"variants": [
        {"name": "a", "next_position": 50, "next_init_position": 0,
         "discontinuity_count": 3, "window_start": 50, "pending_tags": [],
         "segments": []},
        {"name": "b", "next_position": 50, "next_init_position": 0,
         "discontinuity_count": 0, "window_start": 1, "pending_tags": [],
         "segments": [
           {"position": 3, "discontinuous": false, "discontinuity": 0,
            "forced": false, "name": "b_3.aac", "tags": [], "duration": 1.0},
           {"position": 2, "discontinuous": false, "discontinuity": 0,
            "forced": false, "name": "b_2.aac", "tags": [], "duration": 1.0}]}
      ]}',
    state
  )
  wake_up()
  fresh(
    "half-valid state"
  )
  sleep()

  # Not JSON at all.
  file.write(data="\000\001garbage", state)
  wake_up()
  fresh(
    "garbage state"
  )

  if
    checks() != 6
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(check)
