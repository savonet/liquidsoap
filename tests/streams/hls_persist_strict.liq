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

# With strict_persist, an invalid state file makes the output fail without
# calling reopen_on_error. A control output with the same file and
# strict_persist=false streams in the same time.
def make(strict) =
  dir = file.temp_dir("hls_persist_strict")
  on_cleanup({file.rmdir(dir)})
  file.write(
    data='{"variants": [{"name": "a"',
    "#{dir}/state.json"
  )
  calls = ref(0)
  o =
    output.file.hls(
      id="hls_strict_#{strict}",
      strict_persist=strict,
      persist_at="state.json",
      reopen_on_error=fun (_) ->
        begin
          calls := calls() + 1
          0.1
        end,
      segment_duration=1.,
      dir,
      [("a", %mp3)],
      sine()
    )
  errors = ref([])
  clock.assign_new(
    sync="none",
    on_error=fun (e) -> errors := [...errors(), error.methods(e).message],
    [o]
  )
  {dir = dir, calls = calls, errors = errors, output = o}
end

strict = make(true)
control = make(false)

def segments(dir) =
  list.filter(fun (f) -> string.contains(suffix=".mp3", f), file.ls(dir))
end

def check() =
  wait_for(
    "the control output to write segments",
    {list.length(segments(control.dir)) >= 5}
  )
  wait_for(
    "the strict output to fail",
    {strict.errors() != []}
  )
  if
    strict.calls() != 0
  then
    test.fail(
      "reopen_on_error called #{strict.calls()} times on a strict restore \
       failure"
    )
  end
  if
    segments(strict.dir) != []
  then
    test.fail(
      "strict output wrote segments from an invalid state: #{
        segments(strict.dir)
      }"
    )
  end
  if
    strict.output.is_started()
  then
    test.fail(
      "strict output still started after an invalid state file"
    )
  end
  if
    file.contents("#{strict.dir}/state.json") !=
      '{"variants": [{"name": "a"'
  then
    test.fail(
      "strict output replaced the invalid state file with: #{
        file.contents("#{strict.dir}/state.json")
      }"
    )
  end
  if
    control.errors() != []
  then
    test.fail(
      "control output failed: #{control.errors()}"
    )
  end
  test.pass()
end

thread.run(check)
