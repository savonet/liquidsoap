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

# Sleep while the source is down, without persistence, leaves no file the
# output wrote and keeps the others.
dir = file.temp_dir("hls_sleep_no_persist")
on_cleanup({file.rmdir(dir)})
file.write(data="decoy", "#{dir}/decoy.mp3")

on = ref(true)
s = source.available(sine(), {on()})
o =
  output.file.hls(
    fallible=true,
    segment_duration=1.,
    segments=3,
    dir,
    [("a", %mp3), ("b", %fdkaac)],
    s
  )

clock.assign_new(sync="none", [o])

awake = ref(true)
o.on_shutdown(synchronous=true, {awake := false})
stopped = ref(0)
o.on_stop(synchronous=true, {stopped := stopped() + 1})
written = ref([])
o.on_file_change(
  synchronous=true,
  fun (e) -> if e.state != "deleted" then written := [...written(), e.path] end
)

def check() =
  wait_for(
    "a main playlist",
    {file.exists("#{dir}/stream.m3u8")}
  )
  wait_for(
    "8 segments",
    {file.exists("#{dir}/a_8.mp3") and file.exists("#{dir}/b_8.aac")}
  )
  on := false
  wait_for(
    "the output to stop",
    {stopped() > 0}
  )
  if
    file.ls(dir) == ["decoy.mp3"]
  then
    test.fail(
      "a non-final stop removed every file"
    )
  end
  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
  left = list.filter(fun (f) -> f != "decoy.mp3", file.ls(dir))
  if
    left != []
  then
    test.fail(
      "files left after sleep without persistence: #{left}"
    )
  end
  if
    not file.exists("#{dir}/decoy.mp3")
  then
    test.fail(
      "sleep removed a file the output did not write"
    )
  end
  if
    list.length(written()) < 10
  then
    test.fail(
      "only #{list.length(written())} files written"
    )
  end
  test.pass()
end

thread.run(check)
