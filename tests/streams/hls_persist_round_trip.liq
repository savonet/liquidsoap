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

dir = file.temp_dir("hls_persist_round_trip")
on_cleanup({file.rmdir(dir)})

names = ["a", "b", "c"]
s = sine()
o =
  output.file.hls(
    segment_duration=1.,
    segments=3,
    persist_at="state.json",
    dir,
    [("a", %mp3), ("b", %fdkaac), ("c", %shine)],
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

def check() =
  wait_for(
    "6 segments in every variant",
    {list.for_all(fun (r) -> snd(r).last_position() >= 6, recorders)}
  )

  clock(o.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
  if
    not file.exists("#{dir}/state.json")
  then
    test.fail(
      "no state file after sleep"
    )
  end

  # Before the restart: no discontinuity anywhere (KC8).
  def before(r) =
    let (name, r) = r
    list.iter(
      fun (p) ->
        begin
          if
            p.discontinuity_sequence != 0
          then
            test.fail(
              "#{name}: discontinuity sequence #{p.discontinuity_sequence} \
               before any restart"
            )
          end
          if
            list.exists(fun (seg) -> seg.discontinuous, p.segments)
          then
            test.fail(
              "#{name}: EXT-X-DISCONTINUITY before any restart"
            )
          end
        end,
      r.history()
    )
    p = hls_parse(file.contents("#{dir}/#{name}.m3u8"))
    (
      name,
      {
        last = p.media_sequence + list.length(p.segments),
        playlist = p,
        rewrites = list.length(r.history())
      }
    )
  end
  saved = list.map(before, recorders)

  restart_clock(o.clock, wake_ups)

  wait_for(
    "the window to scroll past the restart",
    {
      list.for_all(
        fun (r) ->
          begin
            let (name, r) = r
            r.last_position() >= list.assoc(name, saved).last + 6
          end,
        recorders
      )
    }
  )

  checks = ref(0)
  def after(r) =
    let (name, r) = r
    s = list.assoc(name, saved)
    history = r.history()
    new = list.filter(fun (x) -> fst(x) > s.last, r.segments())

    # Numbering continues from the saved state.
    if
      not list.assoc.mem(s.last + 1, new)
    then
      test.fail(
        "#{name}: no segment #{s.last + 1} after restart, got #{
          list.map(fst, new)
        }"
      )
    end
    first = list.assoc(s.last + 1, new)
    if
      not first.discontinuous
    then
      test.fail(
        "#{name}: first segment after restart (#{first.uri}) is not \
         discontinuous"
      )
    end
    list.iter(
      fun (x) ->
        if
          fst(x) > s.last + 1 and snd(x).discontinuous
        then
          test.fail(
            "#{name}: segment #{snd(x).uri} after the restart is also \
             discontinuous"
          )
        end,
      new
    )

    # The first playlist after the restore continues the window.
    restored = list.nth(history, s.rewrites)
    if
      restored.media_sequence < s.playlist.media_sequence
    then
      test.fail(
        "#{name}: media sequence went back from #{s.playlist.media_sequence} to #{
          restored.media_sequence
        }"
      )
    end
    first_restored =
      list.hd(
        default={uri = "", duration = 0., discontinuous = false, tags = []},
        restored.segments
      )
    if
      not list.exists(
        fun (seg) -> seg.uri == first_restored.uri,
        s.playlist.segments
      )
    then
      test.fail(
        "#{name}: first playlist after restore starts at #{
          first_restored.uri
        }, not a segment of the saved window"
      )
    end

    # Media and discontinuity sequences never go back; the first listed
    # segment never carries EXT-X-DISCONTINUITY.
    def monotonic(prev, p) =
      if
        p.media_sequence < prev.media_sequence
      then
        test.fail(
          "#{name}: media sequence went back #{prev.media_sequence} -> #{
            p.media_sequence
          }"
        )
      end
      if
        p.discontinuity_sequence < prev.discontinuity_sequence
      then
        test.fail(
          "#{name}: discontinuity sequence went back"
        )
      end
      first =
        list.hd(
          default={uri = "", duration = 0., discontinuous = false, tags = []},
          p.segments
        )
      if
        first.discontinuous
      then
        test.fail(
          "#{name}: first listed segment #{first.uri} carries \
           EXT-X-DISCONTINUITY"
        )
      end
      checks := checks() + 1
      p
    end
    ignore(list.fold(monotonic, list.hd(history), history))

    final = list.nth(history, list.length(history) - 1)
    if
      final.discontinuity_sequence != 1
    then
      test.fail(
        "#{name}: discontinuity sequence #{final.discontinuity_sequence} once \
         the window passed the restart, expected 1"
      )
    end
    if
      list.exists(fun (seg) -> seg.discontinuous, final.segments)
    then
      test.fail(
        "#{name}: discontinuity still listed after the window passed it"
      )
    end
    checks := checks() + 1
  end
  list.iter(after, recorders)

  if
    checks() < 10
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(check)
