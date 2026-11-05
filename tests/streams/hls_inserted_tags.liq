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

# Inserted tags (KC20): a tag inserted once appears exactly once, before the
# EXTINF of the first segment opened after it, in that variant only, in
# insertion order. A tag that is not one playlist line is rejected.
dir = file.temp_dir("hls_inserted_tags")
on_cleanup({file.rmdir(dir)})

names = ["a", "b"]
s = sine()
o =
  output.file.hls(
    segment_duration=1.,
    segments=3,
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
ra = list.assoc("a", recorders)
rb = list.assoc("b", recorders)

entry_a = list.find(fun (v) -> v.name == "a", o.streams())

# Inserted while segment a_3 is being published, before a_4 opens.
o.on_file_change(
  synchronous=true,
  fun (e) ->
    if
      e.state == "created" and e.path == "#{dir}/a_3.mp3"
    then
      entry_a.insert_tag("#EXT-X-ONE")
      entry_a.insert_tag(
        "  #EXT-X-TWO:2  "
      )
      entry_a.insert_tag("#EXT-X-THREE")
    end
)

# A tag that is not one playlist line is an invalid-value error. The error
# cannot be caught by the script, so each runs in its own process.
def check_rejected() =
  def run(name, call) =
    script = path.concat(dir, "reject_#{name}.liq")
    file.write(
      data="o = output.file.hls(#{string.quote(dir)} ^ \"/reject_#{name}\", \
       [(\"a\", %mp3)], sine())\n#{call}\nprint(\"accepted\")\nexit(0)\n",
      script
    )
    r =
      process.run(
        timeout=120.,
        "liquidsoap #{process.quote(script)}"
      )
    if
      r.status.code != 1
      or not string.contains(
        substring="Not a playlist tag",
        r.stderr
      )
    then
      test.fail(
        "#{name}: tag not rejected, exit code #{r.status.code}\n#{r.stdout}\n#{
          r.stderr
        }"
      )
    end
  end
  run("newline", "o.insert_tag(\"#EXT-X-BAD-ONE\\n#EXT-X-BAD-TWO\")")
  run("crlf", "(list.hd(o.streams())).insert_tag(\"#EXT-X-BAD\\r\\nbad.mp3\")")
  run("not_a_tag", "o.insert_tag(\"EXT-X-BAD\")")
end

def check() =
  wait_for(
    "10 segments in every variant",
    {ra.last_position() >= 10 and rb.last_position() >= 10}
  )

  check_rejected()

  tagged = list.filter(fun (x) -> snd(x).tags != [], ra.segments())
  if
    list.length(tagged) != 1
  then
    test.fail(
      "a: expected one tagged segment, got #{
        list.map(fun (x) -> (fst(x), snd(x).tags), tagged)
      }"
    )
  end
  let (pos, seg) = list.hd(tagged)
  if
    pos != 4
  then
    test.fail(
      "a: tags on segment #{pos}, expected 4, the first opened after insertion"
    )
  end
  if
    seg.tags != ["#EXT-X-ONE", "#EXT-X-TWO:2", "#EXT-X-THREE"]
  then
    test.fail(
      "a: tags of segment 4: #{seg.tags}"
    )
  end

  # Every listing of a_4 carries them, and no other line does.
  def check_listing(p) =
    list.iteri(
      fun (i, seg) ->
        begin
          pos = p.media_sequence + 1 + i
          if
            pos == 4
            and seg.tags != ["#EXT-X-ONE", "#EXT-X-TWO:2", "#EXT-X-THREE"]
          then
            test.fail(
              "a: a listing of segment 4 has tags #{seg.tags}"
            )
          end
          if
            pos != 4 and seg.tags != []
          then
            test.fail(
              "a: segment #{pos} has tags #{seg.tags}"
            )
          end
        end,
      p.segments
    )
  end
  list.iter(check_listing, ra.history())

  list.iter(
    fun (x) ->
      if
        snd(x).tags != []
      then
        test.fail(
          "b: segment #{fst(x)} has tags #{snd(x).tags} inserted on a"
        )
      end,
    rb.segments()
  )

  test.pass()
end

thread.run(check)
