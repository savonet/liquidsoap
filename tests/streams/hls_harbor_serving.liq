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

# output.harbor.hls (KC25): what is served, with which content type, cache
# headers and configured headers; what is never served; restored files are
# served after a restore.
port = 18741
base = "http://127.0.0.1:#{port}"
parent = file.temp_dir("hls_harbor_serving")
on_cleanup({file.rmdir(parent)})
dir = "#{parent}/live"
cc_dir = "#{parent}/cc"
file.mkdir(dir)
file.mkdir(cc_dir)
file.write(data="secret", "#{parent}/secret.txt")
file.write(data="0123456789abcdef", "#{dir}/key.bin")
file.write(data="partial", "#{dir}/.hls-tmp-decoy.mp3")

live =
  output.harbor.hls(
    port=port,
    path="live",
    tmpdir=dir,
    headers=[("Access-Control-Allow-Origin", "*"), ("X-Test", "yes")],
    segment_duration=2.,
    segments=3,
    persist_at="state.json",
    [("a", %mp3), ("b", %fdkaac)],
    sine()
  )

awake = ref(true)
live.on_shutdown(synchronous=true, {awake := false})
wake_ups = ref(0)
live.on_wake_up(
  synchronous=true,
  {
    awake := true
    wake_ups := wake_ups() + 1
  }
)
rewrites = ref(0)
live.on_file_change(
  synchronous=true,
  fun (e) -> if e.path == "#{dir}/a.m3u8" then rewrites := rewrites() + 1 end
)

cc =
  output.harbor.hls(
    port=port,
    path="/cc/",
    tmpdir=cc_dir,
    headers=[("Cache-Control", "no-store")],
    segment_duration=1.,
    segments=3,
    [("a", %mp3)],
    sine()
  )

def header(r, name) =
  name = string.case(lower=true, name)
  values =
    list.filter_map(
      fun (h) -> string.case(lower=true, fst(h)) == name ? snd(h) : null,
      r.headers
    )
  values
end

def get(path) =
  http.get(normalize_url=false, timeout=5., "#{base}#{path}")
end

# A request sent as is, without the client-side normalisation of dot
# segments that http.get does: its status and headers.
def raw(path) =
  r =
    process.run(
      timeout=10.,
      "curl -s -D - -o /dev/null --path-as-is #{process.quote(base ^ path)}"
    )
  lines = string.split(separator="\r\n", r.stdout)
  status = list.hd(default="", lines)
  status =
    string.split(
      separator=" ",
      status
    )
  status = int_of_string(default=0, list.nth(default="0", status, 1))
  headers =
    list.filter_map(
      fun (l) ->
        begin
          m = r/^([^:]+): *(.*)$/.exec(l)
          m == [] ? null : (list.assoc(1, m), list.assoc(2, m))
        end,
      list.tl(lines)
    )
  {status_code = status, headers = headers}
end

checks = ref(0)

def expect(what, cond) =
  if not cond then test.fail(what) end
  checks := checks() + 1
end

def expect_404(path) =
  r = raw(path)
  expect(
    "#{path}: status #{r.status_code}, expected 404",
    r.status_code == 404
  )
  expect(
    "#{path}: 404 without the configured headers: #{r.headers}",
    header(r, "X-Test") == ["yes"]
    and header(r, "Access-Control-Allow-Origin") == ["*"]
  )
end

def check_served(path, content_type) =
  r = get(path)
  expect(
    "#{path}: status #{r.status_code}",
    r.status_code == 200
  )
  expect(
    "#{path}: content type #{header(r, "Content-Type")}, expected #{
      content_type
    }",
    header(r, "Content-Type") == [content_type]
  )
  expect(
    "#{path}: configured header missing",
    header(r, "X-Test") == ["yes"]
    and header(r, "Access-Control-Allow-Origin") == ["*"]
  )
  h = http.head(normalize_url=false, timeout=5., "#{base}#{path}")
  expect(
    "#{path}: HEAD status #{h.status_code}",
    h.status_code == 200
  )
  expect(
    "#{path}: HEAD content type #{header(h, "Content-Type")}",
    header(h, "Content-Type") == [content_type]
  )
  expect(
    "#{path}: HEAD content length #{header(h, "Content-Length")} for #{
      string.length(encoding="ascii", r)
    } bytes",
    header(h, "Content-Length") == ["#{string.length(encoding="ascii", r)}"]
  )
  r
end

def check_playlist(path) =
  r = check_served(path, "application/vnd.apple.mpegurl")
  expect(
    "#{path}: playlist cache #{header(r, "Cache-Control")}, expected max-age=1",
    header(r, "Cache-Control") == ["max-age=1"]
  )
  r
end

def check_segment(path, content_type) =
  r = check_served(path, content_type)
  cache = header(r, "Cache-Control")
  expect(
    "#{path}: segment cache #{cache}",
    list.length(cache) == 1
    and string.contains(prefix="max-age=", list.hd(cache))
    and list.hd(cache) != "max-age=0"
  )
  expect(
    "#{path}: no Expires",
    list.length(header(r, "Expires")) == 1
  )
end

def check_variant(name, content_type) =
  p = hls_parse(check_playlist("/live/#{name}.m3u8"))
  expect(
    "#{name}: empty playlist",
    p.segments != []
  )
  list.iter(
    fun (seg) -> check_segment("/live/#{seg.uri}", content_type),
    p.segments
  )
  p
end

def check() =
  wait_for(
    timeout=30.,
    "a main playlist and 3 segments",
    {
      file.exists("#{dir}/stream.m3u8")
      and file.exists("#{dir}/a_3.mp3")
      and file.exists("#{dir}/b_3.aac")
    }
  )

  main = check_playlist("/live/stream.m3u8")
  uris =
    list.filter(
      fun (l) -> l != "" and not string.contains(prefix="#", l),
      string.split(separator="\r\n", main)
    )
  expect(
    "main playlist names #{uris}",
    list.length(uris) == 2
    and list.mem("a.m3u8", uris)
    and list.mem("b.m3u8", uris)
  )
  ignore(check_variant("a", "audio/mpeg"))
  ignore(check_variant("b", "audio/aac"))

  key = get("/live/key.bin")
  expect(
    "key file: status #{key.status_code}, body #{string.quote(key)}",
    key.status_code == 200 and key == "0123456789abcdef"
  )

  expect_404("/live/..%2Fsecret.txt")
  expect_404("/live/%2e%2e/secret.txt")
  expect_404("/live/%2E%2E%2Fsecret.txt")
  expect_404("/live/key.bin%00.txt")
  expect_404("/live/.hls-tmp-decoy.mp3")
  expect_404("/live/state.json")
  expect_404("/live/missing.mp3")
  expect_404("/live/")
  expect(
    "a path outside the base is served",
    raw("/elsewhere/a.m3u8").status_code == 404
  )
  expect_404("/live/../secret.txt")
  expect_404("/live/x/../../secret.txt")
  expect_404("/live/..")

  # A configured Cache-Control replaces the generated one.
  wait_for(
    "a cc playlist",
    {file.exists("#{cc_dir}/a.m3u8")}
  )
  cc_segment =
    list.hd(hls_parse(file.contents("#{cc_dir}/a.m3u8")).segments).uri
  list.iter(
    fun (path) ->
      begin
        r = get(path)
        expect(
          "#{path}: status #{r.status_code}",
          r.status_code == 200
        )
        expect(
          "#{path}: Cache-Control #{header(r, "Cache-Control")}, expected only \
           the configured one",
          header(r, "Cache-Control") == ["no-store"]
        )
      end,
    ["/cc/a.m3u8", "/cc/#{cc_segment}"]
  )

  # After a restore, restored segments and rewritten playlists are served.
  before = hls_parse(file.contents("#{dir}/a.m3u8"))
  clock(live.clock).stop()
  wait_for(
    "the output to sleep",
    {not awake()}
  )
  rewrites := 0
  restart_clock(live.clock, wake_ups)
  wait_for(
    "the playlist rewritten after the restore",
    {rewrites() > 0}
  )
  after = check_variant("a", "audio/mpeg")
  restored =
    list.filter(
      fun (seg) ->
        list.exists(fun (old) -> old.uri == seg.uri, before.segments),
      after.segments
    )
  expect(
    "no restored segment listed after the restore: before #{before.segments}, \
     after #{after.segments}",
    restored != []
  )

  if
    checks() < 60
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(check)
