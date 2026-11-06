%include "../test.liq"

# serving.md: an output.harbor.hls with an explicit `id` serves its playlists
# and segments, looked up under that id.
port = 18763
base = "http://127.0.0.1:#{port}/radio"
dir = file.temp_dir("hls_harbor_id")
on_cleanup({file.rmdir(dir)})

s = sine()

o =
  output.harbor.hls(
    id="my_radio",
    port=port,
    path="radio",
    tmpdir=dir,
    segment_duration=1.,
    [("a", %mp3)],
    s
  )

checks = ref(0)

def expect_served(path) =
  r = http.get(timeout=5., "#{base}/#{path}")
  if
    r.status_code != 200
  then
    test.fail(
      "#{path}: status #{r.status_code}, expected 200"
    )
  end
  ref.incr(checks)
  r
end

started = ref(false)

def check() =
  if
    o.id() != "my_radio"
  then
    test.fail(
      "id is #{o.id()}"
    )
  end
  ignore(expect_served("stream.m3u8"))
  media = expect_served("a.m3u8")
  segments =
    list.filter(
      fun (l) -> l != "" and not string.contains(prefix="#", l),
      string.split(separator="\r\n", media)
    )
  list.iter(fun (seg) -> ignore(expect_served(seg)), segments)
  if
    checks() < 4
  then
    test.fail(
      "only #{checks()} files checked"
    )
  end
  test.pass()
end

o.on_file_change(
  synchronous=true,
  fun (e) ->
    if
      not started() and path.basename(e.path) == "a_3.mp3"
    then
      started := true
      thread.run(check)
    end
)
