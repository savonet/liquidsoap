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

# serving.md §3: request paths are decoded with `+` kept and compared in
# canonical form, so a `+` in a name is served and the state file is a 404 in
# any spelling.
port = 18767
base = "http://127.0.0.1:#{port}/live"
dir = file.temp_dir("hls_harbor_paths")
on_cleanup({file.rmdir(dir)})

o =
  output.harbor.hls(
    port=port,
    path="live",
    tmpdir=dir,
    persist_at="state.json",
    segment_duration=1.,
    [("a+b", %mp3)],
    sine()
  )

clock.assign_new(sync="none", [o])

def status(path) =
  r = http.get("#{base}/#{path}")
  r.status_code
end

def check() =
  wait_for(
    "a segment and the state file",
    {file.exists("#{dir}/a+b_2.mp3") and file.exists("#{dir}/state.json")}
  )
  checks = ref(0)
  def expect(path, code) =
    checks := checks() + 1
    got = status(path)
    if
      got != code
    then
      test.fail(
        "#{path}: status #{got}, expected #{code}"
      )
    end
  end

  expect("a+b.m3u8", 200)
  expect("a+b_2.mp3", 200)
  expect("a%2Bb_2.mp3", 200)
  expect("state.json", 404)
  expect("./state.json", 404)
  expect(".//state.json", 404)
  expect("%2E/state.json", 404)
  expect("bad%zzescape", 404)
  if
    checks() != 8
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(check)
