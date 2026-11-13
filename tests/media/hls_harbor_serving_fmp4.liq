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

# output.harbor.hls with fMP4 variants (KC25): init segments are served as
# audio/mp4 or video/mp4 by their variant, media segments as
# video/iso.segment, playlists as application/vnd.apple.mpegurl.
settings.frame.video.width := 160
settings.frame.video.height := 120

port = 18742
base = "http://127.0.0.1:#{port}"
dir = file.temp_dir("hls_harbor_serving_fmp4")
on_cleanup({file.rmdir(dir)})

root = dir

# One output per variant kind, so that each is checked on its own.
def make(path, format, s) =
  dir = "#{root}/#{path}"
  o =
    output.harbor.hls(
      port=port,
      path=path,
      tmpdir=dir,
      segment_duration=1.,
      segments=3,
      [(path, format)],
      s
    )
  ignore(o)
  dir
end

video_dir =
  make(
    "video",
    %ffmpeg(
      format = "mp4",
      %audio(codec = "aac"),
      %video(codec = "libx264", preset = "ultrafast", g = 25)
    ),
    source.mux.video(video=noise(), sine())
  )
audio_dir =
  make("audio", %ffmpeg(format = "mp4", %audio(codec = "aac")), sine())

def header(r, name) =
  name = string.case(lower=true, name)
  list.filter_map(
    fun (h) -> string.case(lower=true, fst(h)) == name ? snd(h) : null,
    r.headers
  )
end

checks = ref(0)

def served(path, content_type) =
  r = http.get(normalize_url=false, timeout=5., "#{base}/#{path}")
  if
    r.status_code != 200
  then
    test.fail(
      "#{path}: status #{r.status_code}"
    )
  end
  if
    header(r, "Content-Type") != [content_type]
  then
    test.fail(
      "#{path}: content type #{header(r, "Content-Type")}, expected #{
        content_type
      }"
    )
  end
  checks := checks() + 1
  r
end

def lines(r) =
  string.split(separator="\r\n", r)
end

def check_variant(name, init_type) =
  ignore(served("#{name}/stream.m3u8", "application/vnd.apple.mpegurl"))
  p = served("#{name}/#{name}.m3u8", "application/vnd.apple.mpegurl")
  maps =
    list.filter_map(
      fun (l) ->
        begin
          m = r/^#EXT-X-MAP:URI="([^"]+)"$/.exec(l)
          m == [] ? null : (list.assoc(1, m))
        end,
      lines(p)
    )
  segments =
    list.filter(
      fun (l) -> l != "" and not string.contains(prefix="#", l),
      lines(p)
    )
  if
    maps == []
  then
    test.fail(
      "#{name}: no EXT-X-MAP in #{p}"
    )
  end
  if
    segments == []
  then
    test.fail(
      "#{name}: no segment in #{p}"
    )
  end
  list.iter(fun (u) -> ignore(served("#{name}/#{u}", init_type)), maps)
  list.iter(
    fun (u) -> ignore(served("#{name}/#{u}", "video/iso.segment")),
    segments
  )
end

def check() =
  wait_for(
    timeout=30.,
    "the video variant's main playlist and 3 segments",
    {
      file.exists("#{video_dir}/stream.m3u8")
      and file.exists("#{video_dir}/video_3.m4s")
    }
  )
  check_variant("video", "video/mp4")
  wait_for(
    timeout=10.,
    "the audio variant's main playlist and 3 segments",
    {
      file.exists("#{audio_dir}/stream.m3u8")
      and file.exists("#{audio_dir}/audio_3.m4s")
    }
  )
  check_variant("audio", "audio/mp4")
  if
    checks() < 10
  then
    test.fail(
      "only #{checks()} files checked"
    )
  end
  test.pass()
end

thread.run(check)
