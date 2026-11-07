%include "../test.liq"

# playlists.md §1, §2, §3, §4; known-complexity.md KC7, KC10, KC14: at every
# file-change event, every URI of every published playlist names an existing
# file, segment names creating new sub-directories mid-stream included.
dir = file.temp_dir("hls_playlist_uris")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
variants = ["a", "b", "c"]
segments_wanted = 25

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

def segment_name(segment) =
  "#{segment.stream_name}/d#{segment.position / 4}/seg #{segment.position}+.#{
    segment.extname
  }"
end

def lines_of(file_path) =
  content = file.contents(file_path)
  if
    not string.contains(suffix="\r\n", content)
  then
    test.fail(
      "#{file_path} does not end with CR LF"
    )
  end
  if
    list.length(string.split(separator="\n", content)) !=
      list.length(string.split(separator="\r\n", content))
  then
    test.fail(
      "#{file_path} has a line not ending with CR LF"
    )
  end
  list.filter(fun (l) -> l != "", string.split(separator="\r\n", content))
end

def tag_value(lines, tag) =
  prefix = "#{tag}:"
  l = list.find(default="", fun (l) -> string.contains(prefix=prefix, l), lines)
  if
    l == ""
  then
    test.fail(
      "missing #{tag} in #{lines}"
    )
  end
  string.sub(
    l,
    start=string.length(prefix),
    length=string.length(l) - string.length(prefix)
  )
end

def uris_of(lines) =
  list.filter(fun (l) -> not string.contains(prefix="#", l), lines)
end

def durations_of(lines) =
  list.filter_map(
    fun (l) ->
      if
        string.contains(prefix="#EXTINF:", l)
      then
        m = r/^#EXTINF:([0-9.]+),$/.exec(l)
        float_of_string(m[1])
      else
        null
      end,
    lines
  )
end

uri_checks = ref(0)
window_checks = ref(0)
main_checks = ref(0)
last_mseq = ref([])
listed_ever = ref([])
all_media_seen = ref(false)

def check_uri_exists(playlist_name, uri) =
  if
    string.contains(prefix="/", uri) or string.contains(substring="://", uri)
  then
    test.fail(
      "#{playlist_name} names a non-relative URI: #{uri}"
    )
  end
  target_path = "#{out}/#{url.decode(plus=false, uri)}"
  if
    not file.exists(target_path)
  then
    test.fail(
      "#{playlist_name} names #{uri} but #{target_path} does not exist"
    )
  end
  uri_checks := uri_checks() + 1
end

def check_media(v) =
  target_path = "#{out}/#{v}.m3u8"
  if
    file.exists(target_path)
  then
    lines = lines_of(target_path)
    if
      list.hd(default="", lines) != "#EXTM3U"
    then
      test.fail(
        "#{target_path} does not start with #EXTM3U"
      )
    end
    version = tag_value(lines, "#EXT-X-VERSION")
    if
      version != "6"
    then
      test.fail(
        "#{target_path} announces version #{version}"
      )
    end
    target = int_of_string(tag_value(lines, "#EXT-X-TARGETDURATION"))
    mseq = int_of_string(tag_value(lines, "#EXT-X-MEDIA-SEQUENCE"))
    uris = uris_of(lines)
    list.iter(fun (uri) -> check_uri_exists(target_path, uri), uris)
    previous = list.assoc(default=0, v, last_mseq())
    if
      mseq < previous
    then
      test.fail(
        "#{target_path}: media sequence went back from #{previous} to #{mseq}"
      )
    end
    last_mseq := list.assoc.remove(v, last_mseq())
    last_mseq := (v, mseq)::last_mseq()
    ever = list.assoc(default=[], v, listed_ever())
    ever =
      list.fold(
        fun (ever, uri) -> list.mem(uri, ever) ? ever : [...ever, uri],
        ever,
        uris
      )
    listed_ever := list.assoc.remove(v, listed_ever())
    listed_ever := (v, ever)::listed_ever()
    left = list.filter(fun (uri) -> not list.mem(uri, uris), ever)
    if
      mseq != list.length(left)
    then
      test.fail(
        "#{target_path}: media sequence #{mseq} but #{list.length(left)} \
         segments left the window"
      )
    end
    if
      left != []
    then
      window = list.fold(fun (x, y) -> x + y, 0., durations_of(lines))
      if
        window < 3. * float(target) - 0.0005
      then
        test.fail(
          "#{target_path}: window of #{window}s is under three target durations \
           (#{target})"
        )
      end
      window_checks := window_checks() + 1
    end
  end
end

def check_main() =
  target_path = "#{out}/stream.m3u8"
  if
    file.exists(target_path)
  then
    lines = lines_of(target_path)
    uris = uris_of(lines)
    if
      list.length(uris) != list.length(variants)
    then
      test.fail(
        "#{target_path} names #{uris}"
      )
    end
    list.iter(fun (uri) -> check_uri_exists(target_path, uri), uris)
    main_checks := main_checks() + 1
  elsif
    all_media_seen()
  then
    test.fail(
      "every variant has a media playlist but #{target_path} was not written"
    )
  end
end

def check_all(_) =
  if
    not test.done()
  then
    list.iter(check_media, variants)
    check_main()
    all_media_seen :=
      list.for_all(fun (v) -> file.exists("#{out}/#{v}.m3u8"), variants)
  end
end

def on_done(event) =
  if
    event.state == "created"
    and string.contains(
      suffix="seg #{segments_wanted}+.mp3",
      event.path
    )
  then
    if
      uri_checks() == 0 or window_checks() == 0 or main_checks() == 0
    then
      test.fail(
        "too few checks: #{uri_checks()} URIs, #{window_checks()} windows, #{
          main_checks()
        } main playlists"
      )
    end
    print(
      "#{uri_checks()} URIs, #{window_checks()} windows, #{main_checks()} main \
       playlists checked"
    )
    test.pass()
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segments=3,
    segments_overhead=1,
    segment_name=segment_name,
    out,
    [
      ("a", %mp3),
      ("b", %shine(bitrate = 64)),
      ("c", %fdkaac(bitrate = 64, transmux = "adts"))
    ],
    s
  )

o.on_file_change(synchronous=true, check_all)
o.on_file_change(synchronous=true, on_done)
