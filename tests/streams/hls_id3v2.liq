tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

# Too flakey.
test.skip()

def run_check() =
  to_check =
    ref(
      {
        aac=null,
        shine=null,
        lame=null,
        fdkaac=null,
        ts_with_meta=null,
        ts=null,
        mp4=null
      }
    )

  def check_done() =
    print(
      "run check: #{to_check()}"
    )
    let {aac, shine, lame, fdkaac, ts_with_meta, ts, mp4} = to_check()

    if
      null.defined(ts)
    then
      test.fail(
        "ts shouldn't have metadata!"
      )
    end

    if
      null.defined(mp4)
    then
      test.fail(
        "mp4 should have metadata but it's not supported by the demuxer yet."
      )
    end

    if
      null.defined(ts_with_meta)
    then
      ts_with_meta = null.get(ts_with_meta)

      if

          ts_with_meta["title"] ==
            "test title"

      and
        ts_with_meta["album"] == "foolol"
      then
        test.pass()
      end
    end
  end

  ts_with_meta = input.hls("#{tmp_dir}/ts_with_meta.m3u8")
  ts_with_meta.on_metadata(
    fast=true,
    fun (m) ->
      begin
        print(
          "ts_with_meta #{m}"
        )
        if m["title"] != "" then to_check := to_check().{ts_with_meta=m} end
        check_done()
      end
  )

  output.dummy(fallible=true, ts_with_meta)
  ts = input.hls("#{tmp_dir}/ts.m3u8")
  ts.on_metadata(
    fast=true,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{ts=m} end
        check_done()
      end
  )

  output.dummy(fallible=true, ts)
  mp4 = input.hls("#{tmp_dir}/mp4.m3u8")
  mp4.on_metadata(
    fast=true,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{mp4=m} end
        check_done()
      end
  )

  output.dummy(fallible=true, mp4)
end

s = sine()
s =
  metadata.map(
    insert_missing=true,
    fun (_) ->
      [
        (
          "title",
          "test title"
        ),
        ("album", "foolol")
      ],
    s
  )

s = mksafe(s)

check_running = ref(false)
segments_created = ref(0)

def segment_name(metadata) =
  let {position, extname, stream_name} = metadata
  "segment-#{stream_name}_#{position}.#{extname}"
end

def on_file_change(~state, fname) =
  if
    state == "created"
  and
    string.contains(prefix="segment-", path.basename(fname))
  and
    string.contains(suffix="mp3", fname)
  then
    ref.incr(segments_created)
  end

  if
    segments_created() > 4 and not check_running()
  then
    check_running := true
    run_check()
  end
end

output.file.hls(
  segment_duration=2.,
  segment_name=segment_name,
  on_file_change=on_file_change,
  tmp_dir,
  [
    ("aac", %ffmpeg(format = "adts", %audio(codec = "aac")).{id3_version=3}),
    (
      "ts_with_meta",
      %ffmpeg(format = "mpegts", %audio(codec = "aac")).{id3_version=4}
    ),
    ("ts", %ffmpeg(format = "mpegts", %audio(codec = "aac")).{id3=false}),
    ("shine", %shine),
    ("lame", %mp3),
    ("fdkaac", %fdkaac),
    (
      "mp4",
      %ffmpeg(
        format = "mp4",
        frag_duration = 10,
        movflags = "+dash+skip_sidx+skip_trailer+frag_custom",
        %audio(codec = "aac")
      )
    )
  ],
  s
)
