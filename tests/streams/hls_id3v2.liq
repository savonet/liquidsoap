s = sine()
s = insert_metadata(s)

def f() =
  s.insert_metadata(
    [
      (
        "title",
        "test title"
      ),
      ("album", "foolol")
    ]
  )
end

thread.run(every=2., f)
s = mksafe(s)
tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})
output.file.hls(
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

to_check =
  ref(
    {
      aac=null(),
      shine=null(),
      lame=null(),
      fdkaac=null(),
      ts_with_meta=null(),
      ts=null(),
      mp4=null()
    }
  )

def check_done() =
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
    null.defined(aac) and null.defined(fdkaac) and null.defined(ts_with_meta)
  then
    aac = null.get(aac)
    fdkaac = null.get(fdkaac)
    ts_with_meta = null.get(ts_with_meta)

    if

        aac["title"] ==
          "test title"

    and
      aac["album"] == "foolol"
    and

        fdkaac["title"] ==
          "test title"

    and
      fdkaac["album"] == "foolol"
    and

        ts_with_meta["title"] ==
          "test title"

    and
      ts_with_meta["album"] == "foolol"
    then
      test.pass()
    end
  end
end

aac = input.hls("#{tmp_dir}/aac.m3u8")
aac =
  source.on_metadata(
    aac,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{aac=m} end
        check_done()
      end
  )

output.dummy(fallible=true, aac)

#< FFMPEG seems to be unable to parse id3v2 metadata
inside mp3 streams..
shine = input.hls("#{tmp_dir}/shine.m3u8")
shine =
source.on_metadata(
shine,
fun (m) ->
begin
if m["title"] != "" then to_check := to_check().{shine=m} end
check_done()
end
)

output.dummy(fallible=true, shine)

lame = input.hls("#{tmp_dir}/lame.m3u8")
lame =
source.on_metadata(
lame,
fun (m) ->
begin
if m["title"] != "" then to_check := to_check().{lame=m} end
check_done()
end
)

output.dummy(fallible=true, lame)
>#

fdkaac = input.hls("#{tmp_dir}/fdkaac.m3u8")
fdkaac =
  source.on_metadata(
    fdkaac,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{fdkaac=m} end
        check_done()
      end
  )

output.dummy(fallible=true, fdkaac)

ts_with_meta = input.hls("#{tmp_dir}/ts_with_meta.m3u8")
ts_with_meta =
  source.on_metadata(
    ts_with_meta,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{ts_with_meta=m} end
        check_done()
      end
  )

output.dummy(fallible=true, ts_with_meta)
ts = input.hls("#{tmp_dir}/ts.m3u8")
ts =
  source.on_metadata(
    ts,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{ts=m} end
        check_done()
      end
  )

output.dummy(fallible=true, ts)
mp4 = input.hls("#{tmp_dir}/mp4.m3u8")
mp4 =
  source.on_metadata(
    mp4,
    fun (m) ->
      begin
        if m["title"] != "" then to_check := to_check().{mp4=m} end
        check_done()
      end
  )

output.dummy(fallible=true, mp4)
clock.assign_new(sync="none", [s, aac, fdkaac, ts_with_meta, ts, mp4])
