s = sine()

s = insert_metadata(s)

def f() =
  s.insert_metadata([("title", "test title"), ("album","foolol")])
end

thread.run(every=2., f)

s = mksafe(s)

tmp_dir = file.temp_dir("tmp")
on_shutdown({file.rmdir(tmp_dir)})

output.file.hls(
  tmp_dir,
  [
   ("aac", %ffmpeg(format="adts",%audio(codec="aac")).{id3_version = 3}),
   ("ts_with_meta", %ffmpeg(format="mpegts",%audio(codec="aac")).{id3 = true, id3_version = 4}),
   ("ts", %ffmpeg(format="mpegts",%audio(codec="aac")))
  ],
  s
)

to_check = ref({
  aac = null(),
  ts_with_meta = null(),
  ts = null()
})

def check_done() =
  let { aac, ts_with_meta, ts } = to_check()

  if null.defined(ts) then test.fail("ts shouldn't have metadata!") end

  if null.defined(aac) and null.defined(ts_with_meta) then
    aac = null.get(aac)
    ts_with_meta = null.get(ts_with_meta)
    if aac["title"] == "test title" and ts_with_meta["title"] == "test title" and
       aac["album"] == "foolol" and ts_with_meta["album"] == "foolol" then
      test.pass()
    end
  end
end

aac = input.hls("#{tmp_dir}/aac.m3u8")

aac = source.on_metadata(aac, fun (m) -> begin
  if m["title"] != "" then
    to_check := to_check().{ aac = m }
  end
  check_done()
end)

output.dummy(fallible=true, aac)

ts_with_meta = input.hls("#{tmp_dir}/ts_with_meta.m3u8")

ts_with_meta = source.on_metadata(ts_with_meta, fun (m) -> begin
  if m["title"] != "" then
    to_check := to_check().{ ts_with_meta = m }
  end
  check_done()
end)

output.dummy(fallible=true, ts_with_meta)

ts = input.hls("#{tmp_dir}/ts.m3u8")

ts = source.on_metadata(ts, fun (m) -> begin
  if m["title"] != "" then
    to_check := to_check().{ ts = m }
  end
  check_done()
end)

output.dummy(fallible=true, ts)

clock.assign_new(sync="none",[s, aac, ts_with_meta, ts])
