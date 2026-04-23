# Regression test for https://github.com/savonet/liquidsoap/issues/5084
# HLS ID3 stream should contain metadata, not just the empty 10-byte ID3 header

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

first_segment = ref(null)

def segment_name(meta) =
  let {position, extname, stream_name} = meta
  "segment-#{stream_name}_#{position}.#{extname}"
end

def on_file_change({state, path = fname}) =
  if
    state == "created"
    and string.contains(prefix="segment-ts_with_meta", path.basename(fname))
    and null.defined(first_segment()) == false
  then
    first_segment := fname
    contents = file.contents(fname)
    if
      string.contains(
        encoding="ascii",
        substring="test title",
        contents
      )
    then
      test.pass()
    else
      test.fail()
    end
  end
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
        (
          "artist",
          "test artist"
        )
      ],
    s
  )

o =
  output.file.hls(
    fallible=true,
    segment_duration=2.,
    segment_name=segment_name,
    tmp_dir,
    [
      (
        "ts_with_meta",
        %ffmpeg(format = "mpegts", %audio(codec = "aac")).{id3_version = 4}
      )
    ],
    s
  )

o.on_file_change(synchronous=true, on_file_change)

thread.run(delay=30., {test.fail()})
