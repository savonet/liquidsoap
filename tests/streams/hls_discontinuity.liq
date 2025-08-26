tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

s = sine()

is_on = ref(true)

s = switch(track_sensitive=false, [(is_on, s)])

clock.assign_new(sync="none", [s])

enc = %ffmpeg(format = "mpegts", %audio(codec = "aac"))

streams = [("stream", enc)]

has_restarted = ref(false)
segments_count = ref(-1)

first_playlist =
  "#EXTM3U\r\n#EXT-X-TARGETDURATION:2\r\n#EXT-X-VERSION:3\r\n#EXT-X-MEDIA-SEQUENCE:0\r\n#EXT-X-DISCONTINUITY-SEQUENCE:0\r\n#EXTINF:2.000,\r\nstream_1.ts\r\n#EXTINF:1.980,\r\nstream_2.ts\r\n#EXTINF:1.980,\r\nstream_3.ts\r\n#EXTINF:1.960,\r\nstream_4.ts\r\n"

second_playlist =
  "#EXTM3U\r\n#EXT-X-TARGETDURATION:2\r\n#EXT-X-VERSION:3\r\n#EXT-X-MEDIA-SEQUENCE:2\r\n#EXT-X-DISCONTINUITY-SEQUENCE:0\r\n#EXTINF:1.980,\r\nstream_3.ts\r\n#EXTINF:1.960,\r\nstream_4.ts\r\n#EXTINF:0.040,\r\nstream_5.ts\r\n#EXT-X-DISCONTINUITY\r\n#EXTINF:2.000,\r\nstream_7.ts\r\n"

third_playlist =
  "#EXTM3U\r\n#EXT-X-TARGETDURATION:2\r\n#EXT-X-VERSION:3\r\n#EXT-X-MEDIA-SEQUENCE:6\r\n#EXT-X-DISCONTINUITY-SEQUENCE:1\r\n#EXTINF:2.000,\r\nstream_7.ts\r\n#EXTINF:1.980,\r\nstream_8.ts\r\n#EXTINF:1.980,\r\nstream_9.ts\r\n#EXTINF:1.960,\r\nstream_10.ts\r\n"

def on_file_change({path = fname}) =
  if
    path.basename(fname) == "stream.m3u8"
  then
    ref.incr(segments_count)
    if
      not has_restarted() and segments_count() == 4
    then
      test.equal(file.contents(fname), first_playlist)
      is_on := false
      thread.run(
        delay=0.1,
        {
          has_restarted := true
          segments_count := 0
          is_on := true
        }
      )
    elsif
      has_restarted() and segments_count() == 1
    then
      test.equal(file.contents(fname), second_playlist)
    elsif
      has_restarted() and segments_count() == 4
    then
      test.equal(file.contents(fname), third_playlist)
      test.pass()
    end
  end
end

o =
  output.file.hls(
    fallible=true,
    persist_at="./hls_discontinuity.json",
    segments=4,
    segment_duration=2.,
    tmp_dir,
    streams,
    s
  )

o.on_file_change(synchronous=true, on_file_change)
