%include "../test.liq"

# H.264 from Matroska is length-prefixed: MPEG-TS writes the stream's first
# parameter sets only, so a file whose header differs is refused when it opens.
playlist_file = file.temp("avcc-to-mpegts", ".m3u")
file.write(
  data="stored-header-first.mkv\nstored-header-other.mkv\nstored-header-third.mkv\n",
  playlist_file
)

s =
  playlist(
    mode="normal",
    loop=false,
    reload_mode="never",
    prefetch=1,
    playlist_file
  )

done = ref(false)

def on_close(filename) =
  if
    not done()
  then
    done := true
    let json.parse ({streams = [{nb_read_packets}]} :
      {streams: [{nb_read_packets: string}]}
    ) = test.ffprobe(count_packets=true, filename)
    packets = int_of_string(nb_read_packets)
    if
      packets < 170 or packets > 175
    then
      test.fail(
        "#{packets} packets copied, expected the first file (100) then the third \
         (75)"
      )
    end
    test.pass()
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mpegts", %video.copy),
  file.temp("avcc-to-mpegts", ".ts"),
  s
)
