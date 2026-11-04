%include "../test.liq"

# MPEG-TS carries codec headers in band, so its copied track takes a file whose
# H.264 header differs from the stream's: every file of the playlist plays.
playlist_file = file.temp("in-band-header", ".m3u")
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
      packets < 240 or packets > 250
    then
      test.fail(
        "#{packets} packets copied, expected the three files (100, 75 and 75), \
         less the few packets dropped before the first key frame"
      )
    end
    test.pass()
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "mpegts", %video.copy),
  file.temp("in-band-header", ".ts"),
  s
)
