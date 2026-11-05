%include "../test.liq"

# The first file a copied track opens commits the stream's parameters, header
# details included: a later file whose H.264 header differs is refused when it
# opens and the playlist moves on to the next one.
playlist_file = file.temp("stored-header", ".m3u")
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

# The check runs ffprobe: a thread of its own runs it, so that the output
# callback does not wait on a process.
def check(filename) =
  let json.parse ({streams = [{nb_read_packets}]} :
    {streams: [{nb_read_packets: string}]}
  ) = test.ffprobe(count_packets=true, filename)
  packets = int_of_string(nb_read_packets)
  if
    packets < 170 or packets > 175
  then
    test.fail(
      "#{packets} packets copied, expected the first file (100) then the third \
       (75), less the few packets dropped before the first key frame"
    )
  end
  test.pass()
end

def on_close(filename) =
  if
    not done()
  then
    done := true
    thread.run(fun () -> check(filename))
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "matroska", %video.copy),
  file.temp("stored-header", ".mkv"),
  s
)
