%include "../test.liq"

# MPEG-TS and raw H.264 carry an Annex B H.264 stream's parameter sets in band:
# files of other sizes and profiles follow each other and the output decodes.
playlist_file = file.temp("changes-video", ".m3u")
file.write(
  data="changes-first.ts\nchanges-bigger.ts\nchanges-third.ts\n",
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

directory = file.temp_dir(cleanup=true, "changes-video")
checked = ref([])

def check(label, filename) =
  r =
    process.run(
      "ffmpeg -nostdin -v error -xerror -i #{process.quote(filename)} -f null -"
    )
  if
    r.status != "exit" or r.status.code != 0 or r.stderr != ""
  then
    test.fail(
      "#{label} does not decode: #{r.status} #{r.status.code} #{r.stderr}"
    )
  end
  let json.parse ({streams = [{nb_read_packets}]} :
    {streams: [{nb_read_packets: string}]}
  ) = test.ffprobe(count_packets=true, filename)
  packets = int_of_string(nb_read_packets)
  if
    packets < 240 or packets > 250
  then
    test.fail(
      "#{label}: #{packets} packets, expected the three files (100, 75 and 75)"
    )
  end
  count =
    atomic(
      fun () ->
        begin
          checked := [...checked(), label]
          list.length(checked())
        end
    )
  if count == 2 then test.pass() end
end

def output(label, format) =
  closed = ref(false)
  def on_close(filename) =
    if
      not closed()
    then
      closed := true
      check(label, filename)
    end
  end
  output.file(
    fallible=true,
    on_close=on_close,
    format,
    path.concat(directory, label),
    s
  )
end

output("out.ts", %ffmpeg(format = "mpegts", %video.copy))
output("out.h264", %ffmpeg(format = "h264", %video.copy))
