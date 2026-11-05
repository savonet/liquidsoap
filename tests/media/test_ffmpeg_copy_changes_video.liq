%include "../test.liq"

# MPEG-TS and raw H.264 or HEVC carry an Annex B stream's parameter sets in
# band: files of other sizes and profiles follow each other and the output
# decodes.
def files(name, list) =
  f = file.temp(name, ".m3u")
  file.write(data=string.concat(separator="\n", list) ^ "\n", f)
  playlist(mode="normal", loop=false, reload_mode="never", prefetch=1, f)
end

def h264() =
  files(
    "changes-h264",
    ["changes-first.ts", "changes-bigger.ts", "changes-third.ts"]
  )
end

def hevc() =
  files(
    "changes-hevc",
    ["changes-hevc-first.ts", "changes-hevc-bigger.ts", "changes-hevc-third.ts"]
  )
end

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
    packets < 220 or packets > 250
  then
    test.fail(
      "#{label}: #{packets} packets, expected the three files"
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
  if count == 4 then test.pass() end
end

def output(label, format, s) =
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

output("h264.ts", %ffmpeg(format = "mpegts", %video.copy), h264())
output("out.h264", %ffmpeg(format = "h264", %video.copy), h264())
output("hevc.ts", %ffmpeg(format = "mpegts", %video.copy), hevc())
output("out.hevc", %ffmpeg(format = "hevc", %video.copy), hevc())
