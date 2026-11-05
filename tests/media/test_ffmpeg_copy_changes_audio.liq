%include "../test.liq"

# ADTS carries an AAC stream's configuration in every frame header, and so does
# MP3: files of other sample rates and channel counts follow each other and the
# outputs decode.
def files(name, list) =
  f = file.temp(name, ".m3u")
  file.write(data=string.concat(separator="\n", list) ^ "\n", f)
  playlist(mode="normal", loop=false, reload_mode="never", prefetch=1, f)
end

aac =
  files(
    "changes-aac",
    ["changes-first.aac", "changes-other.aac", "changes-third.aac"]
  )

mp3 =
  files(
    "changes-mp3",
    ["changes-first.mp3", "changes-other.mp3", "changes-third.mp3"]
  )

directory = file.temp_dir(cleanup=true, "changes-audio")
closed_files = ref([])

def check(label, rates, filename) =
  file = process.quote(filename)
  r =
    process.run(
      "ffmpeg -nostdin -v error -xerror -i #{file} -f null -"
    )
  if
    r.status != "exit" or r.status.code != 0 or r.stderr != ""
  then
    test.fail(
      "#{label} does not decode: #{r.status} #{r.status.code} #{r.stderr}"
    )
  end
  decoded =
    process.read(
      "ffmpeg -nostdin -v info -i #{file} -af ashowinfo -f null - 2>&1 | grep -o \
       'rate:[0-9]* ' | sort -u"
    )
  list.iter(
    fun (rate) ->
      if
        not string.contains(
          substring="rate:#{rate} ",
          decoded
        )
      then
        test.fail(
          "#{label}: no audio decoded at #{rate} Hz, decoded: #{decoded}"
        )
      end,
    rates
  )
end

# The checks run ffmpeg: one thread runs them in turn once every output has
# closed, so that no output callback waits on a process.
def record(closed_file) =
  count =
    atomic(
      fun () ->
        begin
          closed_files := [...closed_files(), closed_file]
          list.length(closed_files())
        end
    )
  if
    count == 3
  then
    thread.run(
      fun () ->
        begin
          list.iter(
            fun (closed_file) ->
              begin
                let (label, rates, filename) = closed_file
                check(label, rates, filename)
              end,
            closed_files()
          )
          test.pass()
        end
    )
  end
end

def output(label, rates, format, s) =
  closed = ref(false)
  def on_close(filename) =
    if
      not closed()
    then
      closed := true
      record((label, rates, filename))
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

output("out.ts", [44100, 48000], %ffmpeg(format = "mpegts", %audio.copy), aac)
output(
  "out.aac",
  [44100, 48000],
  %ffmpeg(format = "adts", %audio.copy),
  files(
    "changes-adts",
    ["changes-first.aac", "changes-other.aac", "changes-third.aac"]
  )
)
output("out.mp3", [44100, 22050], %ffmpeg(format = "mp3", %audio.copy), mp3)
