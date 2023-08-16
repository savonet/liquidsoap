log.level := 4

s = sine()

count = ref(0)

def on_close(filename) =
  count := count() + 1

  j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(filename)}")

  let json.parse ( parsed: {
    streams: [{
      codec_name: string
   }]
  }) = j

  if (list.hd(parsed.streams).codec_name != "flac") then
     test.fail()
  end

  if count() == 3 then
    test.pass()
  end
end

dir = file.temp_dir("flac")

on_cleanup({file.rmdir(dir)})

last_time = ref(time())

def reopen_when() =
  current_time = time()
  time_diff = current_time - last_time()
  last_time := current_time
  2. < time_diff
end

output.file(%flac,
  reopen_delay=2.,
  reopen_when=reopen_when,
  on_close=on_close,
  {time.string("#{dir}/%Hh%M_%S.flac")},
  s
)
