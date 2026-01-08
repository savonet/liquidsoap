log.level := 4
log.file.path := "/tmp/bla.log"
log.stdout := true

s = sine()

s = mksafe(buffer(s))

recordings = ref([])

def shutdown_recordings() =
  log(
    "Stopping recordings"
  )

  list.iter(
    fun (recording) ->
      begin
        log(
          "Shutting down #{recording.id()}"
        )
        recording.shutdown()
      end,
    recordings()
  )

  recordings := []
end

def start_recording(path) =
  log(
    "Starting recording"
  )

  log(
    "Recording file: #{path}"
  )

  recording = output.file(%mp3, append=true, {path}, s)

  recordings := [recording, ...recordings()]
end

tmp = file.temp(cleanup=true, "bla", "mp3")

start_recording(tmp)

tmp2 = file.temp(cleanup=true, "bla", "mp3")

start_recording(tmp2)

thread.run(
  delay=1.,
  fun () ->
    begin
      shutdown_recordings()
      tmp = file.temp(cleanup=true, "bla", "mp3")
      start_recording(tmp)
      tmp2 = file.temp(cleanup=true, "bla", "mp3")
      start_recording(tmp2)
      let [o] = recordings()
      o.on_start(synchronous=true, test.pass)
    end
)
