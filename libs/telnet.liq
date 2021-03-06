%ifdef input.gstreamer.audio_video
# @docof input.gstreamer.audio_video
def input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video)) =
  s = input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video))

  server.register(namespace=s.id(), description="Set gstreamer pipeline state to play",
                  "play", fun (_) -> try
                    s.play()
                    "Done."
                  catch err do
                    "Error while setting state to play: #{err}"
                  end)

  server.register(namespace=s.id(), description="Set gstreamer pipeline state to paused",
                  "pause", fun (_) -> try
                    s.pause()
                    "Done."
                  catch err do
                    "Error while setting state to paused: #{err}"
                  end)

  server.register(namespace=s.id(), description="Restart gstreamer pipeline",
                  "restart", fun (_) -> try
                    s.restart()
                    "Done."
                  catch err do
                    "Error while restarting pipeline: #{err}"
                  end)

  s
end
%endif

# @docof blank.strip
def blank.strip(%argsof(blank.strip), s) =
  s = blank.strip(%argsof(blank.strip), s)

  server.register(namespace=s.id(), description="Check if the source is stripping.",
                  "is_stripping", fun (_) -> begin
                    "#{s.is_blank()}"
                  end)

  s
end

# @docof output.external
def output.external(%argsof(output.external), f, p, s) =
  s = output.external(%argsof(output.external), f, p, s)

  server.register(namespace=s.id(), description="Re-open the output.",
                  "reopen", fun (_) -> begin
                    s.reopen()
                    "Done."
                  end)

  s
end

# @docof input.external.avi
def input.external.avi(%argsof(input.external.avi), s) =
  s = input.external.avi(%argsof(input.external.avi), s)

  server.register(namespace=s.id(), description="Show internal buffer length (in seconds).",
                  "buffer_length", fun (_) -> begin
                    let (audio, video, total) = s.buffer_length()
                    "audio buffer length: #{audio}\nvideo buffer length: #{video}\ntotal buffer length: #{total}"
                  end)

  s
end

# @docof input.harbor
def input.harbor(%argsof(input.harbor), s) =
  s = input.harbor(%argsof(input.harbor), s)

  server.register(namespace=s.id(), description="Stop current source client, if connected.",
                  "stop", fun (_) -> begin
                    s.stop()
                    "Done"
                  end)

  server.register(namespace=s.id(), description="Display current status.",
                  "status", fun (_) -> begin
                    s.status()
                  end)

  server.register(namespace=s.id(), description="Get the buffer's length, in seconds.",
                  "buffer_length", fun (_) -> begin
                    "#{s.buffer_length()}"
                  end)

  s
end

%ifdef input.http
# @docof input.http
def input.http(%argsof(input.http), s) =
  s = input.http(%argsof(input.http), s)

  server.register(namespace=s.id(), description="Start the source, if needed.",
                  "start", fun (_) -> begin
                    s.start()
                    "Done!"
                  end)

  server.register(namespace=s.id(), description="Stop the source if connected.",
                  "stop", fun (_) -> begin
                    s.stop()
                    "Done!"
                  end)

  server.register(namespace=s.id(), description="Get or set the stream's HTTP URL. Setting a new URL will not affect an ongoing connection.",
                  usage="url [url]", "url", fun (url) -> begin
                    if url == "" then
                      s.url()
                    else begin
                      s.set_url({url})
                      "Done!"
                    end end
                  end)

  server.register(namespace=s.id(), description="Return the current status of the source, either \"stopped\" \
                                                 (the source isn't trying to relay the HTTP stream), \"polling\" \
                                                 (attempting to connect to the HTTP stream) or \"connected <url>\" \
                                                 (connected to <url>, buffering or playing back the stream).",
                  "status", fun (_) -> begin
                    s.status()
                  end)

  server.register(namespace=s.id(), description="Get the buffer's length, in seconds.",
                  "buffer_length", fun (_) -> begin
                    "#{s.buffer_length()}"
                  end)

  s
end
%endif
