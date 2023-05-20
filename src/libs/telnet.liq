# @docof request.dynamic
def replaces request.dynamic(%argsof(request.dynamic), fn) =
  s = request.dynamic(%argsof(request.dynamic), fn)

  s.on_wake_up(memoize({
    server.register(namespace=source.id(s), description="Flush the queue and skip the current track",
                    "flush_and_skip", fun (_) -> try
                      s.set_queue([])
                      s.skip()
                      "Done."
                    catch err do
                      "Error while flushing and skipping source: #{err}"
                    end)
  }))

  s
end

%ifdef input.gstreamer.audio_video
# @docof input.gstreamer.audio_video
def replaces input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video)) =
  s = input.gstreamer.audio_video(%argsof(input.gstreamer.audio_video))

  s.on_wake_up(memoize({
    begin
      server.register(namespace=source.id(s), description="Set gstreamer pipeline state to play",
                      "play", fun (_) -> try
                        s.play()
                        "Done."
                      catch err do
                        "Error while setting state to play: #{err}"
                      end)

      server.register(namespace=source.id(s), description="Set gstreamer pipeline state to paused",
                      "pause", fun (_) -> try
                      s.pause()
                      "Done."
                    catch err do
                      "Error while setting state to paused: #{err}"
                    end)

      server.register(namespace=source.id(s), description="Restart gstreamer pipeline",
                      "restart", fun (_) -> try
                        s.restart()
                        "Done."
                      catch err do
                        "Error while restarting pipeline: #{err}"
                      end)
    end
  }))

  s
end
%endif

# @docof blank.strip
def replaces blank.strip(%argsof(blank.strip), s) =
  s = blank.strip(%argsof(blank.strip), s)

  s.on_wake_up(memoize({
    server.register(namespace=source.id(s), description="Check if the source is stripping.",
                    "is_stripping", fun (_) -> begin
                      "#{s.is_blank()}"
                    end)
  }))

  s
end

# @docof output.external
def replaces output.external(%argsof(output.external), f, p, s) =
  s = output.external(%argsof(output.external), f, p, s)

  s.on_wake_up(memoize({
    server.register(namespace=s.id(), description="Re-open the output.",
                    "reopen", fun (_) -> begin
                      s.reopen()
                      "Done."
                    end)
  }))

  s
end

# @docof input.external.avi
def replaces input.external.avi(%argsof(input.external.avi), s) =
  s = input.external.avi(%argsof(input.external.avi), s)

  s.on_wake_up(memoize({
    server.register(namespace=source.id(s), description="Show internal buffer length (in seconds).",
                    "buffer_length", fun (_) -> begin
                      buffered = s.buffered()
                      audio = list.assoc(default=0., "audio", buffered)
                      video = list.assoc(default=0., "video", buffered)
                      total = min(audio, video)
                      "audio buffer length: #{audio}\nvideo buffer length: #{video}\ntotal buffer length: #{total}"
                    end)
  }))

  s
end

# @docof input.harbor
def replaces input.harbor(%argsof(input.harbor), s) =
  s = input.harbor(%argsof(input.harbor), s)

  s.on_wake_up(memoize({
    begin
      server.register(namespace=source.id(s), description="Stop current source client, if connected.",
                     "stop", fun (_) -> begin
                        s.stop()
                        "Done"
                      end)

      server.register(namespace=source.id(s), description="Display current status.",
                      "status", fun (_) -> begin
                        s.status()
                      end)

      server.register(namespace=source.id(s), description="Get the buffer's length, in seconds.",
                      "buffer_length", fun (_) -> begin
                        "#{s.buffer_length()}"
                      end)
    end
  }))

  s
end

%ifdef input.http
# @docof input.http
def replaces input.http(%argsof(input.http), s) =
  s = input.http(%argsof(input.http), s)

  s.on_wake_up(memoize({
    begin
      server.register(namespace=source.id(s), description="Start the source, if needed.",
                      "start", fun (_) -> begin
                        s.start()
                        "Done!"
                      end)

      server.register(namespace=source.id(s), description="Stop the source if connected.",
                      "stop", fun (_) -> begin
                        s.stop()
                        "Done!"
                      end)

      server.register(namespace=source.id(s), description="Get or set the stream's HTTP URL. Setting a new URL will not affect an ongoing connection.",
                      usage="url [url]", "url", fun (url) -> begin
                        if url == "" then
                          s.url()
                        else begin
                          s.set_url({url})
                          "Done!"
                        end end
                      end)

      server.register(namespace=source.id(s), description="Return the current status of the source, either \"stopped\" \
                                                     (the source isn't trying to relay the HTTP stream), \"polling\" \
                                                     (attempting to connect to the HTTP stream) or \"connected <url>\" \
                                                     (connected to <url>, buffering or playing back the stream).",
                      "status", fun (_) -> begin
                        s.status()
                      end)

      server.register(namespace=source.id(s), description="Get the buffer's length, in seconds.",
                      "buffer_length", fun (_) -> begin
                        "#{s.buffer_length()}"
                      end)
    end
  }))

  s
end
%endif
