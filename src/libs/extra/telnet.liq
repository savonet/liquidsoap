# @docof request.dynamic
def replaces request.dynamic(%argsof(request.dynamic), fn) =
  s = request.dynamic(%argsof(request.dynamic), fn)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        server.register(
          namespace=source.id(s),
          description="Flush the queue and skip the current track",
          "flush_and_skip",
          fun (_) ->
            try
              s.set_queue([])
              s.skip()
              "Done."
            catch err do
              "Error while flushing and skipping source: #{err}"
            end
        )
      }
    )
  )

  s
end

# @docof blank.strip
def replaces blank.strip(%argsof(blank.strip), s) =
  s = blank.strip(%argsof(blank.strip), s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        server.register(
          namespace=source.id(s),
          description="Check if the source is stripping.",
          "is_stripping",
          fun (_) -> begin "#{s.is_blank()}" end
        )
      }
    )
  )

  s
end

# @docof output.external
def replaces output.external(%argsof(output.external), f, p, s) =
  s = output.external(%argsof(output.external), f, p, s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        server.register(
          namespace=s.id(),
          description="Re-open the output.",
          "reopen",
          fun (_) ->
            begin
              s.reopen()
              "Done."
            end
        )
      }
    )
  )

  s
end

# @docof input.external.avi
def replaces input.external.avi(%argsof(input.external.avi), s) =
  s = input.external.avi(%argsof(input.external.avi), s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        server.register(
          namespace=source.id(s),
          description="Show internal buffer length (in seconds).",
          "buffer_length",
          fun (_) ->
            begin
              buffered = s.buffered()
              audio = list.assoc(default=0., "audio", buffered)
              video = list.assoc(default=0., "video", buffered)
              total = min(audio, video)
              "audio buffer length: #{audio}\nvideo buffer length: #{
                video
              }\ntotal buffer length: #{total}"
            end
        )
      }
    )
  )

  s
end

# @docof input.harbor
def replaces input.harbor(%argsof(input.harbor), s) =
  s = input.harbor(%argsof(input.harbor), s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        begin
          server.register(
            namespace=source.id(s),
            description="Stop current source client, if connected.",
            "stop",
            fun (_) ->
              begin
                s.stop()
                "Done"
              end
          )

          server.register(
            namespace=source.id(s),
            description="Display current status.",
            "status",
            fun (_) -> begin s.status() end
          )

          server.register(
            namespace=source.id(s),
            description="Get the buffer's length, in seconds.",
            "buffer_length",
            fun (_) -> begin "#{s.buffer_length()}" end
          )
        end
      }
    )
  )

  s
end

%ifdef input.http
# @docof input.http
def replaces input.http(%argsof(input.http), s) =
  s = input.http(%argsof(input.http), s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        begin
          server.register(
            namespace=source.id(s),
            description="Start the source, if needed.",
            "start",
            fun (_) ->
              begin
                s.start()
                "Done!"
              end
          )

          server.register(
            namespace=source.id(s),
            description="Stop the source if connected.",
            "stop",
            fun (_) ->
              begin
                s.stop()
                "Done!"
              end
          )

          server.register(
            namespace=source.id(s),
            description="Get or set the stream's HTTP URL. Setting a new URL \
             will not affect an ongoing connection.",
            usage="url [url]",
            "url",
            fun (url) ->
              begin
                if
                  url == ""
                then
                  s.url()
                else
                  begin
                    s.set_url({url})
                    "Done!"
                  end
                end
              end
          )

          server.register(
            namespace=source.id(s),
            description="Return the current status of the source, either \
             \"stopped\" (the source isn't trying to relay the HTTP stream), \
             \"polling\" (attempting to connect to the HTTP stream) or \
             \"connected <url>\" (connected to <url>, buffering or playing back \
             the stream).",
            "status",
            fun (_) -> begin s.status() end
          )

          server.register(
            namespace=source.id(s),
            description="Get the buffer's length, in seconds.",
            "buffer_length",
            fun (_) ->
              "#{list.fold(fun (l, (_, l')) -> max(l, l'), 0., s.buffered())}"
          )
        end
      }
    )
  )

  s
end
%endif

# @docof lufs
def replaces lufs(%argsof(lufs), s) =
  s = lufs(%argsof(lufs), s)
  s.on_wake_up(
    synchronous=false,
    memoize(
      {
        server.register(
          namespace=source.id(s),
          description="Current value for the LUFS (short-term value computed \
           over the duration specified by the `window` parameter).",
          "lufs",
          fun (_) ->
            "#{s.lufs()} LUFS"
        )

        server.register(
          namespace=source.id(s),
          description="Average LUFS value over the current track.",
          "lufs_integrated",
          fun (_) ->
            "#{s.lufs_integrated()} LUFS"
        )

        server.register(
          namespace=source.id(s),
          description="Momentary LUFS (over a 400ms window).",
          "lufs_momentary",
          fun (_) ->
            "#{s.lufs_momentary()} LUFS"
        )
      }
    )
  )

  s
end

server.register(
  description="Shutdown the running liquidsoap instance",
  "shutdown",
  fun (_) ->
    begin
      shutdown()
      "OK"
    end
)
