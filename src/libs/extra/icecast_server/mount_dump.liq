# Starts recording `feeder` per the mount parameters `mount` and returns the
# function that stops it (mounts.md §9). The limits are checked at each frame,
# so a recording stops right after the write that exceeds them; a write error
# stops it too instead of failing the server's clock.
# @flag hidden
def icecast.server.mounts.dump(state, mount, feeder) =
  if
    not null.defined(mount.dump_file)
  then
    fun () -> ()
  else
    path = time.string(null.get(mount.dump_file))
    time_limit = mount.dump_file_time_limit ?? 0.
    size_limit = mount.dump_file_size_limit ?? 0
    started = time()
    stopped = ref(false)
    output_ref = ref(null)
    def stop() =
      if
        not atomic(fun () -> stopped.exchange(true))
      then
        thread.run(
          fun () ->
            if
              null.defined(output_ref())
            then
              null.get(output_ref()).shutdown()
            end
        )
      end
    end

    def recording() =
      if
        (time_limit > 0. and time() - started > time_limit)
        or (
          size_limit > 0 and file.exists(path) and file.size(path) > size_limit
        )
      then
        stop()
      end
      not stopped()
    end

    def on_error(_) =
      icecast.server.log.important(
        state,
        "Recording to #{path} failed, stopping it."
      )
      stop()
      1000000.
    end

    o =
      output.file(
        id="#{source.id(feeder.source)}.dump",
        fallible=true,
        flush=true,
        reopen_on_error=on_error,
        feeder.encoder([("flush_packets", "1")]),
        path,
        icecast.server.mounts.follow(
          "#{source.id(feeder.source)}.recorded",
          fun () -> recording() ? feeder.source : (source.fail())
        )
      )
    output_ref := o
    icecast.server.mounts.unify_clock(state, o.clock)
    stop
  end
end
