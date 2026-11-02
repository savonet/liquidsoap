# Log files of icecast.server (observability.md §9). logs.liq keeps one
# `icecast.server.log_files.file(label=...)` per log and calls `write` with the
# target and rotation parameters read at that write, and `reopen` with the
# target in force from the server's `reopen_logs`. A failure is logged under
# `label` and never raised; every file closes at shutdown.
let icecast.server.log_files = ()

# The name a file rotated now takes.
# @flag hidden
def icecast.server.log_files.rotated_name(~archive, path) =
  archive ? "#{path}.#{time.string("%Y%m%d_%H%M%S")}" : "#{path}.old"
end

# One log file: `write(~target, ~size_limit, ~archive, line)` appends `line`
# to `target` (`"-"` is standard error, `null` writes nothing), reopening when
# the target changed and rotating first when the file is past `size_limit`.
# @flag hidden
def icecast.server.log_files.file(~label) =
  opened = icecast.server.cell(null)
  lock = ref(false)

  def close() =
    current = opened.get()
    opened.set(null)
    if
      null.defined(current)
    then
      handle = null.get(current).handle
      handle.close()
    end
  end

  def open_file(path) =
    size = file.size(path)
    opened.set(
      {
        path = path,
        handle = file.open(write=true, append=true, path),
        size = icecast.server.cell(size)
      }
    )
  end

  # After a failed rename, lines keep going to the file in place.
  def rotate(~archive, path) =
    close()
    try
      file.move(
        force=true,
        path,
        icecast.server.log_files.rotated_name(archive=archive, path)
      )
    catch err do
      log.severe(
        label=label,
        "Log file rotation failed: #{err.message}"
      )
    end
    open_file(path)
  end

  def write_line(~target, ~size_limit, ~archive, line) =
    current = opened.get()
    if
      null.defined(current) and null.get(current).path != target
    then
      close()
    end
    if not null.defined(opened.get()) then open_file(target) end
    if
      null.get(opened.get()).size.get() > size_limit
    then
      rotate(archive=archive, target)
    end
    current = null.get(opened.get())
    current.handle.write(line)
    current.size.set(current.size.get() + string.length(encoding="ascii", line))
  end

  def locked(fn) =
    icecast.server.with_lock(
      lock,
      fun () ->
        try
          fn()
        catch err do
          try
            close()
          catch _ do
            ()
          end
          log.severe(
            label=label,
            "Log file write failed: #{err.message}"
          )
        end
    )
  end

  def write(~(target:string?), ~(size_limit:int), ~(archive:bool), line) =
    line = "#{line}\n"
    locked(
      fun () ->
        if
          target == ("-" : string?)
        then
          close()
          process.stderr.write(line)
        elsif
          null.defined(target)
        then
          write_line(
            target=null.get(target),
            size_limit=size_limit,
            archive=archive,
            line
          )
        else
          close()
        end
    )
  end

  def reopen((target:string?)) =
    locked(
      fun () ->
        begin
          close()
          if
            null.defined(target) and target != ("-" : string?)
          then
            open_file(null.get(target))
          end
        end
    )
  end

  on_shutdown(fun () -> locked(close))
  {write = write, reopen = reopen}
end
