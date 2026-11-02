# Logs of icecast.server (observability.md §9 to §11, §14). The server state
# holds `icecast.server.logs.create(...)`: listeners.liq writes the `access`
# line of each finished session, mount_outputs.liq the `playlist` line of each
# metadata update, `icecast.server.log` sends a server message to Liquidsoap's
# log and to the `error` log, and `reopen` is the server's `reopen_logs`.
let icecast.server.logs = ()

# The log parameters of `p`, read at one write.
# @flag hidden
def icecast.server.logs.settings(p) =
  {
    access_log = p.access_log(),
    playlist_log = p.playlist_log(),
    error_log = p.error_log(),
    log_level = p.log_level(),
    log_size = p.log_size(),
    log_archive = p.log_archive()
  }
end

# The server's logs, reading their parameters with `settings()` at each write.
# @flag hidden
def icecast.server.logs.create(~label, ~compatibility, settings) =
  access_file = icecast.server.log_files.file(label=label)
  playlist_file = icecast.server.log_files.file(label=label)
  error_file = icecast.server.log_files.file(label=label)

  def current() =
    (settings() :
      {
        access_log: string?,
        playlist_log: string?,
        error_log: string?,
        log_level: int,
        log_size: int,
        log_archive: bool
      }
    )
  end

  def write(~when=fun (_) -> true, file, target, line) =
    try
      s = current()
      if
        when(s)
      then
        file.write(
          target=target(s),
          size_limit=s.log_size,
          archive=s.log_archive,
          line()
        )
      end
    catch err do
      log.severe(
        label=label,
        "Log write failed: #{err.message}"
      )
    end
  end

  def access(record) =
    write(
      access_file,
      fun (s) -> s.access_log,
      fun () ->
        icecast.server.log_lines.access(
          compatibility=compatibility,
          ended=time(),
          record
        )
    )
  end

  def playlist(~mount, ~listeners, metadata) =
    text = icecast.server.stats.title(metadata)
    if
      null.defined(text)
    then
      write(
        playlist_file,
        fun (s) -> s.playlist_log,
        fun () ->
          icecast.server.log_lines.playlist(
            t=time(),
            mount=mount,
            listeners=listeners,
            null.get(text)
          )
      )
    end
  end

  def error(~category, level, message) =
    write(
      when=fun (s) -> level <= s.log_level,
      error_file,
      fun (s) -> s.error_log,
      fun () ->
        icecast.server.log_lines.error(
          t=time(),
          category=category,
          level,
          message
        )
    )
  end

  def reopen() =
    s = current()
    access_file.reopen(s.access_log)
    playlist_file.reopen(s.playlist_log)
    error_file.reopen(s.error_log)
  end

  {access = access, playlist = playlist, error = error, reopen = reopen}
end

# Logs `message` at `level` under the server's label, and to its error log
# with the Icecast `category`.
# @flag hidden
def icecast.server.log(state, ~category="icecast/server", level, message) =
  log(label=state.name, level=level, message)
  state.logs.error(category=category, level, message)
end

# @flag hidden
def icecast.server.log.severe(state, ~category="icecast/server", message) =
  icecast.server.log(state, category=category, 2, message)
end

# @flag hidden
def icecast.server.log.important(state, ~category="icecast/server", message) =
  icecast.server.log(state, category=category, 3, message)
end

# @flag hidden
def icecast.server.log.info(state, ~category="icecast/server", message) =
  icecast.server.log(state, category=category, 4, message)
end

# @flag hidden
def icecast.server.log.debug(state, ~category="icecast/server", message) =
  icecast.server.log(state, category=category, 5, message)
end
