server =
  icecast.server(
    {
      source_password = "change-me",
      access_log = "/var/log/liquidsoap/access.log",
      playlist_log = "/var/log/liquidsoap/playlist.log",
      error_log = "/var/log/liquidsoap/error.log",
      admin_password = "admin-secret"
    }
  )

# logrotate moves the files, then sends SIGHUP.
ignore(on_signal("hup", fun () -> server.reopen_logs()))
