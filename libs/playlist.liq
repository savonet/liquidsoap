# Play a list of files.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is only validated if the function returns true on it. The function is called before resolution, hence metadata will only be available for requests corresponding to local files. This is typically used to avoid repetitions, but be careful: if the function rejects all attempts, the playlist will enter into a consuming loop and stop playing anything.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mime_type Default MIME type for the playlist. Empty string means automatic detection.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), or shuffle the playlist each time it is loaded, and play it in this order for a whole round ("randomize" mode), or pick a random file in the playlist each time ("random" mode).
# @param ~on_done Function executed when the playlist is finished.
# @param ~timeout Timeout (in sec.) for a single download.
# @param playlist Playlist.
def playlist.list(~id="playlist.list.reloadable", ~check_next=fun(_)->true, ~conservative=false, ~default_duration=30., ~length=10., ~loop=true, ~mime_type="", ~mode="normal", ~on_done={()}, ~timeout=20., playlist)
  mode = if not list.mem(mode, ["normal", "random", "randomize"]) then log.severe(label=id, "Invalid mode: #{mode}"); "randomize" else mode end

  # Original playlist when loaded
  playlist_orig = ref(playlist)

  # Randomize the playlist if necessary
  def randomize(p)
    if mode == "randomize" then
      list.sort(fun (x,y) -> int_of_float(random.float()), p)
    else
      p
    end
  end

  # Current remaining playlist
  playlist = ref(randomize(playlist))

  # A reference to know if the source has been stopped
  has_stopped = ref(false)

  # The next function
  def rec next() =
    if loop and list.length(!playlist) == 0 then playlist := randomize(!playlist_orig) end
    file =
      if list.length(!playlist) > 0 then
        if mode == "random" then
          n = random.int(min=0, max=list.length(!playlist))
          list.nth(default="", !playlist, n)
        else
          ret = list.hd(default="", !playlist)
          playlist := list.tl(!playlist)
          ret
        end
      else
        # Playlist finished
        if not !has_stopped then
          has_stopped := true
          log.info(label=id, "Playlist stopped.")
          on_done()
        end
        ""
      end
    if file == "" then
      []
    else
      log.debug(label=id, "Next song will be \"#{file}\".")
      r = request.create(file)
      if check_next(r) then
        [r]
      else
        log.info(label=id, "Request #{request.uri(r)} rejected by check_next.")
        request.destroy(r)
        next()
      end
    end
  end

  # Instantiate the source
  s = request.dynamic.list(id=id, conservative=conservative, default_duration=default_duration, length=length, timeout=timeout, available={not !has_stopped}, next)

  # The reload function
  def reload(p) =
    log.debug(label=id, "Reloading playlist.")
    playlist := randomize(p)
    has_stopped := false
  end

  # Return
  s.{reload = reload}
end

# Read a playlist or a directory, play all files and stop. It returns a pair
# `(reload,source)` where `reload` is a function of type `(?uri:string)->unit`
# that can be used to reload the source and `source` is the actual source. The
# reload function can optionally be called with a new playlist URI, otherwise,
# it reloads the previous URI.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is only validated if the function returns true on it. The function is called before resolution, hence metadata will only be available for requests corresponding to local files. This is typically used to avoid repetitions, but be careful: if the function rejects all attempts, the playlist will enter into a consuming loop and stop playing anything.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mime_type Default MIME type for the playlist. Empty string means automatic detection.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), or shuffle the playlist each time it is loaded, and play it in this order for a whole round ("randomize" mode), or pick a random file in the playlist each time ("random" mode).
# @param ~on_done Function executed when the playlist is finished.
# @param ~prefix Add a constant prefix to all requests. Useful for passing extra information using annotate, or for resolution through a particular protocol, such as replaygain.
# @param ~timeout Timeout (in sec.) for a single download.
# @param uri Playlist URI.
def playlist.reloadable(~id="playlist.reloadable", ~check_next=fun(_)->true, ~conservative=false, ~default_duration=30., ~length=10., ~loop=true, ~mime_type="", ~mode="randomize", ~on_done={()}, ~prefix="", ~timeout=20., uri)
  # URI of the current playlist
  playlist_uri=ref(uri)

  # The load function
  def load_playlist () =
    playlist_uri = path.home.unrelate(!playlist_uri)
    log.info(label=id, "Reloading playlist.")
    if file.is_directory(playlist_uri) then
      log.info(label=id,"Playlist is a directory.")
      files = file.ls(absolute=true, recursive=true, playlist_uri)
      files = list.filter(fun(f) -> not (file.is_directory(f)), files)
      files = list.sort(fun (x,y) -> if x<y then -1 elsif x>y then 1 else 0 end, files)
      def add_prefix(file) = prefix^file end
      list.map(add_prefix, files)
    else
      pl = request.create(playlist_uri)
      result =
        if request.resolve(timeout=timeout, pl) then
          pl = request.filename(pl)
          files = playlist.parse(mime=mime_type, pl)
          def file_request(el) =
            let (meta, file) = el
            file = prefix^file
            s = list.fold(fun (cur, el) ->
            "#{cur},#{fst(el)}=#{string.escape(snd(el))}", "", meta)
            if s == "" then
              file
            else
              "annotate:#{s}:#{file}"
            end
          end
          list.map(file_request,files)
        else
          log.important(label=id, "Couldn't read playlist: request resolution failed.")
          []
        end
      request.destroy(pl)
      result
    end
  end

  # Create the source
  s = playlist.list(id=id, check_next=check_next, conservative=conservative, default_duration=default_duration, length=length, loop=loop, mime_type=mime_type, mode=mode, on_done=on_done, timeout=timeout, load_playlist())

  # The reload function
  def reload(~uri="") =
    if uri != "" then
      playlist_uri := uri
    end
    log(label=id,"Reloading playlist with URI #{!playlist_uri}.")
    s.reload(load_playlist())
  end

  s.{reload = reload}
end

# Read a playlist or a directory and play all files.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is only validated if the function returns true on it. The function is called before resolution, hence metadata will only be available for requests corresponding to local files. This is typically used to avoid repetitions, but be careful: if the function rejects all attempts, the playlist will enter into a consuming loop and stop playing anything.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mime_type Default MIME type for the playlist. Empty string means automatic detection.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), or shuffle the playlist each time it is loaded, and play it in this order for a whole round ("randomize" mode), or pick a random file in the playlist each time ("random" mode).
# @param ~on_reload Callback called after playlist has reloaded.
# @param ~prefix Add a constant prefix to all requests. Useful for passing extra information using annotate, or for resolution through a particular protocol, such as replaygain.
# @param ~reload Amount of time (in seconds or rounds), when applicable, before which the playlist is reloaded; 0 means never.
# @param ~reload_mode Unit of the reload parameter, either "never" (never reload the playlist), "rounds", "seconds" or "watch" (reload the file whenever it is changed).
# @param ~timeout Timeout (in sec.) for a single download.
# @param uri Playlist URI.
def replaces playlist(~id="", ~check_next=fun(_)->true, ~conservative=false, ~default_duration=30., ~length=10., ~loop=true, ~mime_type="", ~mode="randomize", ~on_reload={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", ~timeout=20., uri)

  reload_mode = if not list.mem(reload_mode, ["never", "rounds", "seconds", "watch"]) then log.severe(label=id, "Invalid reload mode: #{mode}"); "seconds" else reload_mode end
  uri = ref(uri)
  round = ref(0)

  # Will be filled with the actual reload function
  rreloader = ref(fun()->())

  # Reload when the playlist is done
  def on_done()
    reloader = !rreloader
    if reload_mode == "rounds" and reload > 0 then
      round := !round + 1
      if !round >= reload then
        round := 0
        reloader()
      end
    end
  end

  s = playlist.reloadable(id=id, check_next=check_next, conservative=conservative, default_duration=default_duration, length=length, loop=loop, mime_type=mime_type, mode=mode, on_done=on_done, prefix=prefix, timeout=timeout, !uri)
  def reloader (~uri="") = s.reload(uri=uri); on_reload () end
  rreloader := {reloader()}

  # Set up reloading for seconds and watch
  if reload_mode == "seconds" and reload > 0 then
    n = float_of_int(reload)
    thread.run(delay=n, every=n, reloader)
  elsif reload_mode == "watch" then
    unwatch = file.watch(!uri, reloader)
    s.on_shutdown(unwatch)
  end

  # Set up telnet commands
  server.register(namespace=id, description="Skip current song in the playlist.", usage="skip", "skip", fun (_) -> begin s.skip(); "OK" end)
  server.register(namespace=id, description="Reload the playlist, unless already being loaded.", usage="reload", "reload", fun (_) -> begin reloader(); "OK" end)
  def uri_cmd (uri') =
    if uri' == "" then !uri
    else
      if reload_mode == "watch" then log.important(label=id, "Warning: the watched file is not updated for now when changing the uri!") end # TODO
      uri := uri'
      reloader(uri=uri')
      "OK"
    end
  end
  server.register(namespace=id, description="Print playlist URI if called without an argument, otherwise set a new one and load it.", usage="uri [<uri>]", "uri", uri_cmd)

  s
end
