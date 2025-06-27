let settings.playlist.mime_types =
  settings.make.void(
    "Mime-types used for guessing playlist formats."
  )

let playlist.parse.cue = ()

# Parse a cue file
# @category Liquidsoap
# @param ~pwd Path to use for relative path resolution
def playlist.parse.cue.full(~pwd=null, content) =
  content = string.split(separator="[\r\n]+", content)

  def parse_file(s) =
    matches = r/^FILE (.+)$/.exec(string.trim(s))
    try
      match = list.assoc(1, matches)
      match_chars = string.chars(match)

      def rec get_last(~char, cur, chars) =
        let [...chars, last] = chars
        if
          last == char
        then
          filename = string.concat(separator="", chars)
          file_type = string.trim(string.concat(separator="", cur))
          file_type =
            if
              file_type == ""
            then
              null
            else
              string.case(lower=true, file_type)
            end
          (filename, file_type)
        else
          get_last(char=char, [last, ...cur], chars)
        end
      end

      let (filename, file_type) =
        if
          list.hd(match_chars) == '"'
        then
          let [_, ...chars] = match_chars
          let (filename, file_type) = get_last(char='"', [], chars)
          (string.unquote('"#{filename}"'), file_type)
        else
          get_last(
            char=
              " ",
            [],
            match_chars
          )
        end

      filename = playlist.parse.get_file(pwd=pwd, filename)
      {filename=filename, file_type=file_type}
    catch _ do
      null
    end
  end

  def parse_track_attribute(s) =
    matches = r/^TRACK ([^\s]+)\s([^\s]+)?$/.exec(string.trim(s))
    try
      position = int_of_string(list.assoc(1, matches))
      type = list.assoc(2, matches)

      {position=position, track_type=string.case(lower=true, type)}
    catch _ : [error.not_found] do
      null
    end
  end

  def parse_rem(s) =
    matches = r/^REM ([^\s]+) (.+)$/.exec(string.trim(s))
    try
      name = string.case(lower=true, list.assoc(1, matches))
      value = string.unquote(list.assoc(2, matches))
      (name, value)
    catch _ : [error.not_found] do
      null
    end
  end

  def parse_timecode(s) =
    matches = r/^([\d]+):([\d]+):([\d]+)/.exec(string.trim(s))
    try
      minutes = int_of_string(list.assoc(1, matches))
      seconds = int_of_string(list.assoc(2, matches))
      frames = int_of_string(list.assoc(3, matches))

      {minutes=minutes, seconds=seconds, frames=frames}
    catch _ : [error.not_found] do
      null
    end
  end

  def parse_index(s) =
    matches = r/^INDEX ([\d]+) ([\d:]+)/.exec(string.trim(s))
    try
      index = int_of_string(list.assoc(1, matches))
      timecode = list.assoc(2, matches)

      (index, null.get(parse_timecode(timecode)))
    catch _ do
      null
    end
  end

  def parse_optional(~label, s) =
    matches =
      regexp(
        "^#{string.case(lower=false, label)} (.+)$"
      ).exec(string.trim(s))
    null.map(string.unquote, list.assoc.nullable(1, matches))
  end

  def end_parse_track(content) =
    content == [] or null.defined(parse_track_attribute(list.hd(content)))
  end

  def rec parse_track(~file, ~track, content) =
    track =
      (track :
        {
          position: int,
          track_type?: string,
          performer?: string,
          title?: string,
          album?: string,
          isrc?: string,
          postgap?: {minutes: int, seconds: int, frames: int},
          pregap?: {minutes: int, seconds: int, frames: int},
          indexes: [
            (
              int
              *
              {
                filename?: string,
                file_type?: string,
                minutes: int,
                seconds: int,
                frames: int
              }
            )
          ]
        }
      )

    if
      end_parse_track(content)
    then
      (file, track, content)
    else
      let [s, ...content] = content

      index = parse_index(s)
      new_file = parse_file(s)

      let (file, track) =
        if
          null.defined(index)
        then
          let (idx, timecode) = null.get(index)
          (
            file,
            {
              ...track,
              indexes=[...track.indexes, (idx, {...file, ...timecode})]
            }
          )
        elsif null.defined(new_file) then (new_file, track)
        else
          track_attributes =
            [
              ("title", fun (title) -> {...track, title=title}),
              ("performer", fun (performer) -> {...track, performer=performer}),
              (
                "pregap",
                fun (pregap) ->
                  {...track, pregap=null.get(parse_timecode(pregap))}
              ),
              (
                "postgap",
                fun (postgap) ->
                  {...track, postgap=null.get(parse_timecode(postgap))}
              ),
              (
                "rem",
                fun (rem) ->
                  begin
                    parsed =
                      parse_rem(
                        "REM #{rem}"
                      )
                    if
                      null.defined(parsed)
                    then
                      let (label, value) = null.get(parsed)
                      if
                        label == "album"
                      then
                        {...track, album=value}
                      else
                        log.important(
                          label="playlist.parse.cue.full",
                          "Unknown track attribute REM #{rem}, please file a bug \
                           report!"
                        )
                        track
                      end
                    end
                  end
              ),
              ("isrc", fun (isrc) -> {...track, isrc=isrc})
            ]
          attribute_names = list.map(fst, track_attributes)

          def rec check_attribute(attribute_names) =
            if
              attribute_names == []
            then
              log.important(
                label="playlist.parse.cue.full",
                "Could not parse track attribute: #{s}"
              )
              track
            else
              let [name, ...attribute_names] = attribute_names
              let fn = list.assoc(name, track_attributes)
              parsed = parse_optional(label=name, s)

              if
                null.defined(parsed)
              then
                fn(null.get(parsed))
              else
                check_attribute(attribute_names)
              end
            end
          end

          (file, check_attribute(attribute_names))
        end
      parse_track(file=file, track=track, content)
    end
  end

  def rec parse_content(~file, ~sheet, content) =
    sheet =
      (sheet :
        {
          catalog?: string,
          performer?: string,
          title?: string,
          rem: [(string * string)],
          tracks: [
            {
              position: int,
              track_type?: string,
              performer?: string,
              title?: string,
              album?: string,
              isrc?: string,
              postgap?: {minutes: int, seconds: int, frames: int},
              pregap?: {minutes: int, seconds: int, frames: int},
              indexes: [
                (
                  int
                  *
                  {
                    filename?: string,
                    file_type?: string,
                    minutes: int,
                    seconds: int,
                    frames: int
                  }
                )
              ]
            }
          ]
        }
      )

    if
      content == []
    then
      sheet
    else
      let [s, ...content] = content

      new_file = parse_file(s)
      new_rem = parse_rem(s)
      track = parse_track_attribute(s)

      if
        null.defined(new_file)
      then
        parse_content(file=new_file, sheet=sheet, content)
      elsif
        null.defined(new_rem)
      then
        parse_content(
          file=file,
          sheet={...sheet, rem=[null.get(new_rem), ...sheet.rem]},
          content
        )
      elsif
        null.defined(track)
      then
        let (file, track, content) =
          parse_track(
            file=file, track={...null.get(track), indexes=[]}, content
          )
        parse_content(
          file=file, sheet={...sheet, tracks=[...sheet.tracks, track]}, content
        )
      else
        sheet_attributes =
          [
            ("catalog", fun (catalog) -> {...sheet, catalog=catalog}),
            ("performer", fun (performer) -> {...sheet, performer=performer}),
            ("title", fun (title) -> {...sheet, title=title})
          ]

        attribute_names = list.map(fst, sheet_attributes)

        def rec check_attribute(attribute_names) =
          if
            attribute_names == []
          then
            log.important(
              label="playlist.parse.cue.full",
              "Could not parse attribute: #{string.quote(s)}"
            )
            sheet
          else
            let [name, ...attribute_names] = attribute_names
            let fn = list.assoc(name, sheet_attributes)
            let parsed = parse_optional(label=name, s)

            if
              null.defined(parsed)
            then
              fn(null.get(parsed))
            else
              check_attribute(attribute_names)
            end
          end
        end

        parse_content(
          file=file, sheet=check_attribute(attribute_names), content
        )
      end
    end
  end

  parse_content(file=null, sheet={tracks=[], rem=[]}, content)
end

let settings.playlist.cue =
  settings.make.void(
    "Settings for parsing cue files"
  )

let settings.playlist.cue.pregap_metadata =
  settings.make(
    description=
      "Metadata used to pass pre-gap cue metadata",
    "liq_pregap"
  )

let settings.playlist.cue.index_zero_metadata =
  settings.make(
    description=
      "Metadata used to pass index 0 cue metadata",
    "liq_index_zero"
  )

let settings.playlist.cue.index_zero_filename_metadata =
  settings.make(
    description=
      "Metadata used to pass index 0 filename metadata",
    "liq_index_zero_filename"
  )

let settings.playlist.cue.postgap_metadata =
  settings.make(
    description=
      "Metadata used to pass pre-gap cue metadata",
    "liq_postgap"
  )

# Parse a cue file and return a value suitable for playlist parser registration.
# @category Liquidsoap
# @param ~pwd Path to use for relative path resolution
def replaces playlist.parse.cue(~pwd=null, content) =
  # Simple test: playlist should have at least one file..
  if
    r/FILE/.test(content)
  then
    let {catalog?, title?, performer?, rem, tracks} =
      playlist.parse.cue.full(pwd=pwd, content)
    let playlist_album = title
    let album_performer = performer
    rem =
      list.map(
        fun (el) ->
          begin
            let (name, value) = el
            if name == "date" then ("year", value) else (name, value) end
          end,
        rem
      )

    def seconds_of_timecode(timecode) =
      let {minutes, seconds, frames} = timecode
      float(minutes) * 60. + float(seconds) + float(frames) / 75.
    end

    def string_of_timecode(timecode) =
      string(seconds_of_timecode(timecode))
    end

    def add_track(tracks, track) =
      let {
        performer?,
        title?,
        album?,
        isrc?,
        pregap?,
        postgap?,
        position,
        indexes
      } = track
      index_zero = list.assoc.nullable(0, indexes)
      index_zero_filename = null.map(fun (m) -> m?.filename, index_zero)
      index = list.assoc.nullable(1, indexes)
      filename = null.map(fun (m) -> m?.filename, index)
      cue_in_metadata = settings.playlist.cue_in_metadata()
      cue_out_metadata = settings.playlist.cue_out_metadata()
      pregap_metadata = settings.playlist.cue.pregap_metadata()
      index_zero_metadata = settings.playlist.cue.index_zero_metadata()
      index_zero_filename_metadata =
        settings.playlist.cue.index_zero_filename_metadata()
      postgap_metadata = settings.playlist.cue.postgap_metadata()

      if
        not null.defined(filename) or (not null.defined(index))
      then
        log.important(
          label="playlist.cue.parse",
          "Track without filename or index: #{track}"
        )
        tracks
      else
        let timecode = null.get(index)
        let filename = null.get(filename)

        timecode = seconds_of_timecode(timecode)
        cue_in = timecode == 0. ? [] : [(cue_in_metadata, string(timecode))]

        cue_out =
          try
            let (old_meta, old_filename) = list.hd(tracks)
            if
              old_filename == filename
            then
              index_zero = list.assoc(default="", index_zero_metadata, old_meta)

              cue_out =
                if
                  index_zero != ""
                then
                  index_zero
                else
                  list.assoc(cue_in_metadata, old_meta)
                end

              [(cue_out_metadata, cue_out)]
            else
              []
            end
          catch _ do
            []
          end

        meta =
          list.fold(
            fun (meta, el) ->
              begin
                let (name, value) = el
                if
                  null.defined(value)
                then
                  [(name, null.get(value)), ...meta]
                else
                  meta
                end
              end,
            [],
            [
              (pregap_metadata, null.map(string_of_timecode, pregap)),
              (postgap_metadata, null.map(string_of_timecode, postgap)),
              (index_zero_metadata, null.map(string_of_timecode, index_zero)),
              (index_zero_filename_metadata, index_zero_filename),
              ("tracknumber", string(position)),
              ("catalog", catalog),
              ...(
                null.defined(performer)
                ? [("artist", performer), ("albumartist", album_performer)] :
                  [("artist", album_performer)]

              ),

              ("title", title),
              ("isrc", isrc),
              ("album", album ?? playlist_album)
            ]
          )

        [([...cue_in, ...cue_out, ...meta, ...rem], filename), ...tracks]
      end
    end

    list.fold(add_track, [], list.rev(tracks))
  else
    []
  end
end

let settings.playlist.mime_types.basic =
  settings.make(
    description=
      "Mime-types used for guessing text-based playlists.",
    [
      {mime="audio/x-scpls", strict=true, parser=playlist.parse.scpls},
      {mime="application/x-cue", strict=true, parser=playlist.parse.cue},
      {mime="audio/x-mpegurl", strict=false, parser=playlist.parse.m3u},
      {mime="audio/mpegurl", strict=false, parser=playlist.parse.m3u},
      {mime="application/x-mpegURL", strict=false, parser=playlist.parse.m3u}
    ]
  )

%ifdef playlist.parse.xml
let settings.playlist.mime_types.xml =
  settings.make(
    description=
      "Mime-types used for guessing xml-based playlists.",
    list.map(
      (fun (mime) -> {mime=mime, strict=true, parser=playlist.parse.xml}),
      [
        "video/x-ms-asf",
        "audio/x-ms-asx",
        "text/xml",
        "application/xml",
        "application/smil",
        "application/smil+xml",
        "application/xspf+xml",
        "application/rss+xml"
      ]
    )
  )
%endif

# @flag hidden
let register_playlist_parsers =
  begin
    registered = ref(false)
    fun () ->
      begin
        if
          not registered()
        then
          parsers = settings.playlist.mime_types.basic()
%ifdef playlist.parse.xml
          parsers = [...parsers, ...settings.playlist.mime_types.xml()]
%endif
          list.iter(
            fun (entry) ->
              playlist.parse.register(
                format=entry.mime, strict=entry.strict, entry.parser
              ),
            parsers
          )
        end
        registered := true
      end
  end
on_start(register_playlist_parsers)

# @docof playlist.parse
def replaces playlist.parse(%argsof(playlist.parse), uri) =
  register_playlist_parsers()
  playlist.parse(%argsof(playlist.parse), uri)
end

# Default id assignment for playlists (the identifier is generated from the
# filename).
# @category Liquidsoap
# @flag hidden
# @param ~default Default name pattern when no useful name can be extracted from `uri`
# @param uri Playlist uri
def playlist.id(~default, uri) =
  basename = path.basename(uri)
  basename =
    if
      basename == "."
    then
      let l = r/\//g.split(uri)
      if l == [] then path.dirname(uri) else list.hd(list.rev(l)) end
    else
      basename
    end

  if
    basename == "."
  then
    string.id.default(default=default, null)
  else
    basename
  end
end

# Retrieve the list of files contained in a playlist.
# @category File
# @param ~mime_type Default MIME type for the playlist. `null` means automatic detection.
# @param ~timeout Timeout for resolving the playlist
# @param uri Path to the playlist
def playlist.files(~id=null, ~mime_type=null, ~timeout=null, uri) =
  id = id ?? playlist.id(default="playlist.files", uri)

  if
    file.is_directory(uri)
  then
    log.info(
      label=id,
      "Playlist is a directory."
    )
    files = file.ls(absolute=true, recursive=true, sorted=true, uri)
    files = list.filter(fun (f) -> not (file.is_directory(f)), files)
    files
  else
    pl = request.create(resolve_metadata=false, uri)
    result =
      if
        request.resolve(timeout=timeout, pl)
      then
        pl = request.filename(pl)
        files = playlist.parse(mime=mime_type, pl)

        def file_request(el) =
          let (meta, file) = el
          s =
            string.concat(
              separator=",",
              list.map(fun (el) -> "#{fst(el)}=#{string.quote(snd(el))}", meta)
            )

          if s == "" then file else "annotate:#{s}:#{file}" end
        end

        list.map.right(file_request, files)
      else
        log.important(
          label=id,
          "Couldn't read playlist: request resolution failed."
        )
        request.destroy(pl)

        error.raise(
          error.invalid,
          "Could not resolve uri: #{uri}"
        )
      end

    request.destroy(pl)
    result
  end
end

%ifdef native
let stdlib_native = native
%endif

# Play a list of files.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is \
#   only validated if the function returns true on it. The function is called \
#   before resolution, hence metadata will only be available for requests \
#   corresponding to local files. This is typically used to avoid repetitions, \
#   but be careful: if the function rejects all attempts, the playlist will \
#   enter into a consuming loop and stop playing anything.
# @param ~prefetch How many requests should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), \
#  or shuffle the playlist each time it is loaded, and play it in this order for a \
#  whole round ("randomize" mode), or pick a random file in the playlist each time \
#  ("random" mode).
# @param ~native Use native implementation, when available.
# @param ~on_loop Function executed when the playlist is about to loop.
# @param ~on_done Function executed when the playlist is finished.
# @param ~max_fail When this number of requests fail to resolve, the whole playlists is considered as failed and `on_fail` is called.
# @param ~on_fail Function executed when too many requests failed and returning the contents of a fixed playlist.
# @param ~timeout Timeout (in sec.) to resolve the request. Defaults to `settings.request.timeout` when `null`.
# @param ~cue_in_metadata Metadata for cue in points. Disabled if `null`.
# @param ~cue_out_metadata Metadata for cue out points. Disabled if `null`.
# @param playlist Playlist.
# @method reload Reload the playlist with given list of songs.
# @method remaining_files Songs remaining to be played.
def playlist.list(
  ~id=null,
  ~check_next=null,
  ~prefetch=null,
  ~loop=true,
  ~mode="normal",
  ~native=false,
  ~on_loop={()},
  ~on_done={()},
  ~max_fail=10,
  ~on_fail=null,
  ~timeout=null,
  ~cue_in_metadata=null("liq_cue_in"),
  ~cue_out_metadata=null("liq_cue_out"),
  playlist
) =
  ignore(native)
  id = string.id.default(default="playlist.list", id)
  mode =
    if
      not list.mem(mode, ["normal", "random", "randomize"])
    then
      log.severe(
        label=id,
        "Invalid mode: #{mode}"
      )
      "randomize"
    else
      mode
    end

  check_next = check_next ?? fun (_) -> true
  should_stop = ref(false)
  on_shutdown({should_stop.set(true)})
  on_fail =
    null.map(
      fun (on_fail) -> {if not should_stop() then on_fail() else [] end},
      on_fail
    )

  # Original playlist when loaded
  playlist_orig = ref(playlist)

  # Randomize the playlist if necessary
  def randomize(p) =
    if mode == "randomize" then list.shuffle(p) else p end
  end

  # Current remaining playlist
  playlist = ref(randomize(playlist))

  # A reference to know if the source has been stopped
  has_stopped = ref(false)

  # Delay the creation of next after the source because we need it to resolve
  # requests at the right content type.
  next_fun = ref(fun () -> null)

  def next() =
    f = next_fun()
    f()
  end

  # Instantiate the source
  default =
    fun () ->
      request.dynamic(
        id=id,
        prefetch=prefetch,
        timeout=timeout,
        retry_delay=1.,
        available={not has_stopped()},
        next
      )

  s =
%ifdef native
    if native then stdlib_native.request.dynamic(id=id, next) else default() end
%else
    default()
%endif

  # The reload function
  def reload(~empty_queue=true, p) =
    log.debug(
      label=id,
      "Reloading playlist."
    )
    playlist_orig := p
    playlist := randomize(playlist_orig())
    has_stopped := false
    if
      empty_queue
    then
      q = s.queue()
      s.set_queue([])
      list.iter(request.destroy, q)
      ignore(s.fetch())
    end
  end

  # When we have more than max_fail failures in a row, we wait for 1 second
  # before trying again in order to avoid infinite loops.
  failed_count = ref(0)
  failed_time = ref(0.)

  # The (real) next function
  def rec next() =
    if
      loop and list.is_empty(playlist())
    then
      on_loop()

      # The above function might have reloaded the playlist
      if
        list.is_empty(playlist())
      then
        playlist := randomize(playlist_orig())
      end
    end

    file =
      if
        list.length(playlist()) > 0
      then
        if
          mode == "random"
        then
          n = random.int(min=0, max=list.length(playlist()))
          list.nth(default="", playlist(), n)
        else
          ret = list.hd(default="", playlist())
          playlist := list.tl(playlist())
          ret
        end
      else
        # Playlist finished
        if
          not has_stopped()
        then
          has_stopped := true
          log.info(
            label=id,
            "Playlist stopped."
          )
          on_done()
        end

        ""
      end

    if
      file == "" or (failed_count() >= max_fail and time() < failed_time() + 1.)
    then
      # Playlist failed too many times recently, don't try next for now.
      null
    else
      log.debug(
        label=id,
        "Next song will be \"#{file}\"."
      )

      r =
        request.create(
          cue_in_metadata=cue_in_metadata,
          cue_out_metadata=cue_out_metadata,
          file
        )

      if
        check_next(r)
      then
        if
          not request.resolve(r)
        then
          log.info(
            label=id,
            "Could not resolve request: #{request.uri(r)}."
          )
          request.destroy(r)
          ref.incr(failed_count)

          # Playlist failed, call handler.
          if
            failed_count() < max_fail
          then
            log.info(
              label=id,
              "Playlist failed."
            )
            if
              null.defined(on_fail)
            then
              f = null.get(on_fail)
              reload(f())
            end
          end

          failed_time := time()
          (next() : request?)
        else
          failed_count := 0
          r
        end
      else
        log.info(
          label=id,
          "Request #{request.uri(r)} rejected by check_next."
        )

        request.destroy(r)
        next()
      end
    end
  end

  next_fun := next

  # List of songs remaining to be played
  def remaining_files() =
    playlist()
  end

  # Return
  s.{reload=reload, remaining_files=remaining_files}
end

# Read a playlist or a directory and play all files.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~check_next Function used to filter next tracks. A candidate track is \
#   only validated if the function returns true on it. The function is called \
#   before resolution, hence metadata will only be available for requests \
#   corresponding to local files. This is typically used to avoid repetitions, \
#   but be careful: if the function rejects all attempts, the playlist will \
#   enter into a consuming loop and stop playing anything.
# @param ~prefetch How many requests should be queued in advance.
# @param ~loop Loop on the playlist.
# @param ~mime_type Default MIME type for the playlist. `null` means automatic \
#  detection.
# @param ~mode Play the files in the playlist either in the order ("normal" mode), \
#  or shuffle the playlist each time it is loaded, and play it in this order for a \
#  whole round ("randomize" mode), or pick a random file in the playlist each time \
#  ("random" mode).
# @param ~native Use native implementation.
# @param ~max_fail When this number of requests fail to resolve, the whole playlists is considered as failed and `on_fail` is called.
# @param ~on_done Function executed when the playlist is finished.
# @param ~on_fail Function executed when too many requests failed and returning the contents of a fixed playlist.
# @param ~on_reload Callback called after playlist has reloaded.
# @param ~prefix Add a constant prefix to all requests. Useful for passing extra \
#  information using annotate, or for resolution through a particular protocol, \
#  such as replaygain.
# @param ~reload Amount of time (in seconds or rounds), when applicable, before \
#  which the playlist is reloaded; 0 means never.
# @param ~reload_mode Unit of the reload parameter, either "never" (never reload \
#  the playlist), "rounds", "seconds" or "watch" (reload the file whenever it is \
#  changed).
# @param ~register_server_commands Register corresponding server commands
# @param ~timeout Timeout (in sec.) to resolve the request. Defaults to `settings.request.timeout` when `null`.
# @param ~cue_in_metadata Metadata for cue in points. Disabled if `null`.
# @param ~cue_out_metadata Metadata for cue out points. Disabled if `null`.
# @param uri Playlist URI.
# @method reload Reload the playlist.
# @method length Length of the of the playlist (the number of songs it contains).
# @method remaining_files Songs remaining to be played.
def replaces playlist(
  ~id=null,
  ~check_next=null,
  ~prefetch=null,
  ~loop=true,
  ~max_fail=10,
  ~mime_type=null,
  ~mode="randomize",
  ~native=false,
  ~on_done={()},
  ~on_fail=null,
  ~on_reload=(fun (_) -> ()),
  ~prefix="",
  ~reload=0,
  ~reload_mode="seconds",
  ~timeout=null,
  ~cue_in_metadata=null("liq_cue_in"),
  ~cue_out_metadata=null("liq_cue_out"),
  ~register_server_commands=true,
  uri
) =
  id = id ?? playlist.id(default="playlist", uri)
  reload_mode =
    if
      not list.mem(reload_mode, ["never", "rounds", "seconds", "watch"])
    then
      log.severe(
        label=id,
        "Invalid reload mode: #{mode}"
      )
      "seconds"
    else
      reload_mode
    end

  round = ref(0)

  # URI of the current playlist
  playlist_uri = ref(uri)

  # List of files in the current playlist
  files = ref([])

  # The reload function
  reloader_ref = ref(fun (~empty_queue=true) -> ignore(empty_queue))

  failed_loads = ref(0)

  # The load function
  def load_playlist() =
    playlist_uri = path.home.unrelate(playlist_uri())

    log.info(
      label=id,
      "Reloading playlist."
    )

    files =
      try
        playlist.files(
          id=id, mime_type=mime_type, timeout=timeout, playlist_uri
        )
      catch err do
        log.info(
          label=id,
          "Playlist load failed: #{err}"
        )

        ref.incr(failed_loads)

        if
          failed_loads() < max_fail
        then
          []
        else
          log.info(
            label=id,
            "Maximum failures reached!"
          )
          on_fail = on_fail ?? fun () -> []
          on_fail()
        end
      end

    list.map.right(fun (file) -> prefix ^ file, files)
  end

  # Create the source
  files := load_playlist()

  # Reload when the playlist is done
  def on_loop() =
    reloader = reloader_ref()
    if
      reload_mode == "rounds" and reload > 0
    then
      round := round() + 1
      if
        round() >= reload
      then
        round := 0
        reloader()
      end
    end
  end

  watcher_reload_ref = ref(fun (_) -> ())

  def on_reload(uri) =
    watcher_reload = watcher_reload_ref()
    watcher_reload(uri)
    on_reload(uri)
  end

  s =
    playlist.list(
      id=id,
      check_next=check_next,
      prefetch=prefetch,
      loop=loop,
      max_fail=max_fail,
      mode=mode,
      native=native,
      on_done=on_done,
      on_loop=on_loop,
      on_fail=on_fail,
      timeout=timeout,
      cue_in_metadata=cue_in_metadata,
      cue_out_metadata=cue_out_metadata,
      files()
    )

  # The reload function
  def s.reload(~empty_queue=true, ~uri=null) =
    if
      failed_loads() < max_fail
    then
      if null.defined(uri) then playlist_uri := null.get(uri) end
      log(
        label=id,
        "Reloading playlist with URI #{playlist_uri()}."
      )
      files := load_playlist()
      s.reload(empty_queue=empty_queue, files())
      on_reload(playlist_uri())
    end
  end

  reloader_ref := s.reload

  def s.length() =
    list.length(files())
  end

  # Set up reloading for seconds and watch
  if
    reload_mode == "seconds" and reload > 0
  then
    n = float_of_int(reload)
    thread.run(delay=n, every=n, s.reload)
  elsif
    reload_mode == "watch"
  then
    watcher =
      if
        file.exists(playlist_uri())
      then
        ref(null(file.watch(playlist_uri(), s.reload)))
      else
        ref(null)
      end

    watched_uri = ref(playlist_uri())

    def watcher_reload(uri) =
      if
        uri != watched_uri()
      then
        w = watcher()
        if null.defined(w) then null.get(w).unwatch() end
        watched_uri := uri
        watcher :=
          if file.exists(uri) then file.watch(uri, s.reload) else null end
      end
    end

    watcher_reload_ref := watcher_reload

    def watcher_shutdown() =
      w = watcher()
      if null.defined(w) then null.get(w).unwatch() end
    end

    s.on_shutdown(synchronous=true, watcher_shutdown)
  end

  if
    register_server_commands
  then
    # Set up telnet commands
    s.register_command(
      description=
        "Skip current song in the playlist.",
      usage="skip",
      "skip",
      fun (_) ->
        begin
          s.skip()
          "OK"
        end
    )

    s.register_command(
      description=
        "Return up to 10 next URIs to be played.",
      usage="next",
      "next",
      fun (n) ->
        begin
          n = max(10, int_of_string(default=10, n))
          requests =
            list.fold(
              (fun (cur, el) -> list.length(cur) < n ? [...cur, el] : cur ),
              [],
              s.queue()
            )

          string.concat(
            separator="\n",
            list.map(
              (
                fun (r) ->
                  begin
                    m = request.metadata(r)
                    get = fun (lbl) -> list.assoc(default="?", lbl, m)
                    status = get("status")
                    uri = get("initial_uri")
                    "[#{status}] #{uri}"
                  end
              ),
              requests
            )
          )
        end
    )

    s.register_command(
      description=
        "Reload the playlist, unless already being loaded.",
      usage="reload",
      "reload",
      fun (_) ->
        begin
          s.reload()
          "OK"
        end
    )

    def uri_cmd(uri') =
      if
        uri' == ""
      then
        playlist_uri()
      else
        if
          reload_mode == "watch"
        then
          log.important(
            label=id,
            "Warning: the watched file is not updated for now when changing the \
             uri!"
          )
        end

        # TODO
        playlist_uri := uri'
        s.reload(uri=uri')
        "OK"
      end
    end

    s.register_command(
      description=
        "Print playlist URI if called without an argument, otherwise set a new \
         one and load it.",
      usage=
        "uri [<uri>]",
      "uri",
      uri_cmd
    )
  end

  s
end
