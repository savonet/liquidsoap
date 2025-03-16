let error.audioscrobbler = error.register("audioscrobbler")

let settings.audioscrobbler =
  settings.make.void(
    "Audioscrobbler settings"
  )

let settings.audioscrobbler.api_key =
  settings.make(
    description=
      "Default API key for audioscrobbler",
    ""
  )

let settings.audioscrobbler.api_secret =
  settings.make(
    description=
      "Default API secret for audioscrobbler",
    ""
  )

audioscrobbler = ()

def audioscrobbler.request(
  ~base_url="http://ws.audioscrobbler.com/2.0",
  ~api_key=null(),
  ~api_secret=null(),
  params
) =
  api_key = api_key ?? settings.audioscrobbler.api_key()
  api_secret = api_secret ?? settings.audioscrobbler.api_secret()

  if
    api_key == "" or api_secret == ""
  then
    error.raise(
      error.audioscrobbler,
      "`api_key` or `api_secret` missing!"
    )
  end

  params = [("api_key", api_key), ...params]

  sig_params = list.sort(fun (v, v') -> string.compare(fst(v), fst(v')), params)
  sig_params = list.map(fun (v) -> "#{fst(v)}#{(snd(v) : string)}", sig_params)
  sig_params = string.concat(separator="", sig_params)

  api_sig = string.digest("#{sig_params}#{api_secret}")

  http.post(
    base_url,
    headers=[("Content-Type", "application/x-www-form-urlencoded")],
    data=http.www_form_urlencoded([...params, ("api_sig", api_sig)])
  )
end

def audioscrobbler.check_response(resp) =
  let xml.parse ({lfm = {xml_params = {status}}} :
    {lfm: {xml_params: {status: string}}}
  ) = resp
  if
    (status == "failed")
  then
    error_ref = error
    let xml.parse ({lfm = {error = {xml_params = {code}}}} :
      {lfm: {error: string.{ xml_params: {code: int} }}}
    ) = resp
    error_ref.raise(
      error_ref.audioscrobbler,
      "Error #{code}: #{error}"
    )
  end
end

def audioscrobbler.auth(
  ~username,
  ~password,
  ~api_key=null(),
  ~api_secret=null()
) =
  resp =
    audioscrobbler.request(
      api_key=api_key,
      api_secret=api_secret,
      [
        ("method", "auth.getMobileSession"),
        ("username", username),
        ("password", password)
      ]
    )

  audioscrobbler.check_response(resp)

  try
    let xml.parse ({lfm = {session = {key}}} :
      {lfm: {session: {name: string, key: string}}}
    ) = resp
    key
  catch err do
    error.raise(
      error.invalid,
      "Invalid response: #{resp}, error: #{err}"
    )
  end
end

let audioscrobbler.api = {track=()}

# Submit a track to the audioscrobbler
# `track.updateNowPlaying` API.
# @category Interaction
def audioscrobbler.api.track.updateNowPlaying(
  ~username,
  ~password,
  ~session_key=null(),
  ~api_key=null(),
  ~api_secret=null(),
  ~artist,
  ~track,
  ~album=null(),
  ~context=null(),
  ~trackNumber=null(),
  ~mbid=null(),
  ~albumArtist=null(),
  ~duration=null()
) =
  session_key =
    session_key
      ??
        audioscrobbler.auth(
          username=username,
          password=password,
          api_key=api_key,
          api_secret=api_secret
        )

  params =
    [
      ("track", track),
      ("artist", artist),
      ...(null.defined(album) ? [("album", null.get(album))] : [] ),
      ...(null.defined(context) ? [("context", null.get(context))] : [] ),
      ...(
        null.defined(trackNumber)
        ? [("trackNumber", string((null.get(trackNumber) : int)))] : []
      ),
      ...(null.defined(mbid) ? [("mbid", null.get(mbid))] : [] ),
      ...(
        null.defined(albumArtist)
        ? [("albumArtist", null.get(albumArtist))] : []
      ),
      ...(
        null.defined(duration)
        ? [("duration", string((null.get(duration) : int)))] : []
      )
    ]

  log.info(
    label="audioscrobbler.api.track.updateNowPlaying",
    "Submitting updateNowPlaying with: #{params}"
  )

  resp =
    audioscrobbler.request(
      api_key=api_key,
      api_secret=api_secret,
      [...params, ("method", "track.updateNowPlaying"), ("sk", session_key)]
    )

  audioscrobbler.check_response(resp)

  try
    let xml.parse (v :
      {
        lfm: {
          nowplaying: {
            track: string.{ xml_params: {corrected: int} },
            artist: string.{ xml_params: {corrected: int} },
            album: string?.{ xml_params: {corrected: int} },
            albumArtist: string?.{ xml_params: {corrected: int} },
            ignoredMessage: {xml_params: {code: int}}
          },
          xml_params: {status: string}
        }
      }
    ) = resp

    log.info(
      label="audioscrobbler.api.track.updateNowPlaying",
      "Done submitting updateNowPlaying with: #{params}"
    )

    v
  catch err do
    error.raise(
      error.invalid,
      "Invalid response: #{resp}, error: #{err}"
    )
  end
end

# @flag hidden
def audioscrobbler.api.apply_meta(
  ~name,
  ~username,
  ~password,
  ~api_key,
  ~api_secret,
  ~session_key,
  fn,
  m
) =
  def c(v) =
    v == "" ? null() : v
  end
  track = m["title"]
  artist = m["artist"]

  if
    track == "" or artist == ""
  then
    log.info(
      label=name,
      "No artist or track present: metadata submission disabled!"
    )
  else
    album = c(m["album"])
    trackNumber =
      try
        null.map(int_of_string, c(m["tracknumber"]))
      catch _ do
        null()
      end
    albumArtist = c(m["albumartist"])
    ignore(
      fn(
        username=username,
        password=password,
        api_key=api_key,
        api_secret=api_secret,
        session_key=session_key,
        track=track,
        artist=artist,
        album=album,
        trackNumber=trackNumber,
        albumArtist=albumArtist
      )
    )
  end
end

# Submit a track using its metadata to the audioscrobbler
# `track.updateNowPlaying` API.
# @category Interaction
def audioscrobbler.api.track.updateNowPlaying.metadata(
  ~username,
  ~password,
  ~session_key=null(),
  ~api_key=null(),
  ~api_secret=null(),
  m
) =
  audioscrobbler.api.apply_meta(
    username=username,
    password=password,
    session_key=session_key,
    api_key=api_key,
    api_secret=api_secret,
    name="audioscrobbler.api.track.updateNowPlaying",
    audioscrobbler.api.track.updateNowPlaying,
    m
  )
end

# Submit a track to the audioscrobbler
# `track.scrobble` API.
# @category Interaction
def audioscrobbler.api.track.scrobble(
  ~username,
  ~password,
  ~session_key=null(),
  ~api_key=null(),
  ~api_secret=null(),
  ~artist,
  ~track,
  ~timestamp=null(),
  ~album=null(),
  ~context=null(),
  ~streamId=null(),
  ~chosenByUser=true,
  ~trackNumber=null(),
  ~mbid=null(),
  ~albumArtist=null(),
  ~duration=null()
) =
  session_key =
    session_key
      ??
        audioscrobbler.auth(
          username=username,
          password=password,
          api_key=api_key,
          api_secret=api_secret
        )

  params =
    [
      ("track", track),
      ("artist", artist),
      ("timestamp", string(timestamp ?? time())),
      ...(null.defined(album) ? [("album", null.get(album))] : [] ),
      ...(null.defined(context) ? [("context", null.get(context))] : [] ),
      ...(null.defined(streamId) ? [("streamId", null.get(streamId))] : [] ),

      ("chosenByUser", chosenByUser ? "1" : "0" ),
      ...(
        null.defined(trackNumber)
        ? [("trackNumber", string((null.get(trackNumber) : int)))] : []
      ),
      ...(null.defined(mbid) ? [("mbid", null.get(mbid))] : [] ),
      ...(
        null.defined(albumArtist)
        ? [("albumArtist", null.get(albumArtist))] : []
      ),
      ...(
        null.defined(duration)
        ? [("duration", string((null.get(duration) : int)))] : []
      )
    ]

  log.info(
    label="audioscrobbler.api.track.scrobble",
    "Submitting updateNowPlaying with: #{params}"
  )

  resp =
    audioscrobbler.request(
      api_key=api_key,
      api_secret=api_secret,
      [...params, ("method", "track.scrobble"), ("sk", session_key)]
    )

  audioscrobbler.check_response(resp)

  try
    let xml.parse (v :
      {
        lfm: {
          scrobbles: {
            scrobble: {
              track: string.{ xml_params: {corrected: int} },
              artist: string.{ xml_params: {corrected: int} },
              album: string?.{ xml_params: {corrected: int} },
              albumArtist: string?.{ xml_params: {corrected: int} },
              timestamp: float,
              ignoredMessage: {xml_params: {code: int}}
            },
            xml_params: {ignored: int, accepted: int}
          },
          xml_params: {status: string}
        }
      }
    ) = resp

    log.info(
      label="audioscrobbler.api.track.scrobble",
      "Done submitting scrobble with: #{params}"
    )

    v
  catch err do
    error.raise(
      error.invalid,
      "Invalid response: #{resp}, error: #{err}"
    )
  end
end

# Submit a track to the audioscrobbler
# `track.scrobble` API using its metadata.
# @category Interaction
def audioscrobbler.api.track.scrobble.metadata(
  ~username,
  ~password,
  ~session_key=null(),
  ~api_key=null(),
  ~api_secret=null(),
  m
) =
  audioscrobbler.api.apply_meta(
    username=username,
    password=password,
    session_key=session_key,
    api_key=api_key,
    api_secret=api_secret,
    name="audioscrobbler.api.track.scrobble",
    audioscrobbler.api.track.scrobble,
    m
  )
end

# Submit songs using audioscrobbler, respecting the full protocol:
# First signal song as now playing when starting, and
# then submit song when it ends.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~delay Submit song when there is only this delay left, in seconds.
# @param ~force If remaining time is null, the song will be assumed to be skipped or cut, and not submitted. Set this to `true` to prevent this behavior
# @param ~metadata_preprocessor Metadata pre-processor callback. Can be used to change metadata on-the-fly before sending to nowPlaying/scrobble. If returning an empty metadata, nothing is sent at all.
def audioscrobbler.submit(
  ~username,
  ~password,
  ~api_key=null(),
  ~api_secret=null(),
  ~delay=10.,
  ~force=false,
  ~metadata_preprocessor=fun (m) -> m,
  s
) =
  session_key =
    audioscrobbler.auth(
      username=username,
      password=password,
      api_key=api_key,
      api_secret=api_secret
    )

  def now_playing(m) =
    thread.run(
      delay=0.,
      {
        try
          audioscrobbler.api.track.updateNowPlaying.metadata(
            username=username,
            password=password,
            api_key=api_key,
            api_secret=api_secret,
            session_key=session_key,
            metadata_preprocessor(m)
          )
        catch err do
          log.important(
            "Error while submitting nowplaying info for #{source.id(s)}: #{err}"
          )
        end
      }
    )
  end

  s = source.on_metadata(s, now_playing)
  f =
    fun (rem, m) ->
      # Avoid skipped songs
      if
        rem > 0. or force
      then
        thread.run(
          delay=0.,
          {
            try
              audioscrobbler.api.track.scrobble.metadata(
                username=username,
                password=password,
                api_key=api_key,
                api_secret=api_secret,
                session_key=session_key,
                metadata_preprocessor(m)
              )
            catch err do
              log.important(
                "Error while submitting scrobble info for #{source.id(s)}: #{
                  err
                }"
              )
            end
          }
        )
      else
        log(
          label="audioscrobbler.submit",
          level=4,
          "Remaining time null: will not submit song (song skipped ?)"
        )
      end
  source.on_end(s, delay=delay, f)
end
