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
    let xml.parse ({lfm = {session = {name, key}}} :
      {lfm: {session: {name: string, key: string}}}
    ) = resp
    assert(name == username)
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
            track: string.{ xml_params: {corrected: string} },
            artist: string.{ xml_params: {corrected: string} },
            album: string?.{ xml_params: {corrected: string} },
            albumArtist: string?.{ xml_params: {corrected: string} },
            timestamp: string,
            ignoredMessage: {xml_params: {code: string}}
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
  ~timestamp=time(),
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
      ("timestamp", string(timestamp)),
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
              track: string.{ xml_params: {corrected: string} },
              artist: string.{ xml_params: {corrected: string} },
              album: string?.{ xml_params: {corrected: string} },
              albumArtist: string?.{ xml_params: {corrected: string} },
              timestamp: string,
              ignoredMessage: {xml_params: {code: string}}
            },
            xml_params: {ignored: string, accepted: string}
          },
          xml_params: {status: string}
        }
      }
    ) = resp

    log.info(
      label="audioscrobbler.api.track.scrobble",
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

let source_on_end = source.on_end
let source_on_metadata = source.on_metadata

# Submit songs using audioscrobbler, respecting the full protocol:
# First signal song as now playing when starting, and
# then submit song when it ends.
# @category Interaction
# @flag extra
# @param ~source Source for tracks. Should be one of: "broadcast", "user", "recommendation" or "unknown". Since liquidsoap is intended for radio broadcasting, this is the default. Sources other than user don't need duration to be set.
# @param ~delay Submit song when there is only this delay left, in seconds.
# @param ~force If remaining time is null, the song will be assumed to be skipped or cut, and not submitted. Set this to `true` to prevent this behavior
def audioscrobbler.submit(
  ~username,
  ~password,
  ~api_key=null(),
  ~api_secret=null(),
  ~delay=10.,
  ~force=false,
  s
) =
  session_key =
    audioscrobbler.auth(
      username=username,
      password=password,
      api_key=api_key,
      api_secret=api_secret
    )

  def apply_meta(fn, m) =
    def c(v) =
      v == "" ? null() : v
    end
    track = m["title"]
    album = c(m["album"])
    artist = m["artist"]
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

  def now_playing(m) =
    try
      apply_meta(audioscrobbler.api.track.updateNowPlaying, m)
    catch err do
      log.important(
        "Error while submitting nowplaying info for #{source.id(s)}: #{err}"
      )
    end
  end

  s = source_on_metadata(s, now_playing)
  f =
    fun (rem, m) ->
      # Avoid skipped songs
      if
        rem > 0. or force
      then
        try
          apply_meta(audioscrobbler.api.track.scrobble, m)
        catch err do
          log.important(
            "Error while submitting scrobble info for #{source.id(s)}: #{err}"
          )
        end
      else
        log(
          label="audioscrobbler.submit",
          level=4,
          "Remaining time null: will not submit song (song skipped ?)"
        )
      end
  source_on_end(s, delay=delay, f)
end
