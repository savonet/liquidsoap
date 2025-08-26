let spinitron = {submit=()}

# Submit a track to the spinitron track system
# and return the raw response.
# @category Interaction
# @flag extra
# @param ~api_key API key
def spinitron.submit.raw(
  ~host="https://spinitron.com/api",
  ~api_key,
  ~live=false,
  ~start=null,
  ~duration=null,
  ~artist,
  ~release=null,
  ~label=null,
  ~genre=null,
  ~song,
  ~composer=null,
  ~isrc=null
) =
  params = [("song", song), ("artist", artist)]

  def fold_optional_string_params(params, param) =
    let (label, param) = param
    if
      null.defined(param)
    then
      [(label, null.get(param)), ...params]
    else
      params
    end
  end

  params =
    list.fold(
      fold_optional_string_params,
      params,
      [
        ("live", null.map(fun (b) -> b ? "1" : "0" , (live : bool?))),
        ("start", start),
        ("duration", null.map(string, (duration : int?))),
        ("release", release),
        ("label", label),
        ("genre", genre),
        ("composer", composer),
        ("isrc", isrc)
      ]
    )

  def encode_param(param) =
    let (label, param) = param
    "#{label}=#{url.encode(param)}"
  end

  params = string.concat(separator="&", list.map(encode_param, params))

  http.post(
    data=params,
    headers=[
      ("Accept", "application/json"),
      ("Content-Type", "application/x-www-form-urlencoded"),
      (
        "Authorization",
        "Bearer #{(api_key : string)}"
      )
    ],
    "#{host}/spins"
  )
end

# Submit a track to the spinitron track system
# and return the parsed response
# @category Interaction
# @flag extra
# @param ~api_key API key
def replaces spinitron.submit(%argsof(spinitron.submit.raw)) =
  resp = spinitron.submit.raw(%argsof(spinitron.submit.raw))

  if
    resp.status_code == 201
  then
    let json.parse (resp :
      {
        id: int,
        playlist_id: int,
        "start" as spin_start: string,
        "end" as spin_end: string?,
        duration: int?,
        timezone: string?,
        image: string?,
        classical: bool?,
        artist: string,
        "artist-custom" as artist_custom: string?,
        composer: string?,
        release: string?,
        "release-custom" as release_custom: string?,
        va: bool?,
        label: string?,
        "label-custom" as label_custom: string?,
        released: int?,
        medium: string?,
        genre: string?,
        song: string,
        note: string?,
        request: bool?,
        local: bool?,
        new: bool?,
        work: string?,
        conductor: string?,
        performers: string?,
        ensemble: string?,
        "catalog-number" as catalog_number: string?,
        isrc: string?,
        upc: string?,
        iswc: string?,
        "_links" as links: {self: {href: string}?, playlist: {href: string}?}?
      }
    ) = resp

    resp
  elsif
    resp.status_code == 422
  then
    let json.parse (errors : [{field: string, message: string}]) = resp

    errors =
      list.map(
        fun (p) ->
          begin
            let {field, message} = p
            "#{field}: #{message}"
          end,
        errors
      )

    errors =
      string.concat(
        separator=", ",
        errors
      )

    error.raise(
      error.raise(
        error.http,
        "Invalid fields: #{errors}"
      )
    )
  else
    let json.parse ({name, message, code, status, type} :
      {name: string, message: string, code: int, status: int, type: string?}
    ) = resp

    type = type ?? "undefined"

    error.raise(
      error.raise(
        error.http,
        "#{name}: #{message} (code: #{code}, status: #{status}, type: #{type})"
      )
    )
  end
end

# Submit a spin using the given metadata to the spinitron track system
# and return the parsed response. `artist` and `song` (or `title`) must
# be present either as metadata or as optional argument.
# @category Interaction
# @flag extra
# @param m Metadata to submit. Overrides optional arguments when present.
# @param ~mapper Metadata mapper that can be used to map metadata fields to spinitron's expected. \
#                Returned metadata are added to the submitted metadata. By default, `title` is \
#                mapped to `song` and `album` to `release` if neither of those passed otherwise.
# @param ~api_key API key
def spinitron.submit.metadata(
  %argsof(spinitron.submit[!artist,!song]),
  ~mapper=(
    fun (m) ->
      [
        ...(m["song"] != "" or m["title"] == "" ? [] : [("song", m["title"])] ),
        ...(
          m["release"] != "" or m["album"] == ""
          ? [] : [("release", m["album"])]
        )
      ]
  ),
  ~artist=null,
  ~song=null,
  m
) =
  m = [...m, ...mapper(m)]

  def conv_opt_arg(convert, label, default) =
    list.assoc.mem(label, m) ? convert(m[label]) : default
  end

  opt_arg =
    fun (label, default) -> conv_opt_arg(fun (x) -> null(x), label, default)

  live = conv_opt_arg(bool_of_string, "live", live)
  start = opt_arg("start", start)
  duration = conv_opt_arg(int_of_string, "duration", duration)
  artist = opt_arg("artist", artist)
  release = opt_arg("release", release)
  label = opt_arg("label", label)
  genre = opt_arg("genre", genre)
  song = opt_arg("song", song)
  composer = opt_arg("composer", composer)
  isrc = opt_arg("isrc", isrc)

  if
    artist == null or song == null
  then
    error.raise(
      error.invalid,
      "Both \"artist\" and \"song\" (or \"title\" metadata) must be provided!"
    )
  end

  artist = null.get(artist)
  song = null.get(song)

  res = spinitron.submit(%argsof(spinitron.submit))

  print(res)

  res
end

# Specialized version of `s.on_metadata` that submits spins using
# the source's metadata to the spinitron track system. `artist` and `song`
# (or `title`) must be present either as metadata or as optional argument.
# @category Interaction
# @flag extra
# @param m Metadata to submit. Overrides optional arguments when present.
# @param ~api_key API key
def spinitron.submit.on_metadata(%argsof(spinitron.submit.metadata), s) =
  def on_metadata(m) =
    if
      m["title"] == "" and m["song"] == ""
    then
      log.severe(
        label=source.id(s),
        "Field \"song\" or \"title\" missing, skipping metadata spinitron \
         submission."
      )
    elsif
      m["artist"] == ""
    then
      log.severe(
        label=source.id(s),
        "Field \"artist\" missing, skipping metadata spinitron submission."
      )
    else
      try
        ignore(spinitron.submit.metadata(%argsof(spinitron.submit.metadata), m))
        log.important(
          label=source.id(s),
          "Successfully submitted spin from metadata"
        )
      catch err do
        log.severe(
          label=source.id(s),
          "Error while submitting spin from metadata: #{err}"
        )
      end
    end
  end

  source.methods(s).on_metadata(synchronous=false, on_metadata)
end
