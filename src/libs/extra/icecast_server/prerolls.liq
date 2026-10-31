# Prerolls of icecast.server (prerolls.md). One store per process keeps loaded
# prerolls, the least recently used dropped past
# `settings.icecast.server.preroll_cache_size`. `icecast.server.preroll_cache`
# returns a view over it, built by the script or the configuration translation
# and passed to decisions: `view(location)` loads an operator's location,
# `view.backend(location)` one a backend chose.
let icecast.server.prerolls = ()

# Most recently used first.
# @flag hidden
let icecast.server.prerolls.store =
  ref(([] : [(string * {data: string, version: string, checked_at: float})]))

# Stores `entry` under `key` as the most recently used, drops the least recently
# used past the budget and returns the entry's bytes.
# @flag hidden
def icecast.server.prerolls.remember(key, entry) =
  budget = settings.icecast.server.preroll_cache_size()
  def rec within(total, entries) =
    list.case(
      entries,
      [],
      fun (entry, rest) ->
        begin
          total = total + string.bytes.length(snd(entry).data)
          total > budget ? [] : [entry, ...within(total, rest)]
        end
    )
  end

  store = icecast.server.prerolls.store
  atomic(
    fun () ->
      store := within(0, [(key, entry), ...list.assoc.remove(key, store())])
  )
  entry.data
end

# @flag hidden
def icecast.server.prerolls.is_url(location) =
  string.contains(prefix="http://", location)
  or string.contains(prefix="https://", location)
end

# A view over the process-wide preroll store that turns a location into bytes:
# a URL, fetched then revalidated with its ETag, or a local file, read again
# when it changes. `view(location)` gives `null` when the location cannot be
# loaded; `view.backend(location)` takes URLs only and gives `""` then.
# @category Internet
# @flag extra
# @param ~preroll_timeout Seconds a fetch may take.
# @param ~preroll_max_size Bytes above which a load fails.
# @param ~preroll_revalidate Seconds a fetched preroll is used before it is revalidated.
# @param ~directory Base directory of local paths.
def icecast.server.preroll_cache(
  ~preroll_timeout=10.,
  ~preroll_max_size=10485760,
  ~preroll_revalidate=7200.,
  ~directory="."
) =
  remember = icecast.server.prerolls.remember

  def too_large(size) =
    error.raise(
      error.invalid,
      "#{size} bytes, more than #{preroll_max_size}"
    )
  end

  def fetch(url, etag) =
    chunks = ref([])
    size = ref(0)
    def on_body_data(chunk) =
      if
        null.defined(chunk)
      then
        size := size() + string.bytes.length(null.get(chunk))
        if size() > preroll_max_size then too_large(size()) end
        chunks := [null.get(chunk), ...chunks()]
      end
    end

    # A raise in `on_body_data` aborts the transfer as an HTTP error.
    response =
      try
        http.get.stream(
          headers=etag == "" ? [] : [("If-None-Match", etag)],
          timeout=preroll_timeout,
          on_body_data=on_body_data,
          url
        )
      catch err do
        if size() > preroll_max_size then too_large(size()) end
        error.raise(err)
      end
    {
      status = response.status_code,
      data = string.concat(list.rev(chunks())),
      etag = list.assoc(default="", "etag", response.headers)
    }
  end

  def stored(data, version) =
    {data = data, version = version, checked_at = time()}
  end

  def fetch_new(url) =
    response = fetch(url, "")
    if
      response.status < 200 or response.status >= 300
    then
      error.raise(
        error.http,
        "status #{response.status}"
      )
    end
    remember(url, stored(response.data, response.etag))
  end

  def revalidate(url, entry) =
    # Marked fresh first, so that uses during the request take the copy.
    ignore(remember(url, stored(entry.data, entry.version)))
    try
      response = fetch(url, entry.version)
      if
        response.status == 304
      then
        remember(url, stored(entry.data, entry.version))
      elsif
        response.status >= 200 and response.status < 300
      then
        remember(url, stored(response.data, response.etag))
      else
        error.raise(
          error.http,
          "status #{response.status}"
        )
      end
    catch err do
      log.important(
        label="icecast.server",
        "Preroll #{url} not revalidated, keeping its copy: #{err.message}"
      )
      entry.data
    end
  end

  def load_url(url, cached) =
    if
      not null.defined(cached)
    then
      fetch_new(url)
    elsif
      time() - null.get(cached).checked_at < preroll_revalidate
    then
      remember(url, null.get(cached))
    else
      revalidate(url, null.get(cached))
    end
  end

  def load_file(path, cached) =
    version = string(file.mtime(path))
    if
      null.defined(cached) and null.get(cached).version == version
    then
      remember(path, null.get(cached))
    else
      size = file.size(path)
      if size > preroll_max_size then too_large(size) end
      remember(path, stored(file.contents(path), version))
    end
  end

  def local_path(location) =
    segments = string.split(separator="/", location)
    if
      list.mem("..", segments)
    then
      error.raise(
        error.invalid,
        "path outside #{directory}"
      )
    end
    path.concat(
      directory,
      string.concat(separator="/", list.filter(fun (s) -> s != "", segments))
    )
  end

  def load(location) =
    try
      is_url = icecast.server.prerolls.is_url(location)
      key = is_url ? location : (local_path(location))
      cached = list.assoc.nullable(key, icecast.server.prerolls.store())
      data = is_url ? load_url(key, cached) : (load_file(key, cached))
      size = string.bytes.length(data)
      if size > preroll_max_size then too_large(size) end
      (data : string?)
    catch err do
      log.important(
        label="icecast.server",
        "Preroll #{location} not loaded, listener admitted without it: #{
          err.message
        }"
      )
      null
    end
  end

  def backend(location) =
    if
      icecast.server.prerolls.is_url(location)
    then
      load(location) ?? ""
    else
      log.important(
        label="icecast.server",
        "Ignoring preroll #{location} chosen by a backend: not an http or https \
         URL."
      )
      ""
    end
  end

  load.{backend = backend, preroll_max_size = preroll_max_size}
end
