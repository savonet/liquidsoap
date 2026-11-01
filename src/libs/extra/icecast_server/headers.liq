# Header entries of icecast.server replies (parameters.md §4.2, listeners.md
# §3.2). An entry is `{name, value?, compute?, status?}`. Parameter validation
# calls `invalid_entry` on each configured entry; the server builds every
# listener reply's header list with `reply`, over the decision's and identity
# headers (turned into entries by `of_pairs`), then the server's and the mount's
# entries. `cors` is the ready-made CORS entry set.
let icecast.server.headers = ()

# Whether `name` is an HTTP token.
# @flag hidden
def icecast.server.headers.is_token(name) =
  r/^[!#$%&'*+.^_`|~0-9A-Za-z-]+$/.test(name)
end

# Whether `value` can be written in a header line.
# @flag hidden
def icecast.server.headers.is_line(value) =
  not r/[\r\n]/.test(value)
end

# Why a configured header entry is invalid, `null` when it is valid.
# @flag hidden
def icecast.server.headers.invalid_entry(entry) =
  value = entry?.value
  status = entry?.status
  if
    null.defined(value) == null.defined(entry?.compute)
  then
    "header #{entry.name}: exactly one of value and compute is required"
  elsif
    not icecast.server.headers.is_token(entry.name)
  then
    "header name #{string.quote(entry.name)} is not an HTTP token"
  elsif
    null.defined(value) and not icecast.server.headers.is_line(null.get(value))
  then
    "header #{entry.name}: value has a line break"
  elsif
    null.defined(status) and (null.get(status) < 100 or null.get(status) > 599)
  then
    "header #{entry.name}: status #{null.get(status)} is not in 100 to 599"
  else
    null
  end
end

# Header list with lowercase names, as harbor's listener and login records
# have them.
# @flag hidden
def icecast.server.headers.lowercase(headers) =
  list.map(fun (h) -> (string.case(lower=true, fst(h)), snd(h)), headers)
end

# Header pairs as fixed entries.
# @flag hidden
def icecast.server.headers.of_pairs(headers) =
  list.map(fun (header) -> {name = fst(header), value = snd(header)}, headers)
end

# Final header list of a reply with `status` to `request`: the first entry of a
# name wins, an entry that does not apply or computes `null` sets no name, and a
# computed header that raises or cannot be written is dropped and logged.
# @flag hidden
def icecast.server.headers.reply(~request, ~status, entries) =
  def entry_value(entry) =
    if
      null.defined(entry?.status) and null.get(entry?.status) != status
    then
      null
    elsif
      null.defined(entry?.value)
    then
      entry?.value
    else
      compute = null.get(entry?.compute)
      value =
        try
          compute(request, status)
        catch err do
          log.important(
            label="icecast.server",
            "Dropping computed header #{string.quote(entry.name)}: #{
              err.message
            }"
          )
          null
        end
      if
        null.defined(value)
        and not (
          icecast.server.headers.is_token(entry.name)
          and icecast.server.headers.is_line(null.get(value))
        )
      then
        log.important(
          label="icecast.server",
          "Dropping computed header #{string.quote(entry.name)}: invalid name or \
           value with a line break."
        )
        null
      else
        value
      end
    end
  end

  def add_entry(headers, entry) =
    name = string.case(lower=true, entry.name)
    if
      list.exists(
        fun (header) -> string.case(lower=true, fst(header)) == name,
        headers
      )
    then
      headers
    else
      value = entry_value(entry)
      null.defined(value)
        ? [...headers, (entry.name, null.get(value))]
        : headers
    end
  end

  list.fold(add_entry, [], entries)
end

# CORS header entries for `http_headers`, computed from the request's `Origin`
# and its server's `compatibility`. Under `"rsas"`: the origin and RSAS's
# exposed headers. Under the other values: Icecast 2.5's set, sent only to a
# request with an `Origin`, the origin on a 2xx reply and `null` on a 4xx or
# 5xx one.
# @category Internet
# @flag extra
# @param ~origins Origins allowed. Empty: any origin.
# @param ~expose Header names appended to `Access-Control-Expose-Headers`.
def icecast.server.headers.cors(~origins=[], ~expose=[]) =
  def origin(request) =
    list.assoc.nullable("origin", request.headers)
  end

  def is_rsas(request) =
    request.compatibility == "rsas"
  end

  def is_2xx(status) =
    status >= 200 and status < 300
  end

  def icecast_only(f) =
    fun (request, status) ->
      if
        is_rsas(request) or not null.defined(origin(request))
      then
        null
      else
        f(status)
      end
  end

  def allow_origin(request, status) =
    o = origin(request)
    if
      not null.defined(o)
      or (origins != [] and not list.mem(null.get(o), origins))
    then
      null
    elsif
      is_rsas(request) or is_2xx(status)
    then
      o
    elsif
      status >= 400 and status < 600
    then
      "null"
    else
      null
    end
  end

  rsas_exposed =
    [
      "Content-Type",
      "icy-metaint",
      "icy-br",
      "ice-audio-info",
      "icy-description",
      "icy-genre",
      "icy-name",
      "icy-samplerate",
      "icy-url"
    ]
  icecast_exposed =
    [
      "content-range",
      "icy-br",
      "icy-description",
      "icy-genre",
      "icy-name",
      "icy-pub",
      "icy-url"
    ]

  def expose_headers(request, _) =
    names =
      if
        is_rsas(request)
      then
        rsas_exposed
      elsif
        null.defined(origin(request))
      then
        icecast_exposed
      else
        []
      end
    names == []
      ? null
      : (
        string.concat(
          separator=", ",
          [...names, ...expose]
        )
      )
  end

  [
    {name = "Access-Control-Allow-Origin", compute = allow_origin},
    {
      name = "Access-Control-Allow-Methods",
      compute = icecast_only(fun (status) -> is_2xx(status) ? "GET" : null)
    },
    {name = "Access-Control-Expose-Headers", compute = expose_headers},
    {name = "Access-Control-Max-Age", compute = icecast_only(fun (_) -> "300")},
    {
      name = "Access-Control-Allow-Headers",
      compute =
        icecast_only(
          fun (_) ->
            "range, if-range"
        )
    }
  ]
end
