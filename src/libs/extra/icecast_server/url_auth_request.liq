# Form fields both URL authentication requests start with, up to `agent`
# (url-auth.md §2, §8).
# @flag hidden
def icecast.server.url_auth.fields(~action, ~user, request) =
  [
    ("action", action),
    ("server", request.server.hostname),
    ("port", string(request.server.port)),
    ("server-instance", request.server.instance),
    ("client", string(request.id)),
    (
      "mount",
      request.query == "" ? request.mount : "#{request.mount}?#{request.query}"
    ),
    ("user", user),
    ("pass", request.password),
    ("ip", request.address),
    ("agent", list.assoc(default="-", "user-agent", request.headers))
  ]
end

# The `listener_add` fields after `agent`.
# @flag hidden
def icecast.server.url_auth.add_fields(~config, request) =
  referrer = list.assoc(default="", "referer", request.headers)
  count = string(request.listeners)
  forwarded =
    list.filter_map(
      fun (name) ->
        null.map(
          fun (value) -> ("#{config.header_prefix}#{name}", value),
          list.assoc.nullable(string.case(lower=true, name), request.headers)
        ),
      config.headers
    )
  [
    ("protocol", "http"),
    ("referrer", referrer),
    ("listener-count", count),
    ("referer", referrer),
    ("listeners", count),
    ...forwarded
  ]
end

# The `listener_remove` fields after `agent`, from the release record.
# @flag hidden
def icecast.server.url_auth.remove_fields(release) =
  artist = list.assoc(default="", "artist", release.metadata)
  title = list.assoc(default="", "title", release.metadata)
  song =
    artist == ""
      ? title
      : (
        title == ""
          ? artist
          : "#{artist} - #{title}"
      )
  [
    ("listener-count", string(release.listeners)),
    ("song", song),
    ("sent", string(release.bytes_sent)),
    ("duration", string(int(release.duration)))
  ]
end

# POSTs `fields` to `url` and returns `{status, headers, body}`, header names
# lowercased. Credentials follow url-auth.md §3, `request` giving the client's
# own; those embedded in `url` are sent by the HTTP client. A transport failure
# raises `error.http`.
# @flag hidden
def icecast.server.url_auth.send(~config, ~fields, request, url) =
  credentials =
    if
      r/^[a-zA-Z][a-zA-Z0-9+.-]*:\/\/[^\/?#@]*@/.test(url)
    then
      null
    elsif
      config.username != "" and config.password != ""
    then
      "#{config.username}:#{config.password}"
    elsif
      request.user != "" or request.password != ""
    then
      "#{request.user}:#{request.password}"
    else
      null
    end
  authorization =
    null.defined(credentials)
      ? [
        (
          "Authorization",
          "Basic #{string.base64.encode(null.get(credentials))}"
        )
      ]
      : []
  response =
    http.post(
      redirect=false,
      timeout=(config.timeout : float),
      headers=[
        ("Content-Type", "application/x-www-form-urlencoded"),
        ...authorization
      ],
      data=http.www_form_urlencoded(fields),
      url
    )

  # The HTTP client answers a transport failure (refused, timed out,
  # unresolved, TLS) with a status from 520 to 526 and no header.
  if
    response.status_code >= 520
    and response.status_code <= 526
    and list.is_empty(response.headers)
  then
    error.raise(
      error.http,
      "#{response.status_code} #{response.status_message}"
    )
  end
  {
    status = response.status_code,
    headers = icecast.server.headers.lowercase(response.headers),
    body = (response : string)
  }
end

# POSTs the fields of `action` about the listener `request` to `url`, then
# `fields`.
# @flag hidden
def icecast.server.url_auth.post(
  ~config,
  ~action,
  ~user,
  ~fields,
  request,
  url
) =
  icecast.server.url_auth.send(
    config=config,
    fields=[
      ...icecast.server.url_auth.fields(action=action, user=user, request),
      ...fields
    ],
    request,
    url
  )
end
