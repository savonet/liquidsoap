# Refusals of icecast.server listener requests (listeners.md §3.1). A
# decision builds its `refuse` value with `icecast.server.refusal` or
# `icecast.server.redirect`; the server refuses a request past a listener limit
# with `icecast.server.refusal.limit`, whose status follows `compatibility`.

# @flag hidden
let icecast.server.reasons =
  [
    (
      301,
      "Moved Permanently"
    ),
    (302, "Found"),
    (
      303,
      "See Other"
    ),
    (
      307,
      "Temporary Redirect"
    ),
    (
      308,
      "Permanent Redirect"
    ),
    (
      400,
      "Bad Request"
    ),
    (
      401,
      "Authentication Required"
    ),
    (
      402,
      "Payment Required"
    ),
    (403, "Forbidden"),
    (
      404,
      "File Not Found"
    ),
    (
      429,
      "Too Many Requests"
    ),
    (
      451,
      "Unavailable For Legal Reasons"
    ),
    (
      500,
      "Internal Server Error"
    ),
    (
      503,
      "Service Unavailable"
    )
  ]

# The challenge of every `401` the server sends.
# @flag hidden
let icecast.server.realm =
  "Basic realm=\"Icecast2 Server\""

# Refusal of a listener request, the `refuse` value of a listener decision. A
# `401` without a `WWW-Authenticate` header gets Icecast's default challenge.
# @category Internet
# @flag extra
# @param ~reason Reason phrase. Defaults to Icecast's phrase for `status`.
# @param ~headers Headers of the refusal.
# @param status Status of the refusal, 300 to 599.
def icecast.server.refusal(~reason=null, ~headers=[], status) =
  has_challenge =
    list.exists(
      fun (header) ->
        string.case(lower=true, fst(header)) == "www-authenticate",
      headers
    )
  challenge =
    status == 401 and not has_challenge
      ? [("WWW-Authenticate", icecast.server.realm)]
      : []
  {
    status = status,
    reason =
      reason ?? list.assoc(default="Refused", status, icecast.server.reasons),
    headers = [...headers, ...challenge]
  }
end

# Redirect of a listener request to `location`, the `refuse` value of a
# listener decision.
# @category Internet
# @flag extra
# @param status Redirect status, such as `302` or `307`.
# @param location URL the listener is sent to.
def icecast.server.redirect(status, location) =
  icecast.server.refusal(status, headers=[("Location", location)])
end

# Refusal of a listener request past `max_clients` or `max_listeners` under
# `compatibility`.
# @flag hidden
def icecast.server.refusal.limit(compatibility) =
  status = compatibility == "icecast-2.4" or compatibility == "kh" ? 403 : 503
  icecast.server.refusal(status)
end
