# The `http_headers` of a scope, the root or a `<mount>` (icecast-config.md
# §8): its `<http-headers>` entries, then its RSAS `<custom-headers>` entries.
# Every entry has the four fields of parameters.md §4.2, unset ones `null`, so
# that static and computed entries share one type.

# @flag hidden
def icecast.server.config.static_header(~status=null, name, value) =
  {name = name, value = value, compute = null, status = status}
end

# @flag hidden
def icecast.server.config.computed_header(~status=null, entry) =
  {name = entry.name, value = null, compute = entry.compute, status = status}
end

# The CORS set of a scope: the one its RSAS `<cors>` block builds, with the
# helper's defaults when it has none.
# @flag hidden
def icecast.server.config.cors_set(block) =
  def texts(tag) =
    list.map(string.trim, list.filter_map(xml.string, xml.children(tag, block)))
  end

  icecast.server.headers.cors(
    origins=texts("allow-origin"),
    expose=texts("expose-headers")
  )
end

# The entry of one `<header>`, as a list holding it, or `[]` when skipped.
# @flag hidden
def icecast.server.config.header(~compatibility, ~cors, header) =
  warn = icecast.server.config.warn
  name = xml.string(attribute="name", header)
  value = xml.string(attribute="value", header)
  status = xml.int(attribute="status", header)
  kind =
    string.case(lower=true, xml.string(attribute="type", header) ?? "static")
  cors_type = list.mem(compatibility, ["icecast-2.5", "rsas"])
  if
    not null.defined(name)
  then
    warn(
      "Ignoring a <header> without name."
    )
    []
  else
    name = null.get(name)
    if
      kind != "static" and kind != "cors"
    then
      warn(
        "Header #{name}: unknown type #{string.quote(kind)}, sending it as \
         static."
      )
    end
    kind = kind == "cors" ? "cors" : "static"
    if
      kind == "cors" and not cors_type
    then
      warn(
        "Header #{name}: no cors type under #{compatibility}, sending it as \
         static."
      )
    end
    if
      null.defined(value)
    then
      [icecast.server.config.static_header(status=status, name, value)]
    elsif
      kind == "cors" and cors_type
    then
      lower = string.case(lower=true, name)
      matching =
        list.filter(
          fun (entry) -> string.case(lower=true, entry.name) == lower,
          cors
        )
      if
        list.is_empty(matching)
      then
        warn(
          "Ignoring cors header #{name}: not in the CORS set and no value."
        )
      end
      list.map(
        fun (entry) ->
          icecast.server.config.computed_header(status=status, entry),
        matching
      )
    else
      warn(
        "Ignoring header #{name}: no value."
      )
      []
    end
  end
end

# @flag hidden
def icecast.server.config.headers(~compatibility, scope) =
  custom = xml.child("custom-headers", scope)
  block = xml.child("cors", custom)
  cors = icecast.server.config.cors_set(block)
  http_headers =
    list.flatten(
      list.map(
        fun (header) ->
          icecast.server.config.header(
            compatibility=compatibility,
            cors=cors,
            header
          ),
        xml.children("header", xml.child("http-headers", scope))
      )
    )
  added =
    list.filter_map(
      fun (header) ->
        begin
          name = xml.string(attribute="name", header)
          value = xml.string(attribute="value", header)
          null.defined(name) and null.defined(value)
            ? icecast.server.config.static_header(
              null.get(name),
              null.get(value)
            )
            : null
        end,
      xml.children("add-header", custom)
    )
  block_entries =
    null.defined(block)
      ? list.map(
        fun (entry) -> icecast.server.config.computed_header(entry),
        cors
      )
      : []
  [...http_headers, ...added, ...block_entries]
end
