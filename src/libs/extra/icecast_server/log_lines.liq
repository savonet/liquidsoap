# Lines of the icecast.server log files (observability.md §10, §11, §14):
# `access` formats a finished listener session, `playlist` a metadata update
# and `error` a message of the server's log. logs.liq writes them.
let icecast.server.log_lines = ()

# `value` with the bytes that Icecast escapes written as `\xHH`: control
# characters, `!`, `"`, `` ` ``, `\`, and spaces unless `keep_spaces`.
# @flag hidden
def icecast.server.log_lines.escape(~keep_spaces=false, value) =
  def special_char(~encoding:_, c) =
    code = string.nth(c, 0)
    code < 0x20
    or code == 0x7f
    or (
      c ==
        " "
      and not keep_spaces
    )
    or list.mem(c, ["!", "\"", "`", "\\"])
  end

  def escape_char(~encoding:_, c) =
    "\\x#{string.hex_of_int(pad=2, string.nth(c, 0))}"
  end

  string.escape(
    encoding="ascii",
    special_char=special_char,
    escape_char=escape_char,
    value
  )
end

# `value` on one line: carriage returns and line feeds become spaces.
# @flag hidden
def icecast.server.log_lines.one_line(value) =
  r/[\r\n]/g.replace(
    fun (_) ->
      " ",
    value
  )
end

# A client's `value` inserted in an error log message (observability.md §14).
# @flag hidden
def icecast.server.log_lines.client_value(~compatibility, value) =
  compatibility == "kh"
    ? icecast.server.log_lines.one_line(value)
    : (icecast.server.log_lines.escape(value))
end

# @flag hidden
def icecast.server.log_lines.date((t:float)) =
  time.string(
    time=t,
    "%d/%b/%Y:%H:%M:%S %z"
  )
end

# The access log line of the finished session `record`, ended at `ended`.
# @flag hidden
def icecast.server.log_lines.access(~compatibility, ~ended, record) =
  uri = list.hd(default="", string.split(separator="\\?", record.uri))
  kh = compatibility == "kh"
  def plain(value) =
    kh
      ? icecast.server.log_lines.one_line(value)
      : (icecast.server.log_lines.escape(value))
  end

  def header(value) =
    if
      not null.defined(value)
    then
      "-"
    elsif
      kh
    then
      value = null.get(value)
      icecast.server.log_lines.one_line(
        string.length(encoding="ascii", value) > 150
          ? string.sub(encoding="ascii", value, start=0, length=150)
          : value
      )
    else
      icecast.server.log_lines.escape(keep_spaces=true, null.get(value))
    end
  end

  user = record.user ?? ""
  user =
    if
      user == ""
    then
      "-"
    else
      kh
        ? url.encode(plus=false, user)
        : (icecast.server.log_lines.escape(user))
    end
  "#{plain(record.ip)} - #{user} [#{icecast.server.log_lines.date(ended)}] \"GET \
   #{plain(uri)} HTTP/#{plain(record.protocol)}\" 200 #{record.bytes_sent} \"#{
    header(record.referer)
  }\" \"#{header(record.user_agent)}\" #{int_of_float(record.duration)}"
end

# The playlist log line of `text` on `mount` with `listeners`, at `t`.
# @flag hidden
def icecast.server.log_lines.playlist(~t, ~mount, ~listeners, text) =
  text =
    r/[|\r\n]/g.replace(
      fun (_) ->
        " ",
      text
    )
  "#{icecast.server.log_lines.date(t)}|#{mount}|#{listeners}|#{text}"
end

# The error log line of `message` at the Liquidsoap `level`, at `t`: only its
# control characters are escaped, so a caller escapes the client values it
# inserts with `escape`.
# @flag hidden
def icecast.server.log_lines.error(~(t:float), ~category, level, message) =
  name =
    if
      level <= 2
    then
      "EROR"
    elsif
      level == 3
    then
      "WARN"
    elsif
      level == 4
    then
      "INFO"
    else
      "DBUG"
    end
  message =
    r/[\x00-\x1f\x7f]/g.replace(
      fun (c) -> "\\x#{string.hex_of_int(pad=2, string.nth(c, 0))}",
      message
    )
  "[#{
    time.string(
      time=t,
      "%Y-%m-%d  %H:%M:%S"
    )
  }] #{name} #{category} #{message}"
end
