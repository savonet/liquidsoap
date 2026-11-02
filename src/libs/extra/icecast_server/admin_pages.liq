# The Liquidsoap admin pages of icecast.server (observability.md §7.2):
# `/admin/listeners` and `/admin/listeners.json`, hidden mounts included.

# The body of `/admin/listeners.json`.
# @flag hidden
def icecast.server.pages.listeners_json(view) =
  def mount(m) =
    {
      mount = m.mount,
      peak_listeners = m.peak_listeners,
      connections = m.connections,
      listening_time = m.listening_time,
      bytes_sent = m.bytes_sent,
      listeners = m.listeners
    }
  end

  json.stringify({mounts = list.map(mount, view.mounts)})
end

# The body of `/admin/listeners`.
# @flag hidden
def icecast.server.pages.listeners_html(view) =
  html = icecast.server.pages.html
  def cell(value) =
    "<td>#{html(value)}</td>"
  end

  def listener(l) =
    "<tr>#{cell(l.ip)}#{cell(l.user ?? "")}#{cell(l.user_agent ?? "")}#{
      cell(l.referer ?? "")
    }#{cell(string.float(decimal_places=0, l.duration))}#{
      cell("#{l.bytes_sent}")
    }</tr>"
  end

  def mount(m) =
    "<section><h2>#{html(m.mount)}</h2><p>Listeners: #{
      list.length(m.listeners)
    }, peak: #{m.peak_listeners}, connections: #{m.connections}, listening time: \
     #{string.float(decimal_places=0, m.listening_time)} \
     s</p><table><tr><th>Address</th><th>User</th><th>User \
     agent</th><th>Referer</th><th>Connected (s)</th><th>Bytes sent</th></tr>#{
      string.concat(list.map(listener, m.listeners))
    }</table></section>"
  end

  "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><meta \
   http-equiv=\"refresh\" \
   content=\"10\"><title>Listeners</title></head><body><h1>Listeners</h1>#{
    string.concat(list.map(mount, view.mounts))
  }</body></html>\n"
end
