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

# Sends the command forms of `/admin/listeners` with `fetch`, shows the
# message of the `iceresponse` in the form, and reloads the page after a
# success and every 10 seconds, except while a metadata field holds text.
# @flag hidden
let icecast.server.pages.admin_script =
  "<script>document.querySelectorAll('form[data-command]').forEach(f=>f.addEventListener('submit',async \
   e=>{e.preventDefault();if(f.dataset.confirm&&!confirm(f.dataset.confirm))return;const \
   out=f.querySelector('output');try{const r=await \
   fetch(f.dataset.command+'?'+new URLSearchParams(new FormData(f)));const x=new \
   DOMParser().parseFromString(await \
   r.text(),'text/xml');out.textContent=x.querySelector('message')?.textContent??r.statusText;if(r.ok)setTimeout(()=>location.reload(),1000)}catch(err){out.textContent=String(err)}}));setInterval(()=>{if(![...document.querySelectorAll('input[name=song]')].some(i=>i.value))location.reload()},10000)</script>"

# `seconds` as `1h 02m 03s`, leading zero units left out.
# @flag hidden
def icecast.server.pages.duration((seconds:float)) =
  total = int_of_float(seconds)
  h = total / 3600
  m = total mod 3600 / 60
  s = total mod 60
  def pad(n) =
    n < 10 ? "0#{n}" : "#{n}"
  end

  if
    h > 0
  then
    "#{h}h #{pad(m)}m #{pad(s)}s"
  elsif
    m > 0
  then
    "#{m}m #{pad(s)}s"
  else
    "#{s}s"
  end
end

# The body of `/admin/listeners`: per mount, its figures, the commands of the
# mount (metadata update, source disconnection) and its listeners.
# @flag hidden
def icecast.server.pages.listeners_html(view) =
  html = icecast.server.pages.html
  duration = icecast.server.pages.duration
  bytes = runtime.memory.prettify_bytes
  def cell(value) =
    "<td>#{html(value)}</td>"
  end

  def entry(name, value) =
    "<dt>#{name}</dt><dd>#{html(value)}</dd>"
  end

  def listener(l) =
    "<tr>#{cell(l.ip)}#{cell(l.user ?? "")}#{cell(l.user_agent ?? "")}#{
      cell(l.referer ?? "")
    }#{cell(duration(l.duration))}#{cell(bytes(l.bytes_sent))}</tr>"
  end

  def listeners(m) =
    list.is_empty(m.listeners)
      ? "<p>No listeners.</p>"
      : "<table><tr><th>Address</th><th>User</th><th>User \
       agent</th><th>Referer</th><th>Connected</th><th>Sent</th></tr>#{
        string.concat(list.map(listener, m.listeners))
      }</table>"
  end

  def source(m) =
    if
      m.source_ip != ""
    then
      m.source_agent == ""
        ? "source client"
        : (m.source_agent)
    elsif
      m.has_source
    then
      "Liquidsoap source"
    else
      "none"
    end
  end

  def commands(m) =
    mount =
      "<input type=\"hidden\" name=\"mount\" value=\"#{html(m.mount)}\">"
    metadata =
      "<form data-command=\"/admin/metadata\">#{mount}<input type=\"hidden\" \
       name=\"mode\" value=\"updinfo\"><input name=\"song\" placeholder=\"Artist \
       - Title\" aria-label=\"Metadata of #{html(m.mount)}\"><button>Update \
       metadata</button><output></output></form>"
    kill =
      "<form data-command=\"/admin/killsource\" data-confirm=\"Disconnect the \
       source of #{html(m.mount)}?\">#{mount}<button>Disconnect \
       source</button><output></output></form>"
    m.has_source ? "#{metadata}#{kill}" : ""
  end

  def mount(m) =
    name = m.name == "" ? m.mount : (m.name)
    "<section><h2>#{html(name)}</h2><dl>#{entry("Mount", m.mount)}#{
      entry("Format", m.mime_type)
    }#{entry("Source", source(m))}#{
      entry(
        "Now playing",
        icecast.server.stats.title(m.metadata) ?? ""
      )
    }#{
      entry(
        "Listeners",
        "#{list.length(m.listeners)} (max #{m.max_listeners}, peak #{
          m.peak_listeners
        })"
      )
    }#{entry("Connections", "#{m.connections}")}#{
      entry(
        "Listening time",
        duration(m.listening_time)
      )
    }#{entry("Sent", bytes(m.bytes_sent))}</dl>#{commands(m)}#{
      listeners(m)
    }</section>"
  end

  body =
    list.is_empty(view.mounts)
      ? "<p>No mounts.</p>"
      : (string.concat(list.map(mount, view.mounts)))
  "<!DOCTYPE html>\n<html><head>#{
    icecast.server.pages.head
  }<title>Admin</title></head><body><h1>Admin</h1>#{body}#{
    icecast.server.pages.admin_script
  }</body></html>\n"
end
