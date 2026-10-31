config =
  xml.tree(
    '<server>
  <hostname>radio.example</hostname>
  <listen port="8000" tls="no"/>
  <mount public="1"><name>/live</name></mount>
  <mount><name>/backup</name></mount>
</server>'
  )

hostname = xml.string(xml.child("hostname", config)) ?? "localhost"
listen = xml.child("listen", config)
port = xml.int(attribute="port", listen) ?? 8000
tls = xml.bool(attribute="tls", listen) ?? false

def mount_of_element(element) =
  {
    name = xml.string(xml.child("name", element)) ?? "/",
    public = xml.bool(attribute="public", element) ?? false
  }
end

mounts = list.map(mount_of_element, xml.children("mount", config))

def warn_unknown(tag) =
  if
    not list.mem(tag, ["hostname", "listen", "mount"])
  then
    log.important(
      "Unknown element <#{tag}>"
    )
  end
end

list.iter(warn_unknown, xml.tags(config))
