%include "../test.liq"

def error_message(fn) =
  try
    ignore(fn())
    "no error"
  catch err : [error.xml] do
    err.message
  end
end

def f() =
  doc =
    xml.tree(
      '<icecast>
  <hostname>radio.example</hostname>
  <limits>
    <clients> 100 </clients>
    <timeout>2.5</timeout>
    <burst>abc</burst>
  </limits>
  <mount type="default"><hidden>Yes</hidden><public>maybe</public></mount>
  <mount type="normal">
    <mount-name>/a</mount-name>
    <custom-headers><cors><allow-origin>https://x</allow-origin></cors></custom-headers>
  </mount>
  <listen-socket port="8000" bind="0.0.0.0"/>
  <listen-socket port="80x"/>
</icecast>'
    )

  let (root_tag, root) = doc
  test.equal(root_tag, "icecast")
  test.equal(root.xml_params, [])
  test.equal(
    xml.tags(doc),
    ["hostname", "limits", "mount", "mount", "listen-socket", "listen-socket"]
  )

  test.equal(list.length(xml.children("mount", doc)), 2)
  test.equal(xml.children("relay", doc), [])
  test.equal(xml.child("relay", doc), null)
  test.equal(xml.tags(xml.child("hostname", doc)), [])
  test.equal(xml.string(xml.child("hostname", doc)), "radio.example")
  test.equal(xml.string(xml.child("location", doc)), null)
  test.equal(xml.string(xml.child("limits", doc)), null)

  test.equal(xml.child("cors", xml.child("relay", doc)), null)
  test.equal(xml.children("cors", xml.child("relay", doc)), [])
  test.equal(xml.tags(xml.child("relay", doc)), [])
  test.equal(xml.int(xml.child("port", xml.child("relay", doc))), null)
  test.equal(xml.bool(attribute="on", xml.child("relay", doc)), null)

  limits = xml.child("limits", doc)
  test.equal(xml.tags(limits), ["clients", "timeout", "burst"])
  test.equal(
    xml.string(xml.child("clients", limits)),
    " 100 "
  )
  test.equal(xml.int(xml.child("clients", limits)), 100)
  test.equal(xml.int(xml.child("sources", limits)), null)
  test.equal(xml.float(xml.child("timeout", limits)), 2.5)
  test.equal(xml.float(xml.child("clients", limits)), 100.)
  test.equal(
    error_message({xml.int(xml.child("burst", limits))}),
    "Invalid <burst>: expected an integer, got \"abc\""
  )

  test.equal(
    error_message({xml.int(xml.child("timeout", limits))}),
    "Invalid <timeout>: expected an integer, got \"2.5\""
  )

  test.equal(
    error_message({xml.float(xml.child("burst", limits))}),
    "Invalid <burst>: expected a number, got \"abc\""
  )

  test.equal(
    error_message({xml.int(xml.tree("<a>99999999999999999999999</a>"))}),
    "Invalid <a>: expected an integer, got \"99999999999999999999999\""
  )

  test.equal(
    xml.string(
      xml.tree(
        "<a> x <b/> </a>"
      )
    ),
    null
  )

  let [default_mount, normal_mount] = xml.children("mount", doc)
  test.equal(xml.string(attribute="type", default_mount), "default")
  test.equal(xml.string(attribute="charset", default_mount), null)
  test.equal(xml.bool(xml.child("hidden", default_mount)), true)
  test.equal(xml.bool(xml.child("burst", default_mount)), null)
  test.equal(
    error_message({xml.bool(xml.child("public", default_mount))}),
    "Invalid <public>: expected a boolean, got \"maybe\""
  )

  def bool_of_text(s) =
    xml.bool(xml.tree("<b>#{s}</b>"))
  end

  test.equal(
    list.map(bool_of_text, ["1", "true", "YES", "on"]),
    [true, true, true, true]
  )

  test.equal(
    list.map(bool_of_text, ["0", "False", "no", "OFF"]),
    [false, false, false, false]
  )

  let [listen_socket, bad_socket] = xml.children("listen-socket", doc)
  test.equal(xml.int(attribute="port", listen_socket), 8000)
  test.equal(xml.int(attribute="ssl", listen_socket), null)
  test.equal(xml.string(attribute="bind", listen_socket), "0.0.0.0")
  test.equal(
    error_message({xml.int(attribute="port", bad_socket)}),
    "Invalid attribute port of <listen-socket>: expected an integer, got \
     \"80x\""
  )

  cors = xml.child("cors", xml.child("custom-headers", normal_mount))
  test.equal(xml.string(xml.child("allow-origin", cors)), "https://x")

  deep =
    xml.tree(
      '<l0><l1><l2><l3><l4 x="4"><l5 x="5">five<l6><l7/></l6></l5><l5 \
       x="7">seven</l5></l4></l3></l2></l1></l0>'
    )

  l4 = xml.child("l4", xml.child("l3", xml.child("l2", xml.child("l1", deep))))

  test.equal(xml.tags(l4), ["l5", "l5"])
  test.equal(xml.int(attribute="x", l4), 4)
  let [l5, last_l5] = xml.children("l5", l4)
  test.equal(xml.int(attribute="x", l5), 5)
  test.equal(xml.int(attribute="x", last_l5), 7)
  test.equal(xml.string(last_l5), "seven")
  test.equal(xml.string(l5), null)

  test.raises({xml.tree("<icecast><hostname></icecast>")}, error.xml)

  test.pass()
end

test.check(f)
