def f() =
  s =
    '<bla param="1" bla="true">
<foo opt="12.3">gni</foo>
<bar />
<bar option="aab">bla</bar>
<blo>1.23</blo>
<blu>false</blu>
<ble>123</ble>
</bla>'

  let xml.parse (x :
    {
      bla: {
        foo: string.{ xml_params: {opt: float} },
        bar: (string? * string?.{ xml_params: [(string * string)] }),
        blo: float,
        blu: bool,
        ble: int,
        xml_params: [(string * string)].{ bla: bool }
      }
    }
  ) = s

  test.equal(
    x,
    {
      bla=
        {
          foo="gni".{xml_params={opt=12.3}},
          bar=(null, "bla".{xml_params=[("option", "aab")]}),
          blo=1.23,
          blu=false,
          ble=123,
          xml_params=[("param", "1"), ("bla", "true")].{bla=true}
        }
    }
  )

  test.equal(
    xml.stringify(
      {
        bla=
          {
            xml_params=[("param", "1"), ("bla", "true")],
            bar="bla".{xml_params=[("option", "aab")]},
            foo=true.{xml_params={opt=12.3}}
          }
      }
    ),
    '<bla param="1" bla="true">
  <bar option="aab">bla</bar>
  <foo opt="12.3">true</foo>
</bla>'
  )

  let xml.parse (x :
    (
      string
      *
      {
        xml_params: [(string * string)],
        xml_children: [
          (
            string
            *
            {
              xml_params: [(string * string)],
              xml_children: [(string * {xml_text: string})]
            }
          )
        ]
      }
    )
  ) = s

  test.equal(
    x,
    (
      "bla",
      {
        xml_params=[("param", "1"), ("bla", "true")],
        xml_children=
          [
            (
              "foo",
              {
                xml_params=[("opt", "12.3")],
                xml_children=[("xml_text", {xml_text="gni"})]
              }
            ),
            ("bar", {xml_params=[], xml_children=[]}),
            (
              "bar",
              {
                xml_params=[("option", "aab")],
                xml_children=[("xml_text", {xml_text="bla"})]
              }
            ),
            (
              "blo",
              {xml_params=[], xml_children=[("xml_text", {xml_text="1.23"})]}
            ),
            (
              "blu",
              {xml_params=[], xml_children=[("xml_text", {xml_text="false"})]}
            ),
            (
              "ble",
              {xml_params=[], xml_children=[("xml_text", {xml_text="123"})]}
            )
          ]
      }
    )
  )

  test.equal(
    xml.stringify(x),
    '<bla param="1" bla="true">
  <foo opt="12.3">gni</foo>
  <bar/>
  <bar option="aab">bla</bar>
  <blo>1.23</blo>
  <blu>false</blu>
  <ble>123</ble>
</bla>'
  )

  # Test custom label with "as" syntax for hyphenated element names
  s2 =
    '<config>
<listen-socket><port>8000</port></listen-socket>
<source-password>hackme</source-password>
</config>'

  let xml.parse (config :
    {
      config: {
        "listen-socket" as listen_socket: {port: int},
        "source-password" as source_password: string
      }
    }
  ) = s2

  test.equal(config.config.listen_socket.port, 8000)
  test.equal(config.config.source_password, "hackme")

  # Test optional fields with "as" syntax
  s3 = '<limits>
<sources>10</sources>
</limits>'

  let xml.parse (limits_config :
    {limits: {sources?: int, "queue-size" as queue_size?: int}}
  ) = s3

  test.equal(limits_config.limits?.sources, 10)
  test.equal(limits_config.limits?.queue_size, null())

  # Test empty elements with string type - should return empty string
  s3 =
    '<root><empty></empty><self-closing/><with-child><child/></with-child></root>'

  let xml.parse (empty_test :
    {
      root: {
        empty: string,
        "self-closing" as self_closing: string,
        "with-child" as with_child: string
      }
    }
  ) = s3

  test.equal(empty_test.root.empty, "")
  test.equal(empty_test.root.self_closing, "")
  test.equal(empty_test.root.with_child, "")

  test.pass()
end

test.check(f)
