def f() =
  s =
    '<bla param="1" bla="true">
<foo opt="12.3">gni</foo>
<bar />
<bar option="aab">bla</bar>
</bla>'

  let xml.parse (x :
    {
      bla: {
        foo: string.{ xml_params: {opt: float} },
        bar: (string? * string?.{ xml_params: [(string * string)] }),
        xml_params: [(string * string)].{ bla: bool }
      }
    }
  ) = s

  test.equal(
    x,
    {
      bla=
        {
          xml_params=[("param", "1"), ("bla", "true")].{bla=true},
          bar=(null(), "bla".{xml_params=[("option", "aab")]}),
          foo="gni".{xml_params={opt=12.3}}
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
            foo="gni".{xml_params={opt=12.3}}
          }
      }
    ),
    '<bla param="1" bla="true">
  <bar option="aab">bla</bar>
  <foo opt="12.3">gni</foo>
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
              xml_children: [(string * {xml_text: string?})]
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
        xml_children=
          [
            (
              "foo",
              {
                xml_children=[("xml_text", {xml_text="gni"})],
                xml_params=[("opt", "12.3")]
              }
            ),
            ("bar", {xml_children=[], xml_params=[]}),
            (
              "bar",
              {
                xml_children=[("xml_text", {xml_text="bla"})],
                xml_params=[("option", "aab")]
              }
            )
          ],
        xml_params=[("param", "1"), ("bla", "true")]
      }
    )
  )

  test.equal(
    xml.stringify(x),
    '<bla param="1" bla="true">
  <foo opt="12.3">gni</foo>
  <bar/>
  <bar option="aab">bla</bar>
</bla>'
  )

  test.pass()
end

test.check(f)
