## Importing/exporting XML values

Support for XML parsing and rendering was first added in liquidsoap `2.3.1`.

You can parse XML strings using a decorator and type annotation. There are two different representations of XML you can use.

### Record access representation

This is the easiest representation. It is intended for quick access to parsed value via
record and tuples.

Here's an example:

```liquidsoap
s =
'<bla param="1" bla="true">
  <foo opt="12.3">gni</foo>
  <bar />
  <bar>bla</bar>
  <blo>1.23</blo>
  <blu>false</blu>
  <ble>123</ble>
</bla>'

let xml.parse (x :
{
  bla: {
    foo: string.{ xml_params: {opt: float} },
    bar: (unit * string),
    blo: float,
    blu: bool,
    ble: int,
    xml_params: { bla: bool }
  }
}
) = s
```

Things to note:

- The basic mappings are: `tag name -> tag content`
- Tag content maps tag parameters to `xml_params`
- When multiple tags are present, their values are collected as tuple (`bar` tag in the example)
- When a tag contains a single ground value (`string`, `bool`, `float` or `integer`), the mapping is from tag name to the corresponding value, with xml attributes attached as methods
- Tag parameters can be converted to ground values and omitted.

The parsing is driven by the type annotation and is intended to be permissive. For instance, this will work:

```liquidsoaop
s = '<bla>foo</bla>'

let xml.parse (x: { bla: unit }) = s
```

### Formal representation

Because XML format can result in complex values, the parser can also use a generic representation.

Here's an example:

```liquidsoap
s =
'<bla param="1" bla="true">
  <foo opt="12.3">gni</foo>
  <bar />
  <bar>bla</bar>
  <blo>1.23</blo>
  <blu>false</blu>
  <ble>123</ble>
</bla>'

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

# x contains:
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
        ),
        (
          "blo",
          {xml_children=[("xml_text", {xml_text="1.23"})], xml_params=[]}
        ),
        (
          "blu",
          {xml_children=[("xml_text", {xml_text="false"})], xml_params=[]}
        ),
        (
          "ble",
          {xml_children=[("xml_text", {xml_text="123"})], xml_params=[]}
        )
      ],
    xml_params=[("param", "1"), ("bla", "true")]
  }
)
```

This representation is much less convenient to manipulate but allows an exact representation of all XML values.

Things to note:

- XML nodes are represented by a pair of the form: `(<tag name>, <tag properties>)`
- `<tag properties>` contains the following:
  - `xml_params`, represented as a list of pairs `(string * string)`
  - `xml_children`, containing an array of the XML node's children.
  - `xml_text`, present when the node is a text node. In this case, `xml_params` or `xm_children` are empty.
- By convention, text nodes are labelled `xml_text`.

### Rendering XML values

XML values can be converted back to strings using `xml.stringify`.

Both the formal and record-access form can be rendered back into XML strings however, with the record-access representations, if a node has multiple children with the same tag, the conversion to XML string will fail.

More generally, if the values you want to convert to XML strings are complex, for instance if they use several times the same tag as child node or if the order of child nodes matters, we recommend using the formal representation to make sure that children ordering is properly preserved.

This is because record methods are not ordered in the language so we make no guarantee that the child nodes they represent be rendered in a specific order.
