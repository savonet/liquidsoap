# Parse an XML document into its formal representation: pairs `(tag, node)`
# where `node` has `xml_params`, `xml_children` and `xml_text` (the element's
# text, `null` unless its only content is text). Text nodes are tagged
# `xml_text`. The tree holds the root and five levels of descendants, since
# types are not recursive: deeper elements are dropped and `xml.tags` does not
# see them. Raises `error.xml` when the document is not well-formed.
# @category String
# @param s XML document.
def xml.tree(s) =
  let xml.parse (doc :
    (
      string
      *
      {
        xml_text: string?,
        xml_params: [(string * string)],
        xml_children: [
          (
            string
            *
            {
              xml_text: string?,
              xml_params: [(string * string)],
              xml_children: [
                (
                  string
                  *
                  {
                    xml_text: string?,
                    xml_params: [(string * string)],
                    xml_children: [
                      (
                        string
                        *
                        {
                          xml_text: string?,
                          xml_params: [(string * string)],
                          xml_children: [
                            (
                              string
                              *
                              {
                                xml_text: string?,
                                xml_params: [(string * string)],
                                xml_children: [
                                  (
                                    string
                                    *
                                    {
                                      xml_text: string?,
                                      xml_params: [(string * string)]
                                    }
                                  )
                                ]
                              }
                            )
                          ]
                        }
                      )
                    ]
                  }
                )
              ]
            }
          )
        ]
      }
    )
  ) = s

  doc
end

# Child elements of a node with a given tag, in document order. Returns `[]`
# when the node is `null`.
# @category String
# @param tag Tag of the children.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.children(
  (tag:string),
  (
  node:
  (
    string
    *
    {
      xml_text: string?,
      xml_params: [(string * string)],
      xml_children: [(string * _)]
    }
  )?
  )
) =
  null.case(
    node,
    {[]},
    fun (node) ->
      begin
        let (_, props) = node
        list.filter(fun (child) -> fst(child) == tag, props.xml_children)
      end
  )
end

# First child element of a node with a given tag, `null` if there is none or
# the node is `null`.
# @category String
# @param tag Tag of the child.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.child((tag:string), node) =
  list.case(xml.children(tag, node), null, fun (child, _) -> child)
end

# Tags of a node's child elements, in document order, text nodes excluded.
# Returns `[]` when the node is `null`.
# @category String
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.tags(
  (
  node:
  (
    string
    *
    {
      xml_text: string?,
      xml_params: [(string * string)],
      xml_children: [(string * _)]
    }
  )?
  )
) =
  null.case(
    node,
    {[]},
    fun (node) ->
      begin
        let (_, props) = node
        list.filter(
          fun (tag) -> tag != "xml_text",
          list.map(fst, props.xml_children)
        )
      end
  )
end

# Text of a node, or the value of one of its attributes, untrimmed. A node
# with mixed content has no text. Returns `null` when the node or the value is
# absent.
# @category String
# @param ~attribute Read this attribute instead of the text.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.string(~(attribute:string?)=null, node) =
  null.case(
    node,
    {null},
    fun (node) ->
      begin
        let (_, props) = node
        null.case(
          attribute,
          {props.xml_text},
          fun (name) -> list.assoc.nullable(name, props.xml_params)
        )
      end
  )
end

# @flag hidden
def xml.convert(~attribute, ~kind, convert, node) =
  null.case(
    xml.string(attribute=attribute, node),
    {null},
    fun (value) ->
      begin
        converted = convert(string.trim(value))
        if
          null.defined(converted)
        then
          converted
        else
          let (tag, _) = null.get(node)
          where =
            null.case(
              attribute,
              {"<#{tag}>"},
              fun (name) ->
                "attribute #{name} of <#{tag}>"
            )

          error.raise(
            error.xml,
            "Invalid #{where}: expected #{kind}, got #{string.quote(value)}"
          )
        end
      end
  )
end

# Integer read as `xml.string` reads text, trimmed, in decimal. Returns `null`
# when absent and raises `error.xml` when malformed or out of range.
# @category String
# @param ~attribute Read this attribute instead of the text.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.int(~(attribute:string?)=null, node) =
  def parse(s) =
    if
      r/^[+-]?\d+$/.test(s)
    then
      try
        int_of_string(s)
      catch _ do
        null
      end
    else
      null
    end
  end

  xml.convert(
    attribute=attribute,
    kind="an integer",
    parse,
    node
  )
end

# Decimal number read as `xml.string` reads text, trimmed. Returns `null` when
# absent and raises `error.xml` when malformed.
# @category String
# @param ~attribute Read this attribute instead of the text.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.float(~(attribute:string?)=null, node) =
  xml.convert(
    attribute=attribute,
    kind="a number",
    fun (s) -> r/^[+-]?(\d+\.?\d*|\.\d+)$/.test(s) ? float_of_string(s) : null,
    node
  )
end

# Boolean read as `xml.string` reads text, trimmed: `1`, `true`, `yes`, `on`
# and `0`, `false`, `no`, `off`, case-insensitive. Returns `null` when absent
# and raises `error.xml` when malformed.
# @category String
# @param ~attribute Read this attribute instead of the text.
# @param node Node of `xml.tree`'s representation, possibly `null`.
def xml.bool(~(attribute:string?)=null, node) =
  def parse(s) =
    s = string.case(s)
    if
      list.mem(s, ["1", "true", "yes", "on"])
    then
      true
    elsif
      list.mem(s, ["0", "false", "no", "off"])
    then
      false
    else
      null
    end
  end

  xml.convert(
    attribute=attribute,
    kind="a boolean",
    parse,
    node
  )
end
