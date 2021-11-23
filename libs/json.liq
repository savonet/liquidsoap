# Convert a value to a JSON string.
# @category String
# @param ~compact Output compact text.
# @param ~json5 Use JSON5 extended spec.
def json.stringify(~compact=false, ~json5=false, s) =
  let json.stringify[compact=compact, json5=json5] j = s
  j
end
