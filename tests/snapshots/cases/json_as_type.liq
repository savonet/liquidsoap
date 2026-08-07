# `as` is a contextual keyword, not a reserved word: it stays an ordinary
# identifier everywhere else. `as json.object` reads a JSON object as an
# association list; `"name" as field` renames a field whose JSON name is not a
# valid identifier.
let json.parse (m : [(string * int)] as json.object) = "{\"a\": 1}"
let json.parse (r : {"start" as spin_start: string}) = "{\"start\": \"x\"}"

as = "still an identifier"

(m, r.spin_start, as)
