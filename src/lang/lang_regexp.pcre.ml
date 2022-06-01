type t = Pcre.regexp
type sub = Pcre.substrings

let regexp s = Pcre.regexp s
let regexp_or l = Pcre.regexp_or l
let split ~pat s = Pcre.split ~pat s
let exec ?pat ?rex s = Pcre.exec ?pat ?rex s
let get_substring sub pos = Pcre.get_substring sub pos
let substitute ?pat ?rex ~subst s = Pcre.substitute ?pat ?rex ~subst s
