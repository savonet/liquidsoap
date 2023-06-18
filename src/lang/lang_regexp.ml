type regexp = Builtins_regexp.regexp

let regexp_t = Builtins_regexp.RegExp.t
let to_regexp = Builtins_regexp.RegExp.of_value
let regexp = Builtins_regexp.RegExp.to_value ?pos:None
let descr_of_regexp { Builtins_regexp.descr; _ } = descr
let string_of_regexp = Builtins_regexp.string_of_regexp

module Regexp = struct
  type sub = Regexp.sub = {
    matches : string option list;
    groups : (string * string) list;
  }

  let get_rex { Builtins_regexp.regexp } = regexp

  let regexp ?(flags = []) s =
    { Builtins_regexp.descr = s; flags; regexp = Regexp.regexp s }

  let split rex = Regexp.split (get_rex rex)
  let exec rex = Regexp.exec (get_rex rex)
  let test rex = Regexp.test (get_rex rex)
  let substitute rex = Regexp.substitute (get_rex rex)
end
