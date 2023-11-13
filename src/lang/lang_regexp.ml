type regexp = Builtins_regexp.regexp

let regexp_t = Builtins_regexp.RegExp.t
let to_regexp = Builtins_regexp.RegExp.of_value
let regexp = Builtins_regexp.RegExp.to_value ?pos:None
let descr_of_regexp { Builtins_regexp.descr; _ } = descr
let string_of_regexp = Builtins_regexp.string_of_regexp
