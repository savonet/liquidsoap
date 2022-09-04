include Lang_core
include Lang_error

let regexp_t = Builtins_regexp.RegExp.t
let to_regexp = Builtins_regexp.RegExp.of_value
let regexp = Builtins_regexp.RegExp.to_value
let string_of_regexp = Builtins_regexp.descr
