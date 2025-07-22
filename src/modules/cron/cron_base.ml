type entry = [ `Int of int | `Range of int * int | `Step of int | `Any ]

let string_of_entry = function
  | `Int i -> string_of_int i
  | `Range (v, v') -> Printf.sprintf "%d-%d" v v'
  | `Step v -> Printf.sprintf "*/%d" v
  | `Any -> "*"

type t = {
  week_day : entry;
  month : entry;
  month_day : entry;
  hour : entry;
  minute : entry;
}

type pos = Lexing.position * Lexing.position

exception Parse_error of (pos * string)
