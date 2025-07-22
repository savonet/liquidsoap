type entry =
  [ `Int of int | `List of int list | `Range of int * int | `Step of int | `Any ]

let string_of_entry = function
  | `Int i -> string_of_int i
  | `List l -> String.concat "," (List.map string_of_int l)
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

exception Parse_error of string
