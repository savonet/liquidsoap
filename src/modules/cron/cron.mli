type entry =
  [ `Int of int | `List of int list | `Range of int * int | `Step of int | `Any ]

type t = {
  week_day : entry;
  month : entry;
  month_day : entry;
  hour : entry;
  minute : entry;
}

exception Parse_error of string

val parse : string -> t
val test : ?time:float -> t -> bool
val string_of_entry : entry -> string
