type entry = [ `Int of int | `Range of int * int | `Step of int | `Any ]

type t = {
  week_day : entry;
  month : entry;
  month_day : entry;
  hour : entry;
  minute : entry;
}

type pos = Lexing.position * Lexing.position

exception Parse_error of (pos * string)

val parse : string -> t
val test : t -> bool
val string_of_entry : entry -> string
