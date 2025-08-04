val escape :
  special_char:(string -> int -> int -> bool) ->
  next:(string -> int -> int) ->
  escape_char:(string -> int -> int -> string) ->
  string ->
  [> `Orig of int * int | `Subst of string * int ] list * int

val utf8_next : string -> int -> int
val utf8_char_code : string -> int -> int -> int
val is_valid_utf8_code_point : string -> int -> int -> bool
val ascii_special_char : string -> int -> int -> bool
val utf8_special_char : string -> int -> int -> bool
val ascii_next : 'a -> int -> int

val escape_char :
  escape_fun:(string -> int -> int -> string) -> string -> int -> int -> string

val escape_utf8_char : strict:bool -> string -> int -> int -> string

val escape_utf8_formatter :
  ?strict:bool ->
  ?special_char:(string -> int -> int -> bool) ->
  string ->
  [> `Orig of int * int | `Subst of string * int ] list * int

val escape_hex_char : string -> int -> int -> string
val escape_octal_char : string -> int -> int -> string

val escape_ascii_formatter :
  ?special_char:(string -> int -> int -> bool) ->
  string ->
  [> `Orig of int * int | `Subst of string * int ] list * int

val has_subst : [ `Orig of int * int | `Subst of string * int ] list -> bool

val escape_string :
  (string -> [ `Orig of int * int | `Subst of string * int ] list * int) ->
  string ->
  string

val escape_utf8_string :
  ?strict:bool ->
  ?special_char:(string -> int -> int -> bool) ->
  string ->
  string

val escape_ascii_string :
  ?special_char:(string -> int -> int -> bool) -> string -> string

val quote_utf8_string : ?strict:bool -> string -> string
val quote_ascii_string : string -> string
val quote_string : string -> string
val unescape_utf8_pattern : string
val unescape_hex_pattern : string
val unescape_octal_pattern : string
val unescape_patterns : string list
val unescape_octal_char : string -> string
val unescape_hex_char : string -> string
val unescape_utf8_char : string -> string
val unescape_char : string -> string
val unescape_string : string -> string
