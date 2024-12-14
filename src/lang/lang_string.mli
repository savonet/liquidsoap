val getpwnam : (string -> Unix.passwd_entry) ref

val escape :
  special_char:(string -> int -> int -> bool) ->
  next:(string -> int -> int) ->
  escape_char:(string -> int -> int -> string) ->
  string ->
  [> `Orig of int * int | `Subst of string * int ] list * int

val utf8_next : string -> int -> int
val utf8_char_code : string -> int -> int -> int
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
val string_of_matrix : string array array -> string
val unbreak_md : string -> string
val find_cmd : (string * string) list -> string option
val print_string : ?pager:bool -> string -> unit
val kprint_string : ?pager:bool -> ((string -> unit) -> unit) -> unit

module Version : sig
  type t = int list * string

  val hash_fold_t : Term_hash.state -> t -> Term_hash.state
  val of_string : string -> t
  val to_string : t -> string
  val num : t -> int list
  val str : t -> string
  val compare : t -> t -> int
end

val home_unrelate : string -> string
val generate_id : string -> string
val encode64 : string -> string
val decode64 : string -> string
val url_encode : ?plus:bool -> string -> string
val url_decode : ?plus:bool -> string -> string
