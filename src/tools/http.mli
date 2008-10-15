(** {2 HTTP connections} *)

(** Error handling *)
type error = Socket | Response | UrlDecoding
exception Error of error
val string_of_error : error -> string

(** A connection with an http server. *)
type connection = Unix.file_descr

(** Decode an url. *)
val url_decode : ?plus:bool -> string -> string

(** Encode an url. *)
val url_encode : ?plus:bool -> string -> string

(** [http_encode url] encode only relevant parts in url *)
val http_encode : string -> string

(** split arg=value&arg2=value2 into (arg, value) Hashtbl.t *)
val args_split : string -> (string, string) Hashtbl.t

(** Connect to an http server given an host and a port. *)
val connect : bind_address:string option -> timeout:float option -> string -> int -> connection

(** Disconnect from an http server. *)
val disconnect : connection -> unit

(** Status of a request:
  * version of the HTTP protocol, status number and status message. *)
type status = string * int * string

val request : connection -> string -> (status * (string * string) list)

(** [get headers socket host port file] makes a GET request.
  * Returns the status and the headers. *)
val get : ?headers:((string*string) list) -> connection
  -> string -> int -> string -> (status * (string * string) list)

(** [post headers socket host port file] makes a POST request.
  * Returns the status and the headers. *)
val post : ?headers:((string*string) list) -> string
  -> connection -> string -> int -> string -> (status * (string * string) list)

(** [read len] reads [len] bytes of data
  * or all available data if [len] is [None]. *)
val read : connection -> int option -> string
