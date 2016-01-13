(** {2 HTTP connections} *)

(** Error handling *)
type error = Socket | Response | UrlDecoding
exception Error of error
val string_of_error : error -> string

(** A connection with an http server. *)
type connection = Unix.file_descr

(** User-agent for liquidsoap *)
val user_agent : string

(** Decode an url. *)
val url_decode : ?plus:bool -> string -> string

(** Encode an url. *)
val url_encode : ?plus:bool -> string -> string

(** Split an URL to return host, port and URI. *)
val url_split_host_port : string -> string * int option * string

(** Basic detection of whether a path is an HTTP url. *)
val is_url : string -> bool

(** Url without the trailing filename. *)
val dirname : string -> string

(** split arg=value&arg2=value2 into (arg, value) Hashtbl.t *)
val args_split : string -> (string, string) Hashtbl.t

(** Connect to an http server given an host and a port. *)
val connect : ?bind_address:string -> string -> int -> connection

(** Disconnect from an http server. *)
val disconnect : connection -> unit

(** Status of a request:
  * version of the HTTP protocol, status number and status message. *)
type status = string * int * string

(** Type for headers data. *)
type headers = (string*string) list

(* An ugly code to read until we see [\r]?\n n times. *)
val read_crlf : ?log:(string -> unit) -> ?max:int -> ?count:int -> 
                timeout:float -> connection -> string

(* Read chunked data. *)
val read_chunked : timeout:float -> connection -> string * int

val request : ?log:(string -> unit) ->
              timeout:float ->
              connection ->
              string -> (string * int * string) * (string * string) list

(** [get ?log ?headers ~timeout socket host port file] makes a GET request.
  * Returns the status and the headers. *)
val get : ?headers:(string * string) list ->
          ?log:(string -> unit) ->
          timeout:float ->
          connection ->
          string ->
          int -> string -> (string * int * string) * (string * string) list


(** [post ?log ?headers ~timeout data socket host port file] makes a POST request.
  * Returns the status and the headers. *)
val post : ?headers:(string * string) list ->
           ?log:(string -> unit) ->
           timeout:float ->
           string ->
           connection ->
           string ->
           int -> string -> (string * int * string) * (string * string) list

(** [read ?log ~timeout len] reads [len] bytes of data
  * or all available data if [len] is [None]. *)
val read : ?log:(string -> unit) ->
           timeout:float -> connection -> int option -> string

(** Type for full Http request. *)
type request = Get | Post of string

(** Perform a full Http request and return the response status,headers
  * and data. *)
val full_request :
           ?headers:(string * string) list ->
           ?port:int ->
           ?log:(string -> unit) ->
           timeout:float ->
           host:string ->
           url:string ->
           request:request ->
           unit -> (string * int * string) * (string * string) list * string

