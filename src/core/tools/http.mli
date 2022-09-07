(** {2 HTTP connections} *)

module type Transport_t = sig
  type connection

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  val default_port : int
  val connect : ?bind_address:string -> string -> int -> connection
  val wait_for : ?log:(string -> unit) -> event -> float -> unit
  val write : connection -> Bytes.t -> int -> int -> int
  val read : connection -> Bytes.t -> int -> int -> int
  val disconnect : connection -> unit
end

module type Http_t = sig
  (** Error handling *)
  type error = Socket | Response

  exception Error of error

  val string_of_error : error -> string

  type connection

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  type uri = {
    protocol : string;
    host : string;
    port : int option;
    path : string;
  }

  (** Default port. *)
  val default_port : int

  (** User-agent for liquidsoap *)
  val user_agent : string

  (** Split an URL into its components. *)
  val parse_url : string -> uri

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

  (** Read from connection *)
  val read : connection -> Bytes.t -> int -> int -> int

  (** Write from connection *)
  val write : connection -> Bytes.t -> int -> int -> int

  (** Wait until Read and/or Write will be non-blocking (mostly..) *)
  val wait_for : ?log:(string -> unit) -> event -> float -> unit

  (** Status of a request:
    * version of the HTTP protocol, status number and status message. *)
  type status = string * int * string

  (** Type for headers data. *)
  type headers = (string * string) list

  (* An ugly code to read until we see [\r]?\n n times. *)
  val read_crlf :
    ?log:(string -> unit) ->
    ?max:int ->
    ?count:int ->
    timeout:float ->
    connection ->
    string

  (* Read chunked data. *)
  val read_chunked : timeout:float -> connection -> string * int

  val request :
    ?log:(string -> unit) ->
    timeout:float ->
    connection ->
    string ->
    (string * int * string) * (string * string) list

  (** [read_with_timeout ?log ~timeout len] reads [len] bytes of data
    * or all available data if [len] is [None]. *)
  val read_with_timeout :
    ?log:(string -> unit) -> timeout:float -> connection -> int option -> string
end

module Make (Transport : Transport_t) :
  Http_t with type connection = Transport.connection

include Http_t with type connection = Unix.file_descr

val status_of_code : int -> string
