(** {2 HTTP Utils} *)

type event = [ `Write | `Read | `Both ]

type socket =
  < typ : string
  ; transport : transport
  ; file_descr : Unix.file_descr
  ; wait_for : ?log:(string -> unit) -> event -> float -> unit
  ; write : Bytes.t -> int -> int -> int
  ; read : Bytes.t -> int -> int -> int
  ; close : unit >

and server =
  < transport : transport
  ; accept : ?timeout:float -> Unix.file_descr -> socket * Unix.sockaddr >

and transport =
  < name : string
  ; protocol : string
  ; default_port : int
  ; connect :
      ?bind_address:string ->
      ?timeout:float ->
      ?prefer:[ `System_default | `Ipv4 | `Ipv6 ] ->
      string ->
      int ->
      socket
  ; server : server >

type uri = {
  protocol : string;
  host : string;
  port : int option;
  path : string;
}

(** Base unix connect *)
val connect :
  ?bind_address:string ->
  ?timeout:float ->
  ?prefer:[ `System_default | `Ipv4 | `Ipv6 ] ->
  string ->
  int ->
  Unix.file_descr

val accept :
  ?timeout:float -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr

val set_socket_default :
  read_timeout:float -> write_timeout:float -> Unix.file_descr -> unit

(** Unix transport and socket. *)
val unix_transport : transport

val unix_socket : Unix.file_descr -> socket

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

(** Read with timeout. *)
val read : timeout:float -> socket -> int -> string

(** Read [len] bytes *)
val really_read : timeout:float -> socket -> int -> string

(** Read chunked data. *)
val read_chunked : timeout:float -> socket -> string * int

type auth = { user : string; password : string }

(** Split authentication string. Raises [Not_found] if failed. *)
val parse_auth : string -> auth
