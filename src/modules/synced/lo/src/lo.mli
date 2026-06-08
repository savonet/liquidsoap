(** Bindings to liblo library to send and receive OSC messages. *)

(** An error occurred. *)
exception Error

(** Raised when an unhandled message kind is received. *)
exception Unhandled

(** Operations on addresses. *)
module Address : sig
  (** An address. *)
  type t

  (** Create an address with given host and port. *)
  val create : string -> int -> t

  (** Default address to send to, guessed by server. *)
  val default : unit -> t
end

(** Messages. *)
module Message : sig
  type timetag = int * int

  type data =
    [ `Int32 of int
    | `Float of float
    | `String of string
    | `Blob of string
    | `Int64 of int
    | `Timetag of timetag
    | `Double of float
    | `Symbol of string
    | `Char of char
    | `Midi of string
    | `True
    | `False
    | `Nil
    | `Infinitum ]

  (** String representation of a message (useful for debugging). *)
  val to_string : data -> string
end

(** Send messages on given address, at given path. *)
val send : Address.t -> string -> Message.data list -> unit

(** Operations for creating servers. *)
module Server : sig
  (** A server. *)
  type t

  (** Raised when an operation is performed on a stopped server. *)
  exception Stopped

  (** Create a server listening on given port with handler function taking as
      argument the path and messages received. *)
  val create : int -> (string -> Message.data array -> unit) -> t

  (** Receive messages and pass them to the handler. This function is blocking
      until a message is received. *)
  val recv : t -> unit

  (** Stop the server. No operation on this server should be performed afterward
      (otherwise, the exception [Stopped] is raised). *)
  val stop : t -> unit
end
