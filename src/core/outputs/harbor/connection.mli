(** The wire side of admission: credentials, refusal replies and the response
    head. Every reply is complete, and the server closes the connection after
    it. *)

val response_head :
  protocol:string ->
  mime:string ->
  metaint:int option ->
  (string * string) list ->
  string

(** User and password from a Basic [authorization] header, else from the [pass]
    and [user] query arguments. *)
val credentials :
  headers:(string * string) list ->
  query:(string * string) list ->
  (string * string) option

val unauthorized : [ `No_credentials | `Wrong_credentials ] -> string
val mount_taken : string

(** A refusal from the admission predicate, sent with an empty body. *)
type refusal = {
  status : int;
  reason : string;
  headers : (string * string) list;
}

val refusal_reply : protocol:string -> refusal -> string

(** The refusal of a predicate that failed. *)
val forbidden : refusal

val unavailable : protocol:string -> string

(** Why a header cannot be sent, if it cannot. *)
val header_error : string * string -> string option

(** Why a refusal cannot be sent, if it cannot. *)
val refusal_error : refusal -> string option
