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

(** The refusal of a predicate that failed. *)
val forbidden : Harbor_refusal.t

val unavailable : protocol:string -> string
