
(** Plug for resolving, that is obtaining a file from an URI.
  * [src/protocols] plugins provide ways
  * to resolve URIs: fetch, generate, ...
  * The request is actually the file abstraction in liquidsoap, and you
  * should think about using it as much as possible. *)

type t

(** [audio] means that we expect an audio file. In case [audio] is set,
  * resolving includes testing that the file can actually be decoded. *)
val create : ?audio:bool -> string -> t option

(** Destroying of a requests causes its file to be deleted if it's a temporary
  * one, for example a downloaded file. If the metadata ["persistent"] is
  * set to ["true"], destroying doesn't happen, unless [force] is set too.
  * Persistent sources are useful for static URIs (see below for the definition
  * of staticity,
  * and in [src/sources/one_file.ml] for an example of use). *)
val destroy : ?force:bool -> t -> unit

(** Check if a request is currently being streamed. *)
val on_air : t -> unit

(** {1 General management} *)

(** Called at exit, for cleaning temporary files
  * and destroying all the requests, even persistent ones. *)
val clean : unit -> unit


(** Every request has an id, and you can find a request from its id. *)

val get_id : t -> int
val from_id : int -> t option

(** Get the list of requests that are currently
  * alive/on air/being resolved. *)

val alive_requests : unit -> int list
val on_air_requests : unit -> int list
val resolving_requests : unit -> int list

(** {1 Resolving}
  *
  * Resolving consists in many steps. Every step consist in rewriting the
  * first URI into other URIs. The process ends when the first URI
  * is a local filename. For example, the initial URI can be a database query,
  * which is then turned into a list of remote locations, which are then
  * tentatively downloaded...
  * At each step [protocol.resolve first_uri timeout] is called,
  * and the function is expected to push the new URIs in the request. *)

type protocol = { resolve : string -> t -> float -> unit ; static : bool }

(** A static request [r] is such that every resolving leads to the same file.
  * Sometimes, it allows removing useless destroy/create/resolve. *)
val is_static : string -> bool option

(** Two plugs are used. One for the proper file resolving, the second
  * one for metadata reading/filling. Metadata filling doesn't come with
  * the Decoder, simply because we want it to occur immediately
  * after resolving. *)

val mresolvers : (string -> (string*string) list) Plug.plug
val protocols : protocol Plug.plug

(** Resolving can fail because an URI is invalid, or doesn't refer to a valid
  * audio file, or simply because there was no enough time left. *)
type resolve_flag = Resolved | Failed | Timeout

(** [resolve request timeout] tries to resolve the request within [timeout]
  * seconds. If resolving succeeds, [is_ready request] is true and you
  * can get a filename. *)
val resolve : t -> float -> resolve_flag

(** [is_ready r] if there's an available local filename. It can be true even if
  * the resolving hasn't been run, if the initial URI was already a local
  * filename. However it's better to run [resolve] all the time, cause it's
  * the only way to proceed to the audio validity test. *)
val is_ready : t -> bool

val get_filename : t -> string option

(** {1 URI manipulation} For protocol plugins. *)

val peek_indicator : t -> string
val pop_indicator : t -> string

(** In most of the case you don't peek or pop any URI, you just push the
  * new URIs you computed from [first_uri]. *)
val push_indicator : t -> string list -> unit

(** {1 Metadatas} *)

type metadata = (string,string) Hashtbl.t
val string_of_metadata : metadata -> string
val short_string_of_metadata : metadata -> string
val set_metadata : t -> string -> string -> unit
val get_metadata : t -> string -> string option
val get_metadatas : t -> metadata

(** {1 Logging}
  * Every request has a separate log in which its history can be written. *)

type log = (Unix.tm*string) Queue.t
val string_of_log : log -> string
val log : t -> string -> unit
val get_log : t -> log
