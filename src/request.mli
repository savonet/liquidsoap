(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** The request is actually the file abstraction in liquidsoap, used
  * whenever possible. *)

type metadata = (string, string) Hashtbl.t

(** An indicator is a resource location (URI),
  * when meaningful, it can be declared as temporary if liquidsoap
  * should destroy it after usage (this means deleting a local file). *)
type indicator

val indicator : ?metadata:metadata -> ?temporary:bool -> string -> indicator

(** Type of requests,
  * which are devices for obtaining a local file from an URI. *)
type t

(** For media requests,
  * resolving includes testing that the file can actually be decoded
  * into a stream of the expected kind. *)
val create :
  kind:Frame.content_kind ->
  ?metadata:(string * string) list ->
  ?persistent:bool ->
  ?indicators:indicator list ->
  string ->
  t

val create_raw :
  ?metadata:(string * string) list ->
  ?persistent:bool ->
  ?indicators:indicator list ->
  string ->
  t

(** Return the kind of a media request, None for raw requests. *)
val kind : t -> Frame.content_kind option

(** Return the request's initial uri. *)
val initial_uri : t -> string

(** Destroying of a requests causes its file to be deleted if it's a temporary
  * one, for example a downloaded file. If the metadata ["persistent"] is
  * set to ["true"], destroying doesn't happen, unless [force] is set too.
  * Persistent sources are useful for static URIs (see below for the definition
  * of staticity,
  * and in [src/sources/one_file.ml] for an example of use). *)
val destroy : ?force:bool -> t -> unit

(** {1 General management} *)

(** Called at exit, for cleaning temporary files
  * and destroying all the requests, even persistent ones. *)
val clean : unit -> unit

(** Every request has an ID, and you can find a request from its ID. *)

val get_id : t -> int
val from_id : int -> t option

(** Get the list of requests that are currently
  * alive/on air/being resolved. *)

val all_requests : unit -> int list
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

type resolver = string -> log:(string -> unit) -> float -> indicator list
type protocol = { resolve : resolver; static : bool }

(** A static request [r] is such that every resolving leads to the same file.
  * Sometimes, it allows removing useless destroy/create/resolve. *)
val is_static : string -> bool

(** Resolving can fail because an URI is invalid, or doesn't refer to a valid
  * audio file, or simply because there was no enough time left. *)
type resolve_flag = Resolved | Failed | Timeout

(** [resolve request timeout] tries to resolve the request within [timeout]
  * seconds. If resolving succeeds, [is_ready request] is true and you
  * can get a filename. *)
val resolve : t -> float -> resolve_flag

(** [is_ready r] if there's an available local filename. It can be true even if
  * the resolving hasn't been run, if the initial URI was already a local
  * filename. *)
val is_ready : t -> bool

(** Return a valid local filename if there is one, which means that the request
  * is ready. *)
val get_filename : t -> string option

(** {1 URI manipulation} For protocol plugins. *)

(** Removes the top URI, possibly destroys the associated file. *)
val pop_indicator : t -> unit

(** Return the top URI, without removing it. *)
val peek_indicator : t -> string

(** In most of the case you don't peek or pop any URI, you just push the
  * new URIs you computed from [first_uri]. *)
val push_indicators : t -> indicator list -> unit

(** {1 Metadatas} *)

val string_of_metadata : metadata -> string
val short_string_of_metadata : metadata -> string
val set_metadata : t -> string -> string -> unit
val get_metadata : t -> string -> string option
val set_root_metadata : t -> string -> string -> unit
val get_root_metadata : t -> string -> string option
val get_all_metadata : t -> metadata

(** {1 Logging}
  * Every request has a separate log in which its history can be written. *)

type log = (Unix.tm * string) Queue.t

val string_of_log : log -> string
val add_log : t -> string -> unit
val get_log : t -> log

(** {1 Media operations}
  * These operations are only meaningful for media requests,
  * and might crash otherwise. *)

(** Indicate that a request is currently being streamed. *)
val on_air : t -> unit

(** Query whether a request is currently being streamed. *)
val is_on_air : t -> bool

(** [duration filename] computes the duration of audio data contained in
  * [filename]. The computation may be expensive.
  * @raise Not_found if no duration computation method is found. *)
val duration : string -> float

(** Return a decoder if the file has been resolved, guaranteed to have
  * available data to deliver. *)
val get_decoder : t -> Decoder.file_decoder option

(** {1 Plugs}
  * Respectively for computing duration,
  * resolving metadata,
  * and resolving URIs.
  * Metadata filling isn't included in Decoder because we want it to
  * occur immediately after request resolution. *)

val dresolvers : (string -> float) Plug.plug
val mresolvers : (string -> (string * string) list) Plug.plug
val protocols : protocol Plug.plug
