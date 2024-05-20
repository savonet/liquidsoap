(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** A request is something from which we can produce a file. *)

(** An indicator is a resource location (URI), when meaningful, it can be
    declared as temporary if liquidsoap should destroy it after usage (this means
    deleting a local file). *)
type indicator

(** Create an indicator. *)
val indicator :
  ?metadata:Frame.metadata -> ?temporary:bool -> string -> indicator

(** Type of requests, which are devices for obtaining a local file from an
    URI. *)
type t

(** Create a request. *)
val create :
  ?resolve_metadata:bool ->
  ?excluded_metadata_resolvers:string list ->
  ?metadata:(string * string) list ->
  ?persistent:bool ->
  ?indicators:indicator list ->
  cue_in_metadata:string option ->
  cue_out_metadata:string option ->
  string ->
  t

(** Return the type of a media request, None for raw requests. *)
val ctype : t -> Frame.content_type option

(** Return the request's initial uri. *)
val initial_uri : t -> string

(** Destroying of a requests causes its file to be deleted if it's a temporary
    one, for example a downloaded file. If the metadata ["persistent"] is set to
    ["true"], destroying doesn't happen, unless [force] is set too.  Persistent
    sources are useful for static URIs (see below for the definition of
    staticity, and in [src/sources/one_file.ml] for an example of use). *)
val destroy : ?force:bool -> t -> unit

(** Status of a request. *)
type status = Idle | Resolving | Ready | Playing | Destroyed

(** Current status of a request. *)
val status : t -> status

(** {1 General management} *)

(** Called at exit, for cleaning temporary files and destroying all the
    requests, even persistent ones. *)
val clean : unit -> unit

(** Identifier of a request. *)
val get_id : t -> int

(** Find a request by its identifier. *)
val from_id : int -> t option

(** Get the list of all requests. *)
val all_requests : unit -> int list

(** Get the list of requests that are currently alive. *)
val alive_requests : unit -> int list

(** Get the list of requests that are currently on air. *)
val on_air_requests : unit -> int list

(** Get the list of requests that are currently being resolved. *)
val resolving_requests : unit -> int list

(** {1 Resolving}

    Resolving consists in many steps. Every step consist in rewriting the
    first URI into other URIs. The process ends when the first URI
    is a local filename. For example, the initial URI can be a database query,
    which is then turned into a list of remote locations, which are then
    tentatively downloaded...
    At each step [protocol.resolve first_uri timeout] is called,
    and the function is expected to push the new URIs in the request. *)

(** Something that resolves an URI. *)
type resolver = string -> log:(string -> unit) -> float -> indicator list

(** A protocol, which can resolve associated URIs. *)
type protocol = { resolve : resolver; static : bool }

(** A static request [r] is such that every resolving leads to the same file.
    Sometimes, it allows removing useless destroy/create/resolve. *)
val is_static : string -> bool

(** Resolving can fail because an URI is invalid, or doesn't refer to a valid
  * audio file, or simply because there was no enough time left. *)
type resolve_flag = Resolved | Failed | Timeout

(** Metadata resolvers priorities. *)
val conf_metadata_decoder_priorities : Dtools.Conf.ut

(** Read the metadata for the toplevel indicator of the request. This is usually
    performed automatically by [resolve] so that you do not have to use this,
    excepting when the [ctype] is [None]. *)
val read_metadata : t -> unit

(** [resolve ?ctype request timeout] tries to resolve the request within
    [timeout] seconds. It finds a decoder for the request which produces content
    type [ctype], unless this is set to [None]. If resolving succeeds, [is_ready
    request] is true and you can get a filename. *)
val resolve : ctype:Frame.content_type option -> t -> float -> resolve_flag

(** [resolved r] if there's an available local filename. It can be true even if
    the resolving hasn't been run, if the initial URI was already a local
    filename. *)
val resolved : t -> bool

(** Return a valid local filename if there is one, which means that the request
    is ready. *)
val get_filename : t -> string option

(** {1 URI manipulation} For protocol plugins. *)

(** Removes the top URI, possibly destroys the associated file. *)
val pop_indicator : t -> unit

(** Return the top URI, without removing it. *)
val peek_indicator : t -> string

(** In most of the case you don't peek or pop any URI, you just push the new
    URIs you computed from [first_uri]. *)
val push_indicators : t -> indicator list -> unit

(** {1 Metadatas} *)

val set_metadata : t -> string -> string -> unit
val get_metadata : t -> string -> string option
val set_root_metadata : t -> string -> string -> unit
val get_all_metadata : t -> Frame.metadata

(** {1 Logging}
    Every request has a separate log in which its history can be written. *)

type log = (Unix.tm * string) Queue.t

val string_of_log : log -> string
val add_log : t -> string -> unit
val get_log : t -> log

(** {1 Media operations}

    These operations are only meaningful for media requests, and might crash
    otherwise. *)

(** Indicate that a request is currently being streamed. *)
val on_air : t -> unit

(** Query whether a request is currently being streamed. *)
val is_on_air : t -> bool

(** [duration ~metadata filename] computes the duration of audio data contained in
    [filename]. The computation may be expensive.
    @raise Not_found if no duration computation method is found. *)
val duration : metadata:Frame.metadata -> string -> float option

(** Return a decoder if the file has been resolved, guaranteed to have
    available data to deliver. *)
val get_decoder : t -> Decoder.file_decoder_ops option

(** {1 Plugs} *)

(** Functions for computing duration. *)
val dresolvers : (metadata:Frame.metadata -> string -> float) Plug.t

(** Type for a metadata resolver. Resolvers are executed in priority
    order and the first returned metadata take precedence over any other
    one later returned. *)
type metadata_resolver = {
  priority : unit -> int;
  resolver :
    metadata:Frame.metadata ->
    extension:string option ->
    mime:string ->
    string ->
    (string * string) list;
}

(** Functions for resolving metadata. Metadata filling isn't included in Decoder
    because we want it to occur immediately after request resolution. *)
val mresolvers : metadata_resolver Plug.t

(** Resolve metadata for a local file: *)
val resolve_metadata :
  initial_metadata:Frame.metadata ->
  excluded:string list ->
  string ->
  Frame.metadata

(** Functions for resolving URIs. *)
val protocols : protocol Plug.t

module Value : Value.Custom with type content := t
