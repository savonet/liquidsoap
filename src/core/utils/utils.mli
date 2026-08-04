(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** Odds and ends that did not earn a module of their own. Everything here is
    used from more than one place; anything used from only one belongs next to
    its caller.

    This module also re-exports {!Liquidsoap_lang_data.Utils}, which is where
    [check_readable] and [string_of_float] come from. *)

(** {1 Paths and files} *)

(** Resolve a path against [current_dir] and check it can be read, raising a
    liquidsoap error positioned at [pos] if it cannot. *)
val check_readable :
  ?current_dir:string ->
  pos:Liquidsoap_lang_prelude.Pos.t list ->
  string ->
  string

val read_all : string -> string
val copy : ?mode:open_flag list -> ?perms:int -> string -> string -> unit
val dir_exists : string -> bool
val mkdir : perm:Unix.file_perm -> string -> unit

(** Create the directory and every missing parent. *)
val ensure_dir : perm:Unix.file_perm -> string -> unit

(** File extension. [get_ext] raises [Not_found] when there is none. *)
val get_ext : string -> string

val get_ext_opt : string -> string option

(** Reopen a channel onto another file, keeping the same file descriptor, so
    that anything already holding the channel keeps working. *)
val reopen_out : out_channel -> string -> unit

val reopen_in : in_channel -> string -> unit

(** Look an executable up in [path]. [which] raises [Not_found]. *)
val which : path:string list -> string -> string

val which_opt : path:string list -> string -> string option

(** {1 Threads and processes} *)

(** [Thread] plus [set_current_thread_name], which names the thread for the
    debugger and for [top]. Only the parts liquidsoap uses: the rest of [Thread]
    is either unused or deprecated. *)
module Thread : sig
  type t = Thread.t

  val create : ('a -> 'b) -> 'a -> t
  val self : unit -> t
  val id : t -> int
  val delay : float -> unit
  val join : t -> unit
  val yield : unit -> unit
  val set_current_thread_name : string -> unit
end

(** [Unix.select], retried on [EINTR]. *)
val select :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

val log_exception : log:Log.t -> bt:string -> string -> unit
val is_docker : bool Lazy.t

(** {1 Time} *)

(** Seconds since the process started. *)
val uptime : unit -> float

external timezone : unit -> int = "liquidsoap_get_timezone" [@@noalloc]

external timezone_by_name : unit -> string * string
  = "liquidsoap_get_timezone_by_name"

(** [Unix.tm] with [tm_isdst] left unknown when it is [None], so that [mktime]
    works it out from the date rather than being told. *)
type tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_isdst : bool option;
}

external mktime : tm -> float = "liquidsoap_mktime"
val strftime : ?time:float -> string -> string

(** {1 Strings} *)

(** Buffer size used when reading and writing. *)
val buflen : int

val string_of_float : float -> string

(** [interpolate subst s] replaces the variables in [s] using [subst]. *)
val interpolate : (string -> string) -> string -> string

(** Register a substitution for {!subst_vars}. *)
val add_subst : string -> string -> unit

val subst_vars : string -> string

(** [String.concat] with a different separator before the last element, for
    rendering lists as "a, b and c". *)
val concat_with_last : last:string -> string -> string list -> string

val normalize_parameter_string : string -> string

(** Parse the 80-bit extended float that AIFF stores its sample rate in. *)
val float_of_extended_float : string -> float

val buffer_drop : Buffer.t -> int -> unit

(** {1 Media} *)

(** MIME type of an ffmpeg container format name. Note that this is a different
    table from the one in {!Icecast_utils}, which answers the same question for
    the Icecast source protocol and does not always give the same answer. *)
val mime_of_container_format : string -> string option

val id3v2_of_metadata : version:int -> (string * string) list -> string

(** {1 Miscellaneous} *)

(** Set the C locale, so that the C library's date and number formatting does
    not depend on the environment. *)
external force_locale : string -> unit = "liquidsoap_set_locale"

val name_of_sockaddr :
  ?rev_dns:bool -> ?show_port:bool -> Unix.sockaddr -> string

(** [Stdlib.abs_float] is not inlined. *)
val abs_float : float -> float

val optional_apply : ('a -> unit) -> 'a option -> unit
