(**************************************************************************)
(*  ocaml-dtools                                                          *)
(*  Copyright (C) 2003-2010  The Savonet Team                             *)
(**************************************************************************)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; either version 2 of the License, or     *)
(*  any later version.                                                    *)
(**************************************************************************)
(*  Contact: savonet-devl@lists.sourceforge.net                           *)
(**************************************************************************)

(* $Id$ *)

(** ocaml-dtools.
    @author Stephane Gimenez *)

(** Configuration management module. *)
module Conf : sig
  (** Type for links between keys *)
  type link = string

  (** Type for paths between keys *)
  type path = link list

  type ut =
    < kind : string option
    ; descr : string
    ; comments : string list
    ; plug : link -> ut -> unit
    ; subs : link list
    ; path : path -> ut
    ; routes : ut -> path list
    ; ut : ut >

  (** Type for untyped keys (or keys with unknown type)
      - [kind]: a string describing the type of this key
      - [descr]: a key description/title
      - [comments]: some comments on the key purposes
      - [plug]: a way to plug subkeys
      - [subs]: the list of link names to subkeys
      - [path]: a way to access subkeys
      - [routes]: a way to find paths to an other key *)

  type 'a t =
    < kind : string option
    ; alias : ?comments:string list -> ?descr:string -> (ut -> unit) -> 'a t
    ; descr : string
    ; comments : string list
    ; plug : link -> ut -> unit
    ; subs : link list
    ; path : path -> ut
    ; routes : ut -> path list
    ; ut : ut
    ; set_d : 'a option -> unit
    ; get_d : 'a option
    ; set : 'a -> unit
    ; get : 'a
    ; is_set : bool
    ; validate : ('a -> bool) -> unit
    ; on_change : ('a -> unit) -> unit >

  (** Type for 'a keys
      - [ut]: cast to un untyped key
      - [set_d]: set the default value associated to the key
      - [get_d]: get the default value associated to the key
      - [set]: set the key value according to a user demmand
      - [get]: retrieve the resulting key value *)

  (** A set of connections to others keys *)
  type links = (link * ut) list

  (** Raised on access to an undefined key (without default value) *)
  exception Undefined of ut

  (** Raised when an invalid link has been specified *)
  exception Invalid of string

  (** Raised when a specified link does not exist *)
  exception Unbound of ut * string

  (** Raised when a specified link already exist *)
  exception Bound of ut * string

  (** Raised on access to a key with a mismatching type *)
  exception Mismatch of ut

  (** Raised on cyclic plug *)
  exception Cyclic of ut * ut

  (** Raised on invalid value set *)
  exception Invalid_Value of ut

  (** Raised when bad configuration assignations are encountered *)
  exception Wrong_Conf of string * string

  (** Raised when bad configuration assignations are encountered inside
      configuration files *)
  exception File_Wrong_Conf of string * int * string

  (** Receipt to build a 'a key *)
  type 'a builder =
    ?d:'a ->
    ?p:(ut -> unit) ->
    ?l:links ->
    ?comments:string list ->
    string ->
    'a t

  val unit : unit builder
  val int : int builder
  val float : float builder
  val bool : bool builder
  val string : string builder

  (** Some key builders *)
  val list : string list builder

  (** A structural key builder *)
  val void :
    ?p:(ut -> unit) -> ?l:links -> ?comments:string list -> string -> ut

  val as_unit : ut -> unit t
  val as_int : ut -> int t
  val as_float : ut -> float t
  val as_bool : ut -> bool t
  val as_string : ut -> string t

  (** Casts to specifically typed keys. Raises [Mismatch] on mismatching cast.
  *)
  val as_list : ut -> string list t

  (** Convert a dot separated string to a path *)
  val path_of_string : string -> path

  (** Convert a path to a dot separated string *)
  val string_of_path : path -> string

  (** Generate a description table of a (sub)key *)
  val descr : ?prefix:path -> ut -> string

  (** Dump the configuration table for a (sub)key *)
  val dump : ?prefix:path -> ut -> string

  (** Add a value to the configuration keys, according to the given correctly
      formatted string: "type key :value" Raises [Wrong_Conf] in badly formatted
      cases. *)
  val conf_set : ut -> string -> unit

  (** Read configuration values from the file associated with the given
      filename. Raises [File_Wrong_Conf] with filename line and and error
      message in case of a bad configuration file. *)
  val conf_file : ut -> string -> unit

  (** A set of command line options to be used with the Arg module. *)
  val args : ut -> (string list * Arg.spec * string) list
end

(** Initialisation management module. Allow to define procedures that must be
    executed at start up, and procedures that are to be executed at exit to have
    a clean quit. *)
module Init : sig
  type t

  (** Root start atom *)
  val start : t

  (** Root stop atom *)
  val stop : t

  (** Define a init atom associated with the given [(unit -> unit)] procedure,
      which eventually depends on others atoms (these atoms will be executed
      before the one currently defined) and triggers other atoms (these atoms
      will be executed after the one currently defined). [after] and [before]
      allow to register the currently defined atom in the depend and triggers
      lists of other atoms. *)
  val make :
    ?name:string ->
    ?depends:t list ->
    ?triggers:t list ->
    ?after:t list ->
    ?before:t list ->
    (unit -> unit) ->
    t

  (** Same as [make] plus a shortcut for "after Init.start". *)
  val at_start :
    ?name:string ->
    ?depends:t list ->
    ?triggers:t list ->
    ?after:t list ->
    ?before:t list ->
    (unit -> unit) ->
    t

  (** Same as [make] plus a shortcut for "before Init.stop". *)
  val at_stop :
    ?name:string ->
    ?depends:t list ->
    ?triggers:t list ->
    ?after:t list ->
    ?before:t list ->
    (unit -> unit) ->
    t

  (** Launch the execution of a given init atom. *)
  val exec : t -> unit

  exception Root_prohibited of [ `User | `Group | `Both ]

  (** This function must be used to launch the main procedure of the program. It
      first execute the registered start atoms, then call the main procedure,
      then execute the registered stop atoms. Exceptions raised by the main
      procedure are caught, in order to close properly even in such cases.
      Exceptions are raised again after cleaning. When invoqued with
      [~prohibit_root:true], it checks for root access rights (euid, egid) and
      exit in this case. *)
  val init : ?prohibit_root:bool -> (unit -> unit) -> unit

  exception StartError of exn
  exception StopError of exn

  val conf : Conf.ut
  val conf_daemon : bool Conf.t
  val conf_daemon_pidfile : bool Conf.t
  val conf_daemon_pidfile_path : string Conf.t
  val conf_daemon_pidfile_perms : int Conf.t
  val conf_trace : bool Conf.t
  val conf_catch_exn : bool Conf.t

  (** A set of command line options to be used with the Arg module. *)
  val args : (string list * Arg.spec * string) list
end

module Log : sig
  type entry = {
    time : float;
    label : string option;
    level : int option;
    log : string;
  }

  (** Type for loggers. *)
  type t =
    < active : int -> bool
    ; level : int
    ; set_level : int -> unit
    ; path : Conf.path
    ; f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
    ; g :
        'a.
        ?colorize:(entry -> entry) ->
        int ->
        ('a, unit, string, unit) format4 ->
        'a >

  type custom_log = { timestamp : bool; exec : string -> unit }

  (** Add a custom logging functions. *)
  val add_custom_log : string -> custom_log -> unit

  (** Remove a custom logging functions. *)
  val rm_custom_log : string -> unit

  (** Make a logger labeled according to the given path. *)
  val make : Conf.path -> t

  (** An atom that starts the logging. *)
  val start : Init.t

  (** An atom that stops the logging. *)
  val stop : Init.t

  (** Force flush all pending log entries. *)
  val flush : unit -> unit

  val conf : Conf.ut
  val conf_level : int Conf.t
  val conf_unix_timestamps : bool Conf.t
  val conf_stdout : bool Conf.t
  val conf_file : bool Conf.t
  val conf_file_path : string Conf.t
  val conf_file_append : bool Conf.t
  val conf_file_perms : int Conf.t

  (** A set of command line options to be used with the Arg module. *)
  val args : (string list * Arg.spec * string) list
end
