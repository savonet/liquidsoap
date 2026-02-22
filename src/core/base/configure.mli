(** Constants describing configuration options of liquidsoap. *)

val conf : Dtools.Conf.ut
val conf_init : Dtools.Conf.ut
val conf_debug : bool Dtools.Conf.t
val conf_debug_errors : bool Dtools.Conf.t
val conf_default_font : string Dtools.Conf.t
val conf_force_start : bool Dtools.Conf.t

(** String describing the OS *)
val host : string

(** String describing the version. *)
val version : string

val restart : bool ref
val git_snapshot : bool

(** String describing the software. *)
val vendor : string

(** Where to look for standard .liq scripts to include *)
val liq_libs_dir : unit -> string

(** Where to look for private executables. *)
val bin_dir : unit -> string

(** Standard path. *)
val path : unit -> string list

(** Maximal id for a request. *)
val requests_max_id : int

val requests_table_size : int

(** Configured directories. Typically /var/(run|log)/liquidsoap. *)
val rundir : unit -> string

val logdir : unit -> string

(** String containing versions of all enabled bindings. *)
val libs_versions : unit -> string
