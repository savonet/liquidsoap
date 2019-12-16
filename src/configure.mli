(** Constants describing configuration options of liquidsoap. *)

(** String describing the version. *)
val version : string

val restart : bool ref

(** Is this build a git snapshot ? *)
val git_commit : string

val git_snapshot : bool

(** String describing the software. *)
val vendor : string

(** Substitution of configured variables *)
val var_script : string ref

val subst_vars : string -> string

(** Where to look for standard .liq scripts to include *)
val libs_dir : string

(** Directories where to search for libraries.. *)
val findlib_path : string list

(** Is dynlink available? *)
val dynlink : bool

(** Where to look for private executables. *)
val bin_dir : string

(** Standard path. *)
val path : string list

(** Helper for all things that need to be
  * executed before script parsing. *)
val at_init : (unit -> unit) -> unit

val run_init : unit -> unit

(** Executable extension. *)
val exe_ext : string

(** Default font file *)
val default_font : string

(** Function to reencode tags into utf8. *)
val recode_tag : ?in_enc:string -> ?out_enc:string -> string -> string

(** Maximal id for a request. *)
val requests_max_id : int

(** Magic mime detection *)
val file_mime : (string -> string) option

val data_mime : (?len:int -> string -> string) option
val requests_table_size : int

(** Configured directories. Typically /var/(run|log)/liquidsoap. *)
val rundir : string

val logdir : string

(** Display inferred types. *)
val display_types : bool ref

(** Liquidsoap configuration root *)
val conf : Dtools.Conf.ut

(** Is the architecture big endian? *)
val big_endian : bool

(** String containing versions of all enabled bindings. *)
val libs_versions : string

(** File watch utility. *)
val file_watcher : File_watcher.watch ref

(** JSON parser. *)
module JSON : module type of JSON
