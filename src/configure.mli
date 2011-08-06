(** Constants describing configuration options of liquidsoap. *)

(** String describing the version. *)
val version : string

(** Is this build a SVN snapshot ? *)
val svn_snapshot : bool

(** Substitution of configured variables *)
val var_script : string ref
val subst_vars : string -> string

(** Where to look for standard .liq scripts to include *)
val libs_dir : string

(** Directories where to search for libraries.. *)
val findlib_path : string list

(** Is dynlink available? *)
val dynlink : bool

(** Where to look for dynamically loadable modules (cmxs). *)
val plugins_dir : string

(** Helper to load dynamic modules. *)
val load_dynlinks : unit -> unit

(** Helper to load dynamic plugins. *)
val load_plugins_dir : string -> unit

(** Function to reencode tags into utf8. *)
val recode_tag : ?in_enc:string -> ?out_enc:string -> string -> string

(** Maximal id for a request. *)
val requests_max_id : int

(** Magic mime detection *)
val file_mime : (string -> string) option
val data_mime : (?len:int -> string -> string) option

val requests_table_size : int

(** Program used for text-to-speech. *)
val tts_program : string

(** Configured directories. Typically /var/(run|log)/liquidsoap. *)
val rundir : string
val logdir : string

(** Display infered types. *)
val display_types : bool ref

(** Liquidsoap configuration root *)
val conf : Dtools.Conf.ut

(** Is the architecture big endian? *)
val big_endian : bool

(** String containing versions of all enabled bindings. *)
val libs_versions : string
