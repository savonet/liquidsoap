(** Constants describing configuration options of liquidsoap. *)

(** String describing the version. *)
val version : string

(** Substitution of configured variables *)
val var_script : string ref
val subst_vars : string -> string

(** Where to look for standard .liq scripts to include *)
val libs_dir : string

(** Command line entry for dynamic loading of plugins *)
val dynliq_option : (string list * Arg.spec * string) list

(** Function to reencode tags into utf8. *)
val recode_tag : ?encoding:string -> string -> string

(** Maximal id for a request. *)
val requests_max_id : int

(** Magic mime detection *)
val file_mime : string -> string option
val data_mime : ?len:int -> string -> string option

val requests_table_size : int

(** Program used for text-to-speech. *)
val tts_program : string

(** Configured directories. Typically /var/(run|log)/liquidsoap. *)
val rundir : string
val logdir : string

(** Function used to change the sampling frequency of an audio buffer. *)
val resample : (float -> float array -> int -> int -> float array) option

(** Display infered types. *)
val display_types : bool ref

(** Liquidsoap configuration root *)
val conf : Dtools.Conf.ut
