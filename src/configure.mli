(** String describing the version. *)
val version : string

(** Where to look for .liq scripts to include *)
val libs_path : string list

(** Command line entry for dynamic loading of plugins *)
val dynliq_option : (string list * Arg.spec * string) list

(** Function to reencode tags into utf8. *)
val recode_tag : string -> string

(** Maximal id for a request. *)
val requests_max_id : int

val requests_table_size : int

(** Program used for text-to-speech. *)
val tts_program : string

(** Configured directories. Typically /var/(run|log)/liquidsoap. *)
val rundir : string
val logdir : string

(** Function used to change the sampling frequency of an audio buffer. *)
val resample : (float -> float array -> int -> int -> float array) option

(** Should we include standard scripts automatically *)
val load_libs : bool ref
