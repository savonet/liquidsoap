(** Binding to the proprietary StereoTool processing library.
    Please refer to the library's documentation for details
    regarding this binding's functions. *)

type t

type load_type =
  [ `Totalinit
  | `All_settings
  | `Audiofm
  | `Audio
  | `Processing
  | `Repair
  | `Repair_no_pnr
  | `Sublevel_pnr ]

exception Library_not_found
exception Library_initialized of string

val init : ?license_key:string -> filename:string -> unit -> t
val software_version : t -> int
val api_version : t -> int
val valid_license : t -> bool
val unlincensed_used_features : t -> string option
val load_preset : ?load_type:load_type -> filename:string -> t -> bool
val latency : samplerate:int -> feed_silence:bool -> t -> int

val process_interleaved :
  samplerate:int -> channels:int -> t -> float array -> int -> int -> unit

val process : samplerate:int -> t -> float array array -> int -> int -> unit
