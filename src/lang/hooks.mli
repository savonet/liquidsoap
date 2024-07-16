(* Language essentials *)

type log =
  < f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a >

val make_log : (string list -> log) ref
val log : string list -> log
val liq_libs_dir : (unit -> string) ref
val log_path : string option ref

type dirtype = [ `User | `System ]

val cache_maintenance : (dirtype -> unit) ref

(* Media-specific dependencies. *)

val eval_check :
  (env:(string * Value.t) list -> tm:Term.t -> Value.t -> unit) ref

type encoder_params =
  [ `Anonymous of string | `Encoder of encoder | `Labelled of string * Value.t ]
  list

and encoder = string * encoder_params

val make_encoder : (pos:Pos.Option.t -> encoder -> Value.t) ref
val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) ref
val has_encoder : (Value.t -> bool) ref

val mk_source_ty :
  (?pos:Term_base.parsed_pos ->
  string ->
  Parsed_term.source_annotation ->
  Type.t)
  ref

val mk_clock_ty : (?pos:Term_base.parsed_pos -> unit -> Type.t) ref
val source_methods_t : (unit -> Type.t) ref
val getpwnam : (string -> Unix.passwd_entry) ref
