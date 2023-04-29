(* Language essentials *)

val regexp : (?flags:Regexp.flag list -> string -> Regexp.regexp) ref

type log =
  < f :
      'a.
      ?pre_process:(string -> string) ->
      int ->
      ('a, unit, string, unit) format4 ->
      'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a >

val make_log : (string list -> log) ref
val log : string list -> log
val liq_libs_dir : (unit -> string) ref
val log_path : string option ref

(* Media-specific dependencies. *)

val eval_check :
  (env:(string * Value.t lazy_t) list -> tm:Term.term -> Value.t -> unit) ref

type encoder_params =
  (string * [ `Value of Value.t | `Encoder of encoder ]) list

and encoder = string * encoder_params

val make_encoder : (pos:Pos.Option.t -> Term.t -> encoder -> Value.t) ref
val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) ref
val has_encoder : (Value.t -> bool) ref
val collect_after : ((unit -> Value.t) -> Value.t) ref

val mk_source_ty :
  (pos:Pos.t ->
  string ->
  (string * (string * (string * string) list)) list ->
  Type.t)
  ref

val source_methods_t : (unit -> Type.t) ref
val getpwnam : (string -> Unix.passwd_entry) ref
