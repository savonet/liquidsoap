(** To be filled when source values are instantiated. *)
val source_eval_check :
  (k:Content_unifier.t -> pos:Pos.Option.t -> Value.t -> unit) ref

(** To be filled when encoder values are instantiated. *)
type encoder_params =
  (string * [ `Value of Value.t | `Encoder of encoder ]) list

and encoder = string * encoder_params

val make_encoder : (pos:Pos.Option.t -> Term.t -> encoder -> Value.t) ref
val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) ref
val has_encoder : (Value.t -> bool) ref
val liq_libs_dir : (unit -> string) ref
val log_path : string option ref
val collect_after : ((unit -> Value.t) -> Value.t) ref

module type Regexp_t = Regexp.T

val regexp : (module Regexp_t) ref

type log =
  < f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a >

val make_log : (string list -> log) ref
val log : string list -> log
