(** To be filled when source values are instantiated. *)
val source_eval_check :
  (k:Frame.content_kind -> pos:Pos.Option.t -> Value.t -> unit) ref

(** To be filled when encoder values are instantiated. *)
type encoder_params =
  (string * [ `Value of Value.t | `Encoder of encoder ]) list

and encoder = string * encoder_params

val make_encoder : (pos:Pos.Option.t -> Term.t -> encoder -> Value.t) ref
val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) ref
val has_encoder : (Value.t -> bool) ref
val liq_libs_dir : (unit -> string) ref
val version : (unit -> string) ref
val log_path : string option ref
val collect_after : ((unit -> Value.t) -> Value.t) ref
