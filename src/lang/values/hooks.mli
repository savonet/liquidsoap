(* Language essentials *)

type log =
  < f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a >

(** What the language leaves to whoever links it: one implementation each, and a
    stand-in where there is none. *)
type 'a t

val get : 'a t -> 'a

(** Raises [Failure] if the hook already has an implementation. *)
val implement : 'a t -> 'a -> unit

val fallback : 'a t -> 'a -> unit
val make_log : (string list -> log) t
val log : string list -> log
val liq_libs_dir : (unit -> string) t
val log_path : string option ref

(* Media-specific dependencies. *)

val eval_check : (env:(string * Value.t) list -> tm:Term.t -> Value.t -> unit) t

type encoder_params =
  [ `Anonymous of string | `Encoder of encoder | `Labelled of string * Value.t ]
  list

and encoder = string * encoder_params

val make_encoder : (pos:Pos.Option.t -> encoder -> Value.t) t
val type_of_encoder : (pos:Pos.Option.t -> Term.encoder -> Type.t) t
val has_encoder : (Value.t -> bool) t

val mk_source_ty :
  (?pos:Term.parsed_pos -> string -> Parsed_term.source_annotation -> Type.t) t

val mk_clock_ty : (?pos:Term.parsed_pos -> unit -> Type.t) t
val source_methods_t : (unit -> Type.t) t
val getpwnam : (string -> Unix.passwd_entry) ref
