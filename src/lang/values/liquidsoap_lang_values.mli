(** Runtime values and the global environment they are registered in. *)

(** Runtime values: what a script evaluates to. *)
module Value = Value

(** The global environment builtins are registered in. *)
module Environment = Environment

(** Capabilities the language needs but cannot implement, filled in by the
    streaming core. See the comment at the top of hooks.ml. *)
module Hooks = Hooks

(** Errors raised by builtins. *)
module Error = Error
