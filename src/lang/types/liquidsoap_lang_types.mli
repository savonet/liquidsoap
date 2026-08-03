(** The type system: representation, constraints, custom types, unification and
    the printable form used in error messages. *)

(** Types and their constructors. *)
module Type = Type

(** The raw representation behind {!Type}, for the few places that need to build
    or traverse it directly. *)
module Type_base = Type_base

(** Registering an OCaml type as a liquidsoap type. *)
module Type_custom = Type_custom

(** The printable representation of a type, used in error messages. *)
module Repr = Repr

(** Unification, subtyping, generalization and instantiation. *)
module Typing = Typing
