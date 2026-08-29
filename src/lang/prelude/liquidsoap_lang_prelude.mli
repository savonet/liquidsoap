(** Leaf utilities every other liquidsoap-lang library needs. *)

(** Running a function with no other atomic section alongside. *)
module Atomic_section = Atomic_section

(** Locks over shared state. *)
module Mutex_utils = Mutex_utils

(** Positions in source files. *)
module Pos = Pos

(** String escaping, quoting and UTF-8 handling. *)
module Lang_string = Lang_string

(** The hash used to key the on-disk caches. *)
module Term_hash = Term_hash
