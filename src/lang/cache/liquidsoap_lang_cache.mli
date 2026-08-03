(** The on-disk caches. *)

(** The marshalling store and its maintenance. *)
module Cache = Cache

(** The typechecking cache, keyed on the parsed term and the builtin
    environment. *)
module Term_cache = Term_cache
