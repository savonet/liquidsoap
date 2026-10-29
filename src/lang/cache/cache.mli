type dirtype = [ `System | `User ]

val user_file_perms : int ref
val user_dir_perms : int ref
val system_file_perms : int ref
val system_dir_perms : int ref
val enabled : unit -> bool
val user_dir_override : (unit -> string option) ref
val system_dir_override : (unit -> string option) ref
val dir : dirtype -> string option

(** Evict entries that are too old, and the oldest ones past
    [$LIQ_CACHE_MAX_FILES]. Run automatically after every [store]. *)
val maintenance : dirtype -> unit

(** What a cache file holds, checked before unmarshalling. *)
type kind = [ `Term | `Table ]

(** [None] unless the file carries this build's header for [kind] and its
    payload hash matches; a rejected file is reported and left to be rewritten.
*)
val retrieve :
  ?name:string -> dirtype:dirtype -> kind:kind -> string -> 'a option

val store : dirtype:dirtype -> kind:kind -> string -> 'a -> unit

module Table : sig
  type 'a t

  val load : ?name:string -> dirtype:dirtype -> string -> 'a t
  val get : 'a t -> string -> (unit -> 'a) -> 'a
  val store : dirtype:dirtype -> 'a t -> unit
end
