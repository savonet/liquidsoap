type dirtype = [ `System | `User ]

val user_file_perms : int ref
val user_dir_perms : int ref
val system_file_perms : int ref
val system_dir_perms : int ref
val enabled : unit -> bool
val user_dir_override : (unit -> string option) ref
val system_dir_override : (unit -> string option) ref
val dir : dirtype -> string option
val retrieve : ?name:string -> dirtype:dirtype -> string -> 'a option
val store : dirtype:dirtype -> string -> 'a -> unit

module Table : sig
  type 'a t

  val load : ?name:string -> dirtype:dirtype -> string -> 'a t
  val get : 'a t -> string -> (unit -> 'a) -> 'a
  val store : dirtype:dirtype -> 'a t -> unit
end
