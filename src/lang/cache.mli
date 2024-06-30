val enabled : unit -> bool
val dir_override : string option ref
val dir : unit -> string option
val retrieve : ?name:string -> string -> 'a option
val store : string -> 'a -> unit

module Table : sig
  type 'a t

  val load : ?name:string -> string -> 'a t
  val get : 'a t -> string -> (unit -> 'a) -> 'a
  val store : 'a t -> unit
end
