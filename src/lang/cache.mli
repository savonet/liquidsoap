val dir : unit -> string option
val retrieve : string -> 'a option
val store : string -> 'a -> unit

module Table : sig
  type 'a t

  val load : string -> 'a t
  val get : 'a t -> string -> (unit -> 'a) -> 'a
  val store : 'a t -> unit
end
