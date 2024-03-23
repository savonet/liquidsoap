include module type of Stdlib.Atomic

module Awaitable : sig
  type 'a t

  val make : 'a -> 'a t
  val get : 'a t -> 'a Future.t
  val set : 'a t -> 'a -> unit
end
