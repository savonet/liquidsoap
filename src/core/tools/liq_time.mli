type t

module type T = sig
  val implementation : string
  val time : unit -> t
  val sleep_until : t -> unit
  val of_float : float -> t
  val to_float : t -> float
  val ( |+| ) : t -> t -> t
  val ( |-| ) : t -> t -> t
  val ( |*| ) : t -> t -> t
  val ( |<| ) : t -> t -> bool
  val ( |<=| ) : t -> t -> bool
end

type implementation = (module T)

val unix : implementation
val implementations : (string, implementation) Hashtbl.t
