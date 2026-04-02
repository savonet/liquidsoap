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

(** [set_offset impl off] returns a new time implementation with [off] as its
    origin: [time ()] returns [raw_time () - off] and [sleep_until] adds [off]
    back before delegating. *)
val set_offset : implementation -> t -> implementation

val unix : implementation
val implementations : (string, implementation) Hashtbl.t
