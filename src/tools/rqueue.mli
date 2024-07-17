type 'a cell
type 'a t
exception Empty

val create : unit -> 'a t
    (** [create ()] return an empty queue *)

val length : 'a t -> int
    (** [length q] return the number of cells in [q] *)

val is_empty : 'a t -> bool
    (** [is_empty q] return [true] if [q] has no cell *)

val to_list : 'a t -> 'a list
    (** [to_list q] return a list containing all the cells of [q] *)

val top : 'a t -> 'a
    (** [top q] return the cell at the top of the queue *)

val push : 'a t -> 'a -> unit
    (** [push q c] add the cell [c] at the top of the queue [q] *)

val unshift : 'a t -> 'a -> unit
    (** [unshift q c] add the cell [c] at the end of the queue [q] *)

val shift : 'a t -> 'a
    (** [shift q] remove the first cell from [q] and return it *)

val pop : 'a t -> 'a
    (** [pop q] remove the last cell from [q] and return it *)
