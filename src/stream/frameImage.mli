module Generic = Image.Generic

type t

val create : int -> int -> t

val of_RGB24_string : string -> int -> t

val of_I420_string : string -> int -> t

val to_generic : t -> Generic.t -> unit

(** Convert to format useable by [Graphics.make_image]. *)
val to_int_image : t -> int array array

val copy : t -> t

val width : t -> int

val height : t -> int

val dimensions : t -> int * int

(** Size in bytes. *)
val size : t -> int

val blank : t -> unit

val randomize : t -> unit

(** [blit_all src dst] blits an entire image. *)
val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

val get_pixel : t -> int -> int -> int * int * int * int

val set_pixel : t -> int -> int -> (int * int * int * int) -> unit

(** Add the fist image to the second. *)
val add : t -> ?x:int -> ?y:int -> t -> unit

module Effect : sig
  val greyscale : t -> unit

  val sepia : t -> unit

  val invert : t -> unit

  val lomo : t -> unit

  val translate : t -> int -> int -> unit

  module Alpha : sig
    val scale : t -> float -> unit
  end
end
