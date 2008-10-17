type t

type color = int * int * int

(** Create a frame of a given width / height. *)
val create : int -> int -> t

val copy : t -> t

(** [blit src dst] *)
val blit : t -> t -> unit

val fill : t -> color -> unit

(** Fill from a YUV420 buffer (of a given width). *)
val of_YUV420 : string * string * string -> int -> t

val to_YUV420 : t -> (string * string * string)

(** Get the value of a pixel. *)
val get : t -> int -> int -> color

val set : t -> int -> int -> color -> unit

val randomize : t -> unit

val scale : t -> t -> unit

val scale_to : t -> int -> int -> t

val proportional_scale : t -> t -> unit

val proportional_scale_to : t -> int -> int -> t

val to_bmp : t -> string

val to_int_image : t -> int array array

val greyscale : t -> unit

val invert : t -> unit

val add : t -> t -> unit

val rotate : t -> float -> unit

val scale_opacity : t -> float -> unit
