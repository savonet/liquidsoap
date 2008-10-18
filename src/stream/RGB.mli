(** {2 Types} *)

(** An RGBA frame. *)
type t

(** An RGBA color. *)
type color = int * int * int * int

(** {2 Basic manipulation} *)

(** Create a frame of a given width and height. *)
val create : int -> int -> t

(** Copy a frame. *)
val copy : t -> t

(** Get the value of a pixel. *)
val get : t -> int -> int -> color

(** Set the value of a pixel. *)
val set : t -> int -> int -> color -> unit

(** [blit src dst] copies the contents of the frame [src] into [dst]. Both
  * frames must have the same size. *)
val blit : t -> t -> unit

(** {2 Conversions} *)

(** Fill a frame from a YUV420 buffer of a given width. *)
val of_YUV420 : string * string * string -> int -> t

(** Convert a frame to YUV420 format. *)
val to_YUV420 : t -> (string * string * string)

(** Convert a frame to BMP format. *)
val to_bmp : t -> string

(** Convert a frame to an array of [int] of format 0xRRGGBB. Useful for using
  * the [Graphics] module. *)
val to_int_image : t -> int array array

(** {2 Effetcs} *)

(** Fill all the pixel of a frame with a given color. *)
val fill : t -> color -> unit

(** Give a random color to every pixel of the frame (alphas are preserved). *)
val randomize : t -> unit

(** [scale dst src] scales the frame [src] to the frame [dst]. *)
val scale : t -> t -> unit

(** Scale a frame to a given width and height. A new frame is returned. *)
val scale_to : t -> int -> int -> t

(** [proportional_scale dst src] scales the frame [src] to the frame [dst]. The
  * width / height ratio is preserved and black borders are added if necessary. *)
val proportional_scale : t -> t -> unit

(** Same as [proportional_scale] but creates a new frame of given width and
  * height. *)
val proportional_scale_to : t -> int -> int -> t

(** Convert the colors of the frame to greyscale. *)
val greyscale : t -> unit

val invert : t -> unit

val add : t -> t -> unit

val rotate : t -> float -> unit

val scale_opacity : t -> float -> unit

val affine : t -> float -> float -> int -> int -> unit

val mask : t -> t -> unit
