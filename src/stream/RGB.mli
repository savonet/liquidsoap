(** Raised when reading from a buffer of a file which is not of the expected
  * format *)
exception Invalid_format of string

(** {2 Types} *)

(** An RGBA frame. *)
type t

(** An RGBA color. *)
type color = int * int * int * int

(** {2 Utility functions} *)
val rgb_of_int : int -> int * int * int

(** {2 Basic manipulation} *)

(** Create a frame of a given width and height. *)
val create : int -> int -> t

(** Copy a frame. *)
val copy : t -> t

(** Width of a frame. *)
val get_width : t -> int

(** Height of a frame. *)
val get_height : t -> int

(** Get the value of a pixel. *)
val get_pixel : t -> int -> int -> color

(** Set the value of a pixel. *)
val set_pixel : t -> int -> int -> color -> unit

(** [blit src dst] copies the contents of the frame [src] into [dst]. Both
  * frames must have the same size. *)
val blit : t -> t -> unit

(** [blit src dst dx dy] blits [src] into [dst] with a translation of [(dx,
  * dy)]. Frames don't have to be of the same size. *)
val blit_off : t -> t -> int -> int -> unit

(** [add dst src] adds the frame [src] on top of the frame [dst] in [dst]. *)
val add : t -> t -> unit

(** {2 Conversions} *)

(** Fill a frame from a YUV420 buffer of a given width. *)
val of_YUV420 : string * string * string -> int -> t

(** Convert a frame to YUV420 format. *)
val to_YUV420 : t -> (string * string * string)

(** Convert a frame to BMP format. *)
val to_bmp : t -> string

(** Save frame in a bitmap file. *)
val save_bmp : t -> string -> unit

(** Read a PPM in a string. [alpha] is an optional color meaning transparency. *)
val of_ppm : ?alpha:(int * int * int) -> string -> t

(** Same as [of_ppm] but reads PPM from a file. *)
val read_ppm : ?alpha:(int * int * int) -> string -> t

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

(** Convert the colors of a frame to greyscale. *)
val greyscale : t -> unit

(** Invert the colors of a frame. *)
val invert : t -> unit

(** Rotate an image by a given angle (in radians). *)
val rotate : t -> float -> unit

(** Multiply opacity of the image by a coefficient (between 0 and 1). *)
val scale_opacity : t -> float -> unit

(** [affine ax ay bx by] scales the image by [(ax, ay)] and translates it of
  * [(ox, oy)]. *)
val affine : t -> float -> float -> int -> int -> unit

(** [mask f m] sets the alpha mask of the frame [f] according to the frame [m]. *)
val mask : t -> t -> unit

(** Emulate the "Lomo effect". *)
val lomo : t -> unit
