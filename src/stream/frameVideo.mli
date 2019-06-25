module Image = FrameImage

type t

val make : int -> int -> int -> t

(** Video with a single image. *)
val single : Image.t -> t

val blit : t -> int -> t -> int -> int -> unit

val copy : t -> t

(** Length in images. *)
val length : t -> int

(** Size in bytes. *)
val size : t -> int

(** Obtaine the i-th image of a video. *)
val get : t -> int -> Image.t

val iter : (Image.t -> unit) -> t -> int -> int -> unit

val blank : t -> int -> int -> unit
