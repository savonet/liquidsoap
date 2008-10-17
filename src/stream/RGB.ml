type t

type color = int * int * int * int

external create : int -> int -> t = "caml_rgb_create"

external copy : t -> t = "caml_rgb_copy"

external blit : t -> t -> unit = "caml_rgb_blit"

external fill : t -> color -> unit = "caml_rgb_fill"

external of_YUV420 : string * string * string -> t -> unit = "caml_rgb_of_YUV420"

let of_YUV420 (y, u, v) width =
  let height = String.length y / width in
  let ans = create width height in
    of_YUV420 (y, u, v) ans;
    ans

external to_YUV420 : t -> string * string * string = "caml_rgb_to_YUV420"

external get : t -> int -> int -> color = "caml_rgb_get"

external set : t -> int -> int -> color -> unit = "caml_rgb_set"

external randomize : t -> unit = "caml_rgb_randomize"

external scale : t -> t -> unit = "caml_rgb_scale"

let scale_to src w h =
  let dst = create w h in
    scale dst src;
    dst

external proportional_scale : t -> t -> unit = "caml_rgb_proportional_scale"

let proportional_scale_to src w h =
  let dst = create w h in
    proportional_scale dst src;
    dst

external to_bmp : t -> string = "caml_rgb_to_bmp"

external to_int_image : t -> int array array = "caml_rgb_to_color_array"

external greyscale : t -> unit = "caml_rgb_greyscale"

external invert : t -> unit = "caml_rgb_invert"

external add : t -> t -> unit = "caml_rgb_add"

external rotate : t -> float -> unit = "caml_rgb_rotate"

external scale_opacity : t -> float -> unit = "caml_rgb_scale_opacity"

external affine : t -> float -> float -> int -> int -> unit = "caml_rgb_affine"
