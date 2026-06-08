exception Not_a_plugin
exception Failure

val version : unit -> int * int

type plugin

val default_paths : string list
val load : string -> plugin

type plugin_type = Filter | Source | Mixer2 | Mixer3

val string_of_plugin_type : plugin_type -> string

type color_model = BGRA8888 | RGBA8888 | Packed32

val string_of_color_model : color_model -> string

type info = {
  name : string;
  author : string;
  plugin_type : plugin_type;
  color_model : color_model;
  frei0r_version : int;
  major_version : int;
  minor_version : int;
  num_params : int;
  explanation : string;
}

val info : plugin -> info

type param_type = Bool | Double | Color | Position | String

val string_of_param_type : param_type -> string

type param_info = {
  param_name : string;
  param_type : param_type;
  param_explanation : string;
}

val param_info : plugin -> int -> param_info

type t

val create : plugin -> int -> int -> t

type color = float * float * float
type position = float * float

val get_param_bool : t -> int -> bool
val get_param_float : t -> int -> float
val get_param_color : t -> int -> color
val get_param_position : t -> int -> position
val get_param_string : t -> int -> string
val set_param_bool : t -> int -> bool -> unit
val set_param_float : t -> int -> float -> unit
val set_param_color : t -> int -> color -> unit
val set_param_position : t -> int -> position -> unit
val set_param_string : t -> int -> string -> unit

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val update0 : t -> float -> data -> unit
val update1 : t -> float -> data -> data -> unit
val update2 : t -> float -> data -> data -> data -> unit
val update3 : t -> float -> data -> data -> data -> data -> unit
