type converter =
  | Conv_sinc_best_quality
  | Conv_sinc_medium_quality
  | Conv_fastest
  | Conv_zero_order_hold
  | Conv_linear

external get_conv_name : converter -> string = "ocaml_samplerate_get_conv_name"

external get_conv_descr : converter -> string
  = "ocaml_samplerate_get_conv_descr"

external convert :
  converter -> int -> float -> float array -> int -> int -> float array
  = "ocaml_samplerate_convert_byte" "ocaml_samplerate_convert"

type t

external create : converter -> int -> t = "ocaml_samplerate_new"

external process :
  t ->
  float ->
  float array ->
  int ->
  int ->
  float array ->
  int ->
  int ->
  int * int = "ocaml_samplerate_process_byte" "ocaml_samplerate_process"

external process_ba :
  t ->
  float ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int * int = "ocaml_samplerate_process_ba"

external process_alloc : t -> float -> float array -> int -> int -> float array
  = "ocaml_samplerate_process_alloc"

external reset : t -> unit = "ocaml_samplerate_reset"
