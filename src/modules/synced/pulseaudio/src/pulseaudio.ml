exception Error of int

let () = Callback.register_exception "pa_exn_error" (Error 0)

external string_of_error : int -> string = "ocaml_pa_strerror"

(* TODO *)
type sample_format =
  | Sample_format_s16le
  | Sample_format_s16be
  | Sample_format_float32le
  | Sample_format_float32be

type sample = {
  sample_format : sample_format;
  sample_rate : int;
  sample_chans : int;
}

type map
type dir = Dir_nodirection | Dir_playback | Dir_record | Dir_upload

type buffer_attr = {
  max_length : int;
  target_length : int;
  prebuffering : int;
  min_request : int;
  fragment_size : int;
}

module Simple = struct
  type t

  external free : t -> unit = "ocaml_pa_simple_free"

  external create :
    string option ->
    string ->
    dir ->
    string option ->
    string ->
    sample ->
    map option ->
    buffer_attr option ->
    t = "ocaml_pa_simple_new_byte" "ocaml_pa_simple_new"

  let create ?server ~client_name ~dir ?dev ~stream_name ~sample ?map ?attr () =
    create server client_name dir dev stream_name sample map attr

  external write : t -> float array array -> int -> int -> unit
    = "ocaml_pa_simple_write_float"

  external write_floatarray : t -> floatarray array -> int -> int -> unit
    = "ocaml_pa_simple_write_floatarray"

  external write_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit = "ocaml_pa_simple_write_float_ba"

  external drain : t -> unit = "ocaml_pa_simple_drain"
  external flush : t -> unit = "ocaml_pa_simple_flush"
  external latency : t -> int = "ocaml_pa_simple_get_latency"

  external read : t -> float array array -> int -> int -> unit
    = "ocaml_pa_read_float"

  external read_floatarray : t -> floatarray array -> int -> int -> unit
    = "ocaml_pa_read_floatarray"

  external read_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit = "ocaml_pa_read_float_ba"
end
