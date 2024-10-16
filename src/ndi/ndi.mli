(** Binding to the proprietary NDI library.
    Please refer to the library's documentation for details
    regarding this binding's functions. *)

type t
type source = { source_name : string; source_url : string }

exception Library_not_found
exception Library_initialized of string

val init : filename:string -> unit -> t
val version : t -> string

val find :
  ?show_local_sources:bool ->
  ?groups:string ->
  ?extra_ips:string list ->
  ?timeout:int ->
  t ->
  source list

module Frame : sig
  module Audio : sig
    type fltp = {
      data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
      stride : int;
    }

    type data = [ `Fltp of fltp ]

    type t = {
      sample_rate : int;
      channels : int;
      samples : int;
      timecode : int64 option;
      data : data;
      metadata : string option;
      timestamp : int64 option;
    }
  end

  module Video : sig
    type i420 = {
      data :
        (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
      stride : int;
    }

    type data = [ `I420 of i420 ]
    type format = [ `Progressive ]

    type t = {
      xres : int;
      yres : int;
      frame_rate_N : int;
      frame_rate_D : int;
      picture_aspect_ratio : float option;
      format : format;
      timecode : int64 option;
      data : data;
      metadata : string option;
      timestamp : int64 option;
    }
  end
end

module Send : sig
  type sender

  val init :
    ?clock_audio:bool ->
    ?clock_video:bool ->
    ?groups:string ->
    ?name:string ->
    t ->
    sender

  val send_audio : sender -> Frame.Audio.t -> unit
  val send_video : sender -> Frame.Video.t -> unit
  val destroy : sender -> unit
end
