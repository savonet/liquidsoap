(** Bindings to libavutil. The interface is the one of spec/avutil.md §4. *)

type input
type output
type 'a container
type audio = [ `Audio ]
type video = [ `Video ]
type subtitle = [ `Subtitle ]
type media_type = Media_types.t
type ('line, 'media) format
type version = { major : int; minor : int; micro : int }

val version : version
val version_string : version -> string

module Frame : sig
  type 'media t

  val pts : _ t -> Int64.t option
  val set_pts : _ t -> Int64.t option -> unit
  val duration : _ t -> Int64.t option
  val set_duration : _ t -> Int64.t option -> unit
  val pkt_dts : _ t -> Int64.t option
  val set_pkt_dts : _ t -> Int64.t option -> unit
  val metadata : _ t -> (string * string) list
  val set_metadata : _ t -> (string * string) list -> unit
  val best_effort_timestamp : _ t -> Int64.t option
end

type 'media frame = 'media Frame.t

type error =
  [ `Bsf_not_found
  | `Decoder_not_found
  | `Demuxer_not_found
  | `Encoder_not_found
  | `Eof
  | `Exit
  | `Filter_not_found
  | `Invalid_data
  | `Muxer_not_found
  | `Option_not_found
  | `Patch_welcome
  | `Protocol_not_found
  | `Stream_not_found
  | `Bug
  | `Eagain
  | `Unknown
  | `Experimental
  | `Other of int
  | `Failure of string ]

exception Error of error

val string_of_error : error -> string
val expr_parse_and_eval : string -> float

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create_data : int -> data

type rational = { num : int; den : int }

val string_of_rational : rational -> string
val qp2lambda : int

module Time_format : sig
  type t = [ `Second | `Millisecond | `Microsecond | `Nanosecond ]
end

val time_base : unit -> rational

module Log : sig
  type level =
    [ `Quiet
    | `Panic
    | `Fatal
    | `Error
    | `Warning
    | `Info
    | `Verbose
    | `Debug
    | `Trace ]

  val set_level : level -> unit
  val set_callback : (string -> unit) -> unit
  val clear_callback : unit -> unit
end

module Channel_layout : sig
  type layout = Channel_layout.t
  type t

  val standard_layouts : t list
  val stereo : t
  val mono : t
  val five_point_one : t
  val compare : t -> t -> bool
  val find : string -> t
  val get_description : t -> string
  val get_nb_channels : t -> int
  val get_default : int -> t
  val get_mask : t -> int64 option
end

module Sample_format : sig
  type t = Sample_format.t

  val get_name : t -> string option
  val find : string -> t
  val get_id : t -> int
  val find_id : int -> t
end

module Color_space : sig
  type t = Color_space.t

  val name : t -> string
  val from_name : string -> t option
end

module Color_range : sig
  type t = Color_range.t

  val name : t -> string
  val from_name : string -> t option
end

module Color_primaries : sig
  type t = Color_primaries.t

  val name : t -> string
  val from_name : string -> t option
end

module Color_trc : sig
  type t = Color_trc.t

  val name : t -> string
  val from_name : string -> t option
end

module Chroma_location : sig
  type t = Chroma_location.t

  val name : t -> string
  val from_name : string -> t option
end

module Pixel_format : sig
  type t = Pixel_format.t
  type flag = Pixel_format_flag.t

  type component_descriptor = {
    plane : int;
    step : int;
    offset : int;
    shift : int;
    depth : int;
  }

  type descriptor = private {
    name : string;
    nb_components : int;
    log2_chroma_w : int;
    log2_chroma_h : int;
    flags : flag list;
    comp : component_descriptor list;
    alias : string option;
  }

  val descriptor : t -> descriptor
  val bits : descriptor -> int
  val planes : t -> int
  val to_string : t -> string option
  val of_string : string -> t
  val get_id : t -> int
  val find_id : int -> t
end

module Audio : sig
  val create_frame :
    Sample_format.t -> Channel_layout.t -> int -> int -> audio frame

  val frame_get_sample_format : audio frame -> Sample_format.t
  val frame_get_sample_rate : audio frame -> int
  val frame_get_channels : audio frame -> int
  val frame_get_channel_layout : audio frame -> Channel_layout.t
  val frame_nb_samples : audio frame -> int
end

module Video : sig
  type planes = (data * int) array

  val create_frame : int -> int -> Pixel_format.t -> video frame
  val frame_get_linesize : video frame -> int -> int

  val frame_visit :
    make_writable:bool -> (planes -> unit) -> video frame -> video frame

  val frame_get_width : video frame -> int
  val frame_get_height : video frame -> int
  val frame_get_pixel_format : video frame -> Pixel_format.t
  val frame_get_pixel_aspect : video frame -> rational option
  val frame_get_color_space : video frame -> Color_space.t
  val frame_get_color_range : video frame -> Color_range.t
  val frame_get_color_primaries : video frame -> Color_primaries.t
  val frame_get_color_trc : video frame -> Color_trc.t
  val frame_get_chroma_location : video frame -> Chroma_location.t
end

module Subtitle : sig
  type frame
  type subtitle_type = Subtitle_type.t
  type subtitle_flag = Subtitle_flag.t

  val header_ass_default : unit -> string

  type pict = {
    x : int;
    y : int;
    w : int;
    h : int;
    nb_colors : int;
    planes : data array * int array;
  }

  type rectangle = {
    pict : pict option;
    flags : subtitle_flag list;
    rect_type : subtitle_type;
    text : string;
    ass : string;
  }

  type content = {
    format : int;
    start_display_time : int;
    end_display_time : int;
    rectangles : rectangle list;
    pts : int64 option;
  }

  val create_frame : content -> frame
  val get_content : frame -> content
  val get_pts : frame -> int64 option
end

module Options : sig
  type t

  type flag =
    [ `Encoding_param
    | `Decoding_param
    | `Audio_param
    | `Video_param
    | `Subtitle_param
    | `Export
    | `Readonly
    | `Bsf_param
    | `Runtime_param
    | `Filtering_param
    | `Deprecated
    | `Child_consts ]

  type 'a entry = {
    default : 'a option;
    min : 'a option;
    max : 'a option;
    values : (string * 'a) list;
  }

  type ground =
    [ `Flags of int64 entry
    | `Int of int entry
    | `Int64 of int64 entry
    | `Float of float entry
    | `Double of float entry
    | `String of string entry
    | `Rational of rational entry
    | `Binary of string entry
    | `Dict of string entry
    | `UInt64 of int64 entry
    | `Image_size of string entry
    | `Pixel_fmt of Pixel_format.t entry
    | `Sample_fmt of Sample_format.t entry
    | `Video_rate of string entry
    | `Duration of int64 entry
    | `Color of string entry
    | `Channel_layout of Channel_layout.t entry
    | `Bool of bool entry ]

  type spec = [ ground | `Array of ground ]

  type opt = {
    name : string;
    help : string option;
    flags : flag list;
    spec : spec;
  }

  val opts : t -> opt list

  type obj
  type 'a getter = ?search_children:bool -> name:string -> obj -> 'a

  val get_string : string getter
  val get_int : int getter
  val get_int64 : int64 getter
  val get_float : float getter
  val get_rational : rational getter
  val get_image_size : (int * int) getter
  val get_pixel_fmt : Pixel_format.t getter
  val get_sample_fmt : Sample_format.t getter
  val get_video_rate : rational getter
  val get_channel_layout : Channel_layout.t getter
  val get_dictionary : (string * string) list getter
end

type value =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]

type opts = (string, value) Hashtbl.t

val string_of_opts : opts -> string
val filter_opts : string array -> opts -> unit

module HwContext : sig
  type device_type = Hw_device_type.t
  type device_context
  type frame_context

  val create_device_context :
    ?device:string -> ?opts:opts -> device_type -> device_context

  val create_frame_context :
    width:int ->
    height:int ->
    src_pixel_format:Pixel_format.t ->
    dst_pixel_format:Pixel_format.t ->
    device_context ->
    frame_context
end
