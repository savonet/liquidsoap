type input
type output
type 'a container
type audio = [ `Audio ]
type video = [ `Video ]
type subtitle = [ `Subtitle ]
type media_type = Media_types.t
type ('line, 'media) format

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

let () = Callback.register_exception "ocaml_avutil_error" (Error `Unknown)

external string_of_error : error -> string = "ocaml_avutil_string_of_error"

let string_of_error = function
  | `Failure message -> message
  | error -> string_of_error error

let () =
  Printexc.register_printer (function
    | Error error ->
        Some (Printf.sprintf "Avutil.Error(%s)" (string_of_error error))
    | _ -> None)

let failure fmt =
  Printf.ksprintf (fun message -> raise (Error (`Failure message))) fmt

type version = { major : int; minor : int; micro : int }

external version : unit -> version = "ocaml_avutil_version"

let version = version ()

let version_string { major; minor; micro } =
  Printf.sprintf "%d.%d.%d" major minor micro

module Frame = struct
  type 'media t

  external pts : _ t -> Int64.t option = "ocaml_avutil_frame_pts"

  external set_pts : _ t -> Int64.t option -> unit
    = "ocaml_avutil_frame_set_pts"

  external duration : _ t -> Int64.t option = "ocaml_avutil_frame_duration"

  external set_duration : _ t -> Int64.t option -> unit
    = "ocaml_avutil_frame_set_duration"

  external pkt_dts : _ t -> Int64.t option = "ocaml_avutil_frame_pkt_dts"

  external set_pkt_dts : _ t -> Int64.t option -> unit
    = "ocaml_avutil_frame_set_pkt_dts"

  external metadata : _ t -> (string * string) list
    = "ocaml_avutil_frame_metadata"

  external set_metadata : _ t -> (string * string) list -> unit
    = "ocaml_avutil_frame_set_metadata"

  external best_effort_timestamp : _ t -> Int64.t option
    = "ocaml_avutil_frame_best_effort_timestamp"
end

type 'media frame = 'media Frame.t

external expr_parse_and_eval : string -> float
  = "ocaml_avutil_expr_parse_and_eval"

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create_data length =
  if length < 0 then failure "negative data length";
  Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout length

type rational = { num : int; den : int }

let string_of_rational { num; den } = Printf.sprintf "%d/%d" num den

external qp2lambda : unit -> int = "ocaml_avutil_qp2lambda"

let qp2lambda = qp2lambda ()

module Time_format = struct
  type t = [ `Second | `Millisecond | `Microsecond | `Nanosecond ]
end

external time_base : unit -> rational = "ocaml_avutil_time_base"

(* Log delivery, spec/avutil.md §7.1.

   The C side queues every captured message and numbers it. A receiver is a
   callback with the range of message numbers captured while it was
   installed: [set_callback] opens a range, [clear_callback] and a
   replacement close it. One thread, started by the first [set_callback],
   takes the messages in order and gives each to the receiver whose range
   holds its number.

   [mutex] protects [state] and is never held while a callback runs. *)
module Log = struct
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

  external set_level : level -> unit = "ocaml_avutil_set_log_level"
  external start_capture : unit -> int = "ocaml_avutil_log_start_capture"
  external stop_capture : unit -> int = "ocaml_avutil_log_stop_capture"
  external wait : unit -> string array = "ocaml_avutil_log_wait"

  type receiver = { first : int; mutable last : int; callback : string -> unit }

  type state = {
    mutable receivers : receiver list;
    mutable delivered : int;
    mutable delivery_thread : Thread.t option;
  }

  let state = { receivers = []; delivered = 0; delivery_thread = None }
  let mutex = Mutex.create ()
  let delivered_changed = Condition.create ()

  (* Ends, at message number [last], the range of every receiver. *)
  let close_receivers last =
    List.iter (fun r -> r.last <- min r.last last) state.receivers;
    state.receivers <-
      List.filter (fun r -> r.last > state.delivered) state.receivers

  let deliver message =
    let receiver =
      Mutex.protect mutex (fun () ->
          List.find_opt
            (fun r -> r.first <= state.delivered && state.delivered < r.last)
            state.receivers)
    in
    (match receiver with
      | None -> ()
      | Some { callback; _ } -> (
          try callback message
          with exn ->
            Printf.eprintf "Avutil.Log: the log callback raised %s\n%!"
              (Printexc.to_string exn)));
    Mutex.protect mutex (fun () ->
        state.delivered <- state.delivered + 1;
        close_receivers max_int;
        Condition.broadcast delivered_changed)

  let rec delivery_loop () =
    Array.iter deliver (wait ());
    delivery_loop ()

  (* ponytail: the delivery thread belongs to the domain of the first
     [set_callback]; a dedicated domain if that domain may terminate. *)
  let set_callback callback =
    Mutex.protect mutex (fun () ->
        let first = start_capture () in
        close_receivers first;
        state.receivers <-
          { first; last = max_int; callback } :: state.receivers;
        if state.delivery_thread = None then
          state.delivery_thread <- Some (Thread.create delivery_loop ()))

  let on_delivery_thread () =
    match state.delivery_thread with
      | Some thread -> Thread.id thread = Thread.id (Thread.self ())
      | None -> false

  let clear_callback () =
    Mutex.protect mutex (fun () ->
        let last = stop_capture () in
        close_receivers last;
        if not (on_delivery_thread ()) then
          while state.delivered < last do
            Condition.wait delivered_changed mutex
          done)
end

module Channel_layout = struct
  type layout = Channel_layout.t
  type t

  external standard_layouts : unit -> t array
    = "ocaml_avutil_standard_channel_layouts"

  external find : string -> t = "ocaml_avutil_find_channel_layout"
  external compare : t -> t -> bool = "ocaml_avutil_compare_channel_layouts"

  external get_description : t -> string
    = "ocaml_avutil_channel_layout_description"

  external get_nb_channels : t -> int
    = "ocaml_avutil_channel_layout_nb_channels"

  external get_default : int -> t = "ocaml_avutil_default_channel_layout"
  external get_mask : t -> int64 option = "ocaml_avutil_channel_layout_mask"

  let standard_layouts = Array.to_list (standard_layouts ())
  let stereo = find "stereo"
  let mono = find "mono"
  let five_point_one = find "5.1"
end

module Sample_format = struct
  type t = Sample_format.t

  external get_name : t -> string option = "ocaml_avutil_sample_format_name"
  external find : string -> t = "ocaml_avutil_find_sample_format"
  external get_id : t -> int = "ocaml_avutil_sample_format_id"
  external find_id : int -> t = "ocaml_avutil_find_sample_format_id"
end

module Color_space = struct
  type t = Color_space.t

  external name : t -> string = "ocaml_avutil_color_space_name"
  external from_name : string -> t option = "ocaml_avutil_color_space_from_name"
end

module Color_range = struct
  type t = Color_range.t

  external name : t -> string = "ocaml_avutil_color_range_name"
  external from_name : string -> t option = "ocaml_avutil_color_range_from_name"
end

module Color_primaries = struct
  type t = Color_primaries.t

  external name : t -> string = "ocaml_avutil_color_primaries_name"

  external from_name : string -> t option
    = "ocaml_avutil_color_primaries_from_name"
end

module Color_trc = struct
  type t = Color_trc.t

  external name : t -> string = "ocaml_avutil_color_trc_name"
  external from_name : string -> t option = "ocaml_avutil_color_trc_from_name"
end

module Chroma_location = struct
  type t = Chroma_location.t

  external name : t -> string = "ocaml_avutil_chroma_location_name"

  external from_name : string -> t option
    = "ocaml_avutil_chroma_location_from_name"
end

module Pixel_format = struct
  type t = Pixel_format.t
  type flag = Pixel_format_flag.t

  type component_descriptor = {
    plane : int;
    step : int;
    offset : int;
    shift : int;
    depth : int;
  }

  type descriptor = {
    name : string;
    nb_components : int;
    log2_chroma_w : int;
    log2_chroma_h : int;
    flags : flag list;
    comp : component_descriptor list;
    alias : string option;
  }

  external descriptor : t -> descriptor = "ocaml_avutil_pixel_format_descriptor"
  external bits : descriptor -> int = "ocaml_avutil_pixel_format_bits"
  external planes : t -> int = "ocaml_avutil_pixel_format_planes"
  external to_string : t -> string option = "ocaml_avutil_pixel_format_name"
  external of_string : string -> t = "ocaml_avutil_find_pixel_format"
  external get_id : t -> int = "ocaml_avutil_pixel_format_id"
  external find_id : int -> t = "ocaml_avutil_find_pixel_format_id"
end

module Audio = struct
  external create_frame :
    Sample_format.t -> Channel_layout.t -> int -> int -> audio frame
    = "ocaml_avutil_audio_create_frame"

  external frame_get_sample_format : audio frame -> Sample_format.t
    = "ocaml_avutil_audio_frame_sample_format"

  external frame_get_sample_rate : audio frame -> int
    = "ocaml_avutil_audio_frame_sample_rate"

  external frame_get_channels : audio frame -> int
    = "ocaml_avutil_audio_frame_channels"

  external frame_get_channel_layout : audio frame -> Channel_layout.t
    = "ocaml_avutil_audio_frame_channel_layout"

  external frame_nb_samples : audio frame -> int
    = "ocaml_avutil_audio_frame_nb_samples"
end

module Video = struct
  type planes = (data * int) array

  external create_frame : int -> int -> Pixel_format.t -> video frame
    = "ocaml_avutil_video_create_frame"

  external frame_get_linesize : video frame -> int -> int
    = "ocaml_avutil_video_frame_linesize"

  external frame_planes : video frame -> bool -> planes
    = "ocaml_avutil_video_frame_planes"

  (* The finaliser holds the frame, which owns every buffer a plane of its
     visits points into: the buffers live as long as the bigarrays. *)
  let frame_visit ~make_writable visit frame =
    let planes = frame_planes frame make_writable in
    Array.iter
      (fun (data, _) ->
        Gc.finalise (fun _ -> ignore (Sys.opaque_identity frame)) data)
      planes;
    visit planes;
    frame

  external frame_get_width : video frame -> int
    = "ocaml_avutil_video_frame_width"

  external frame_get_height : video frame -> int
    = "ocaml_avutil_video_frame_height"

  external frame_get_pixel_format : video frame -> Pixel_format.t
    = "ocaml_avutil_video_frame_pixel_format"

  external frame_get_pixel_aspect : video frame -> rational option
    = "ocaml_avutil_video_frame_pixel_aspect"

  external frame_get_color_space : video frame -> Color_space.t
    = "ocaml_avutil_video_frame_color_space"

  external frame_get_color_range : video frame -> Color_range.t
    = "ocaml_avutil_video_frame_color_range"

  external frame_get_color_primaries : video frame -> Color_primaries.t
    = "ocaml_avutil_video_frame_color_primaries"

  external frame_get_color_trc : video frame -> Color_trc.t
    = "ocaml_avutil_video_frame_color_trc"

  external frame_get_chroma_location : video frame -> Chroma_location.t
    = "ocaml_avutil_video_frame_chroma_location"
end

module Subtitle = struct
  type frame
  type subtitle_type = Subtitle_type.t
  type subtitle_flag = Subtitle_flag.t

  let header_ass_default () =
    String.concat "\r\n"
      [
        "[Script Info]";
        "; Script generated by ocaml-ffmpeg";
        "ScriptType: v4.00+";
        "PlayResX: 384";
        "PlayResY: 288";
        "ScaledBorderAndShadow: yes";
        "";
        "[V4+ Styles]";
        "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, \
         OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, \
         ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, \
         Alignment, MarginL, MarginR, MarginV, Encoding";
        "Style: \
         Default,Arial,16,&Hffffff,&Hffffff,&H0,&H0,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,1";
        "";
        "[Events]";
        "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, \
         Effect, Text";
        "";
      ]

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

  external create_frame : content -> frame
    = "ocaml_avutil_subtitle_create_frame"

  external get_content : frame -> content = "ocaml_avutil_subtitle_content"
  external get_pts : frame -> int64 option = "ocaml_avutil_subtitle_pts"
end

external rational_of_float : float -> rational
  = "ocaml_avutil_rational_of_float"

module Options = struct
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

  type kind =
    [ `Flags
    | `Int
    | `Int64
    | `UInt64
    | `Duration
    | `Double
    | `Float
    | `Rational
    | `String
    | `Binary
    | `Dict
    | `Image_size
    | `Video_rate
    | `Color
    | `Pixel_fmt
    | `Sample_fmt
    | `Channel_layout
    | `Bool
    | `Const
    | `Unsupported ]

  (* An FFmpeg option as its class declares it: [default_int],
     [default_float] and [default_string] are the members of the default
     value its [kind] uses, a named constant among them. *)
  type raw = {
    raw_name : string;
    raw_help : string option;
    raw_unit : string option;
    kind : kind;
    is_array : bool;
    raw_flags : flag list;
    default_int : int64;
    default_float : float;
    default_string : string option;
    raw_min : float;
    raw_max : float;
  }

  external class_options : t -> raw array = "ocaml_avutil_class_options"
  external child_classes : t -> t array = "ocaml_avutil_child_classes"

  (* FFmpeg stores every bound as a float. *)
  let saturated bound =
    if bound <= Int64.to_float Int64.min_int then Int64.min_int
    else if bound >= Int64.to_float Int64.max_int then Int64.max_int
    else Int64.of_float bound

  let found find key = try Some (find key) with Not_found -> None

  (* The named constants of [raw]: the constants of its class that share
     its unit. *)
  let named_values constants raw convert =
    match raw.raw_unit with
      | None -> []
      | Some _ ->
          List.filter_map
            (fun constant ->
              if constant.raw_unit = raw.raw_unit then
                Some (constant.raw_name, convert constant)
              else None)
            constants

  let ground constants raw : ground option =
    let unbounded default = { default; min = None; max = None; values = [] } in
    let text () = unbounded raw.default_string in
    let format find_id =
      match found find_id (Int64.to_int raw.default_int) with
        | Some `None | None -> None
        | Some format -> Some format
    in
    let int64 () =
      {
        default = Some raw.default_int;
        min = Some (saturated raw.raw_min);
        max = Some (saturated raw.raw_max);
        values = named_values constants raw (fun c -> c.default_int);
      }
    in
    let float () =
      {
        default = Some raw.default_float;
        min = Some raw.raw_min;
        max = Some raw.raw_max;
        values = named_values constants raw (fun c -> c.default_float);
      }
    in
    match raw.kind with
      | `Flags -> Some (`Flags (int64 ()))
      | `Int64 -> Some (`Int64 (int64 ()))
      | `UInt64 -> Some (`UInt64 (int64 ()))
      | `Duration -> Some (`Duration (int64 ()))
      | `Int ->
          Some
            (`Int
               {
                 default = Some (Int64.to_int raw.default_int);
                 min = Some (Int64.to_int (saturated raw.raw_min));
                 max = Some (Int64.to_int (saturated raw.raw_max));
                 values =
                   named_values constants raw (fun c ->
                       Int64.to_int c.default_int);
               })
      | `Double -> Some (`Double (float ()))
      | `Float -> Some (`Float (float ()))
      | `Rational ->
          Some
            (`Rational
               {
                 default = Some (rational_of_float raw.default_float);
                 min = Some (rational_of_float raw.raw_min);
                 max = Some (rational_of_float raw.raw_max);
                 values = [];
               })
      | `String -> Some (`String (text ()))
      | `Binary -> Some (`Binary (text ()))
      | `Dict -> Some (`Dict (text ()))
      | `Image_size -> Some (`Image_size (text ()))
      | `Video_rate -> Some (`Video_rate (text ()))
      | `Color -> Some (`Color (text ()))
      | `Pixel_fmt ->
          Some (`Pixel_fmt (unbounded (format Pixel_format.find_id)))
      | `Sample_fmt ->
          Some (`Sample_fmt (unbounded (format Sample_format.find_id)))
      | `Channel_layout ->
          Some
            (`Channel_layout
               (unbounded
                  (Option.bind raw.default_string (found Channel_layout.find))))
      | `Bool ->
          let default =
            if raw.default_int < 0L then None else Some (raw.default_int <> 0L)
          in
          Some
            (`Bool
               {
                 (unbounded default) with
                 values =
                   named_values constants raw (fun c -> c.default_int <> 0L);
               })
      | `Const | `Unsupported -> None

  let without_default_and_values : ground -> ground =
    let strip entry = { entry with default = None; values = [] } in
    function
    | `Flags e -> `Flags (strip e)
    | `Int e -> `Int (strip e)
    | `Int64 e -> `Int64 (strip e)
    | `Float e -> `Float (strip e)
    | `Double e -> `Double (strip e)
    | `String e -> `String (strip e)
    | `Rational e -> `Rational (strip e)
    | `Binary e -> `Binary (strip e)
    | `Dict e -> `Dict (strip e)
    | `UInt64 e -> `UInt64 (strip e)
    | `Image_size e -> `Image_size (strip e)
    | `Pixel_fmt e -> `Pixel_fmt (strip e)
    | `Sample_fmt e -> `Sample_fmt (strip e)
    | `Video_rate e -> `Video_rate (strip e)
    | `Duration e -> `Duration (strip e)
    | `Color e -> `Color (strip e)
    | `Channel_layout e -> `Channel_layout (strip e)
    | `Bool e -> `Bool (strip e)

  let opt constants raw =
    Option.map
      (fun ground ->
        let spec =
          if raw.is_array then `Array (without_default_and_values ground)
          else (ground :> spec)
        in
        {
          name = raw.raw_name;
          help = raw.raw_help;
          flags = raw.raw_flags;
          spec;
        })
      (ground constants raw)

  let rec opts option_class =
    let raws = Array.to_list (class_options option_class) in
    let constants = List.filter (fun raw -> raw.kind = `Const) raws in
    List.filter_map (opt constants) raws
    @ List.concat_map opts (Array.to_list (child_classes option_class))

  type obj
  type 'a getter = ?search_children:bool -> name:string -> obj -> 'a

  let getter read : _ getter =
   fun ?(search_children = false) ~name obj -> read search_children name obj

  external get_string : bool -> string -> obj -> string
    = "ocaml_avutil_get_option_string"

  external get_int : bool -> string -> obj -> int
    = "ocaml_avutil_get_option_int"

  external get_int64 : bool -> string -> obj -> int64
    = "ocaml_avutil_get_option_int64"

  external get_float : bool -> string -> obj -> float
    = "ocaml_avutil_get_option_float"

  external get_rational : bool -> string -> obj -> rational
    = "ocaml_avutil_get_option_rational"

  external get_image_size : bool -> string -> obj -> int * int
    = "ocaml_avutil_get_option_image_size"

  external get_pixel_fmt : bool -> string -> obj -> Pixel_format.t
    = "ocaml_avutil_get_option_pixel_format"

  external get_sample_fmt : bool -> string -> obj -> Sample_format.t
    = "ocaml_avutil_get_option_sample_format"

  external get_video_rate : bool -> string -> obj -> rational
    = "ocaml_avutil_get_option_video_rate"

  external get_channel_layout : bool -> string -> obj -> Channel_layout.t
    = "ocaml_avutil_get_option_channel_layout"

  external get_dictionary : bool -> string -> obj -> (string * string) list
    = "ocaml_avutil_get_option_dictionary"

  let get_string = getter get_string
  let get_int = getter get_int
  let get_int64 = getter get_int64
  let get_float = getter get_float
  let get_rational = getter get_rational
  let get_image_size = getter get_image_size
  let get_pixel_fmt = getter get_pixel_fmt
  let get_sample_fmt = getter get_sample_fmt
  let get_video_rate = getter get_video_rate
  let get_channel_layout = getter get_channel_layout
  let get_dictionary = getter get_dictionary
end

type value =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]

type opts = (string, value) Hashtbl.t

external render_value : value -> string = "ocaml_avutil_render_option_value"

(* Each key once, with its most recent binding: [Hashtbl.fold] gives the
   bindings of a key most recent first. *)
let bindings opts =
  let seen = Hashtbl.create (Hashtbl.length opts) in
  Hashtbl.fold
    (fun key value bindings ->
      if Hashtbl.mem seen key then bindings
      else (
        Hashtbl.add seen key ();
        (key, value) :: bindings))
    opts []

let string_of_opts opts =
  String.concat ","
    (List.map
       (fun (key, value) -> key ^ "=" ^ render_value value)
       (bindings opts))

let filter_opts unused opts =
  let kept = Hashtbl.create (Array.length unused) in
  Array.iter (fun key -> Hashtbl.replace kept key ()) unused;
  Hashtbl.filter_map_inplace
    (fun key value -> if Hashtbl.mem kept key then Some value else None)
    opts

module HwContext = struct
  type device_type = Hw_device_type.t
  type device_context
  type frame_context

  external create_device_context :
    string -> (string * value) array -> device_type -> device_context
    = "ocaml_avutil_create_device_context"

  let create_device_context ?(device = "") ?opts device_type =
    let options = Option.fold ~none:[] ~some:bindings opts in
    create_device_context device (Array.of_list options) device_type

  external create_frame_context :
    width:int ->
    height:int ->
    src_pixel_format:Pixel_format.t ->
    dst_pixel_format:Pixel_format.t ->
    device_context ->
    frame_context = "ocaml_avutil_create_frame_context"
end
