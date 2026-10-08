open Avutil

type version = Avutil.version

external version : unit -> version = "ocaml_avcodec_version"

let version = version ()

type ('media, 'mode) codec
type 'media params
type 'media decoder
type 'media encoder
type encode = [ `Encoder ]
type decode = [ `Decoder ]
type profile = { id : int; profile_name : string }

type descriptor = {
  media_type : Avutil.media_type;
  name : string;
  long_name : string option;
  properties : Codec_properties.t list;
  mime_types : string list;
  profiles : profile list;
}

external flag_qscale : unit -> int = "ocaml_avcodec_flag_qscale"

let flag_qscale = flag_qscale ()

external params : 'media encoder -> 'media params
  = "ocaml_avcodec_encoder_parameters"

external descriptor : 'media params -> descriptor option
  = "ocaml_avcodec_parameters_descriptor"

external time_base : 'media encoder -> Avutil.rational
  = "ocaml_avcodec_encoder_time_base"

external name : _ codec -> string = "ocaml_avcodec_codec_name"
external description : _ codec -> string = "ocaml_avcodec_codec_description"

type capability = Codec_capabilities.t

external capabilities : ([< `Audio | `Video ], _) codec -> capability list
  = "ocaml_avcodec_capabilities"

type hw_config_method = Hw_config_method.t

type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}

external hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list
  = "ocaml_avcodec_hw_configs"

(* The option bindings of a caller's table, least recent first: the C side
   lets a later binding of a key replace an earlier one. *)
let bindings = function
  | None -> [||]
  | Some opts ->
      Array.of_list (Hashtbl.fold (fun key v l -> (key, v) :: l) opts [])

let report_unused opts unused = Option.iter (filter_opts unused) opts

module Packet = struct
  type 'media t
  type flag = [ `Keyframe | `Corrupt | `Discard | `Trusted | `Disposable ]

  type replaygain = {
    track_gain : int;
    track_peak : int;
    album_gain : int;
    album_peak : int;
  }

  type side_data =
    [ `Replaygain of replaygain
    | `Strings_metadata of (string * string) list
    | `Metadata_update of (string * string) list ]

  external add_side_data : 'media t -> side_data -> unit
    = "ocaml_avcodec_packet_add_side_data"

  external side_data : 'media t -> side_data list
    = "ocaml_avcodec_packet_side_data"

  external dup : 'media t -> 'media t = "ocaml_avcodec_packet_dup"
  external get_flags : 'media t -> flag list = "ocaml_avcodec_packet_flags"

  external set_flags : 'media t -> flag list -> unit
    = "ocaml_avcodec_packet_set_flags"

  external get_size : 'media t -> int = "ocaml_avcodec_packet_size"

  external get_stream_index : 'media t -> int
    = "ocaml_avcodec_packet_stream_index"

  external set_stream_index : 'media t -> int -> unit
    = "ocaml_avcodec_packet_set_stream_index"

  external get_pts : 'media t -> Int64.t option = "ocaml_avcodec_packet_pts"

  external set_pts : 'media t -> Int64.t option -> unit
    = "ocaml_avcodec_packet_set_pts"

  external get_dts : 'media t -> Int64.t option = "ocaml_avcodec_packet_dts"

  external set_dts : 'media t -> Int64.t option -> unit
    = "ocaml_avcodec_packet_set_dts"

  external get_duration : 'media t -> Int64.t option
    = "ocaml_avcodec_packet_duration"

  external set_duration : 'media t -> Int64.t option -> unit
    = "ocaml_avcodec_packet_set_duration"

  external get_position : 'media t -> Int64.t option
    = "ocaml_avcodec_packet_pos"

  external set_position : 'media t -> Int64.t option -> unit
    = "ocaml_avcodec_packet_set_pos"

  external content : 'media t -> string = "ocaml_avcodec_packet_content"

  let to_bytes packet = Bytes.unsafe_of_string (content packet)

  external create : string -> 'media t = "ocaml_avcodec_packet_create"
end

(* An identifier family and the media type of its codecs; the order is the
   one of the C side. *)
type family =
  | Audio_family
  | Video_family
  | Subtitle_family
  | Unknown_family
  | All_family

external codecs : encoder:bool -> family -> ('media, 'mode) codec list
  = "ocaml_avcodec_codecs"

external find_by_name :
  encoder:bool -> family -> string -> ('media, 'mode) codec
  = "ocaml_avcodec_find_by_name"

external find_by_id : family -> encoder:bool -> 'id -> ('media, 'mode) codec
  = "ocaml_avcodec_find_by_id"

external codec_id : family -> _ codec -> 'id = "ocaml_avcodec_codec_id"
external id_name : family -> 'id -> string = "ocaml_avcodec_string_of_id"

external id_descriptor : family -> 'id -> descriptor option
  = "ocaml_avcodec_descriptor"

external params_id : family -> _ params -> 'id = "ocaml_avcodec_parameters_id"
external params_bit_rate : _ params -> int = "ocaml_avcodec_parameters_bit_rate"

external create_decoder :
  'media params option -> ('media, decode) codec -> 'media decoder
  = "ocaml_avcodec_create_decoder"

(* spec/avcodec.md §11, best-value selection. *)
let find_best ~equal supported default =
  match supported with
    | [] -> default
    | first :: _ ->
        if List.exists (equal default) supported then default else first

module Audio = struct
  type 'mode t = (audio, 'mode) codec
  type id = Codec_id.audio

  let family = Audio_family
  let descriptor (id : id) = id_descriptor family id
  let codec_ids = Codec_id.audio
  let encoders : encode t list = codecs ~encoder:true family
  let decoders : decode t list = codecs ~encoder:false family

  let find_encoder_by_name name : encode t =
    find_by_name ~encoder:true family name

  let find_encoder (id : id) : encode t = find_by_id family ~encoder:true id

  let find_decoder_by_name name : decode t =
    find_by_name ~encoder:false family name

  let find_decoder (id : id) : decode t = find_by_id family ~encoder:false id

  external get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list
    = "ocaml_avcodec_supported_channel_layouts"

  external get_supported_sample_formats : _ t -> Avutil.Sample_format.t list
    = "ocaml_avcodec_supported_sample_formats"

  external get_supported_sample_rates : _ t -> int list
    = "ocaml_avcodec_supported_sample_rates"

  let find_best_channel_layout codec default =
    find_best ~equal:Channel_layout.compare
      (get_supported_channel_layouts codec)
      default

  let find_best_sample_format codec default =
    find_best ~equal:( = ) (get_supported_sample_formats codec) default

  let find_best_sample_rate codec default =
    find_best ~equal:Int.equal (get_supported_sample_rates codec) default

  let create_decoder ?params (codec : decode t) : audio decoder =
    create_decoder params codec

  external sample_format : audio decoder -> Sample_format.t
    = "ocaml_avcodec_decoder_sample_format"

  external create_encoder :
    (string * value) array ->
    Channel_layout.t ->
    int ->
    Sample_format.t ->
    rational ->
    encode t ->
    audio encoder * string array
    = "ocaml_avcodec_create_audio_encoder_bytecode"
      "ocaml_avcodec_create_audio_encoder"

  let create_encoder ?opts ~channel_layout ~sample_rate ~sample_format
      ~time_base codec =
    let encoder, unused =
      create_encoder (bindings opts) channel_layout sample_rate sample_format
        time_base codec
    in
    report_unused opts unused;
    encoder

  external frame_size : audio encoder -> int
    = "ocaml_avcodec_encoder_frame_size"

  let get_name = name
  let get_description = description
  let string_of_id (id : id) = id_name family id
  let get_id (codec : _ t) : id = codec_id family codec
  let get_params_id (params : audio params) : id = params_id family params

  external get_channel_layout : audio params -> Avutil.Channel_layout.t
    = "ocaml_avcodec_parameters_channel_layout"

  external get_nb_channels : audio params -> int
    = "ocaml_avcodec_parameters_nb_channels"

  external get_sample_format : audio params -> Avutil.Sample_format.t
    = "ocaml_avcodec_parameters_sample_format"

  let get_bit_rate (params : audio params) = params_bit_rate params

  external get_sample_rate : audio params -> int
    = "ocaml_avcodec_parameters_sample_rate"
end

module Video = struct
  type 'mode t = (video, 'mode) codec
  type id = Codec_id.video

  let family = Video_family
  let descriptor (id : id) = id_descriptor family id
  let codec_ids = Codec_id.video
  let encoders : encode t list = codecs ~encoder:true family
  let decoders : decode t list = codecs ~encoder:false family

  let find_encoder_by_name name : encode t =
    find_by_name ~encoder:true family name

  let find_encoder (id : id) : encode t = find_by_id family ~encoder:true id

  let find_decoder_by_name name : decode t =
    find_by_name ~encoder:false family name

  let find_decoder (id : id) : decode t = find_by_id family ~encoder:false id

  external get_supported_frame_rates : _ t -> Avutil.rational list
    = "ocaml_avcodec_supported_frame_rates"

  external get_supported_color_spaces : _ t -> Avutil.Color_space.t list
    = "ocaml_avcodec_supported_color_spaces"

  external get_supported_color_ranges : _ t -> Avutil.Color_range.t list
    = "ocaml_avcodec_supported_color_ranges"

  external get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list
    = "ocaml_avcodec_supported_pixel_formats"

  let find_best_frame_rate codec default =
    let equal a b = a.num * b.den = b.num * a.den in
    find_best ~equal (get_supported_frame_rates codec) default

  let is_hardware format =
    match Pixel_format.descriptor format with
      | descriptor -> List.mem `Hwaccel descriptor.flags
      | exception Not_found -> false

  let find_best_pixel_format ?(hwaccel = false) codec default =
    let supported = get_supported_pixel_formats codec in
    if supported = [] || List.mem default supported then default
    else (
      let usable format = hwaccel || not (is_hardware format) in
      Option.value (List.find_opt usable supported) ~default)

  let create_decoder ?params (codec : decode t) : video decoder =
    create_decoder params codec

  type hardware_context =
    [ `Device_context of HwContext.device_context
    | `Frame_context of HwContext.frame_context ]

  external create_encoder :
    (string * value) array ->
    rational option ->
    hardware_context option ->
    Pixel_format.t ->
    int ->
    int ->
    rational ->
    encode t ->
    video encoder * string array
    = "ocaml_avcodec_create_video_encoder_bytecode"
      "ocaml_avcodec_create_video_encoder"

  let create_encoder ?opts ?frame_rate ?hardware_context ~pixel_format ~width
      ~height ~time_base codec =
    let encoder, unused =
      create_encoder (bindings opts) frame_rate hardware_context pixel_format
        width height time_base codec
    in
    report_unused opts unused;
    encoder

  let get_name = name
  let get_description = description
  let string_of_id (id : id) = id_name family id
  let get_id (codec : _ t) : id = codec_id family codec
  let get_params_id (params : video params) : id = params_id family params

  external get_width : video params -> int = "ocaml_avcodec_parameters_width"
  external get_height : video params -> int = "ocaml_avcodec_parameters_height"

  external get_sample_aspect_ratio : video params -> Avutil.rational
    = "ocaml_avcodec_parameters_sample_aspect_ratio"

  external get_pixel_format : video params -> Avutil.Pixel_format.t option
    = "ocaml_avcodec_parameters_pixel_format"

  let get_pixel_aspect params =
    match get_sample_aspect_ratio params with
      | { num = 0; _ } -> None
      | ratio -> Some ratio

  let get_bit_rate (params : video params) = params_bit_rate params
end

module Subtitle = struct
  type 'mode t = (subtitle, 'mode) codec
  type id = Codec_id.subtitle

  let family = Subtitle_family
  let descriptor (id : id) = id_descriptor family id
  let codec_ids = Codec_id.subtitle
  let encoders : encode t list = codecs ~encoder:true family
  let decoders : decode t list = codecs ~encoder:false family

  let find_encoder_by_name name : encode t =
    find_by_name ~encoder:true family name

  let find_encoder (id : id) : encode t = find_by_id family ~encoder:true id

  let find_decoder_by_name name : decode t =
    find_by_name ~encoder:false family name

  let find_decoder (id : id) : decode t = find_by_id family ~encoder:false id
  let get_name = name
  let get_description = description
  let string_of_id (id : id) = id_name family id
  let get_id (codec : _ t) : id = codec_id family codec
  let get_params_id (params : subtitle params) : id = params_id family params
end

module Unknown = struct
  type 'mode t = ([ `Data ], 'mode) codec
  type id = Codec_id.unknown

  let codec_ids = Codec_id.unknown
  let string_of_id (id : id) = id_name Unknown_family id

  let get_params_id (params : [ `Data ] params) : id =
    params_id Unknown_family params
end

type id = Codec_id.codec_id

let string_of_id (id : id) = id_name All_family id

module BitstreamFilter = struct
  type filter = { name : string; codecs : id list; options : Avutil.Options.t }
  type 'a t

  external filters : unit -> filter list = "ocaml_avcodec_bitstream_filters"

  let filters = filters ()

  external init :
    (string * value) array ->
    string ->
    'a params ->
    'a t * 'a params * string array = "ocaml_avcodec_bitstream_filter_init"

  let init ?opts filter params =
    let instance, output, unused = init (bindings opts) filter.name params in
    report_unused opts unused;
    (instance, output)

  external send : 'a t -> 'a Packet.t option -> unit
    = "ocaml_avcodec_bitstream_filter_send"

  let send_packet filter packet = send filter (Some packet)
  let send_eof filter = send filter None

  external receive_packet : 'a t -> 'a Packet.t
    = "ocaml_avcodec_bitstream_filter_receive"
end

(* The decode and encode loops of spec/avcodec.md §11, over the send and
   receive steps of the stubs. A send answers [false] when the codec wants
   its output read first, a receive [None] when nothing is ready. The user
   function runs between steps, with the codec free. *)

type state = Open | Draining | Drained

external state : _ -> state = "ocaml_avcodec_codec_state"

external send_packet : 'media decoder -> 'media Packet.t option -> bool
  = "ocaml_avcodec_send_packet"

external receive_frame : 'media decoder -> 'media frame option
  = "ocaml_avcodec_receive_frame"

external send_frame : 'media encoder -> 'media frame option -> bool
  = "ocaml_avcodec_send_frame"

external receive_packet : 'media encoder -> 'media Packet.t option
  = "ocaml_avcodec_receive_packet"

let rec deliver ~receive codec f =
  match receive codec with
    | None -> ()
    | Some output ->
        f output;
        deliver ~receive codec f

let rec send_after_delivery ~send ~receive codec f input =
  deliver ~receive codec f;
  if not (send codec input) then
    send_after_delivery ~send ~receive codec f input

let process ~send ~receive codec f input =
  if state codec <> Open then raise (Error `Eof);
  send_after_delivery ~send ~receive codec f (Some input);
  deliver ~receive codec f

let flush ~send ~receive codec f =
  if state codec = Open then send_after_delivery ~send ~receive codec f None;
  if state codec = Draining then deliver ~receive codec f

let decode decoder f packet =
  process ~send:send_packet ~receive:receive_frame decoder f packet

let flush_decoder decoder f =
  flush ~send:send_packet ~receive:receive_frame decoder f

let encode encoder f frame =
  process ~send:send_frame ~receive:receive_packet encoder f frame

let flush_encoder encoder f =
  flush ~send:send_frame ~receive:receive_packet encoder f
