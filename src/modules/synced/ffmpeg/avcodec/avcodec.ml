open Avutil
module Ba = Bigarray.Array1

external version : unit -> int = "ocaml_avcodec_version" [@@noalloc]

let version =
  let v = version () in
  { major = v lsr 16; minor = (v lsr 8) land 0xff; micro = v land 0xff }

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
  = "ocaml_avcodec_encoder_params"

external time_base : 'media encoder -> Avutil.rational
  = "ocaml_avcodec_encoder_time_base"

external get_name : _ codec -> string = "ocaml_avcodec_get_name"
external get_description : _ codec -> string = "ocaml_avcodec_get_description"
external init : unit -> unit = "ocaml_avcodec_init" [@@noalloc]

let () = init ()

external get_next_codec :
  unit option -> (('a, 'b) codec * 'c * bool * unit option) option
  = "ocaml_avcodec_get_next_codec"

let all_codecs =
  let rec f cur h =
    match get_next_codec h with
      | None -> cur
      | Some (codec, id, is_encoder, h) -> f ((codec, id, is_encoder) :: cur) h
  in
  f [] None

external name : _ codec -> string = "ocaml_avcodec_name"

type capability = Codec_capabilities.t

(* To be used with Audio.t and Video.t *)
external capabilities : ([< `Audio | `Video ], encode) codec -> capability array
  = "ocaml_avcodec_capabilities"

let capabilities c = Array.to_list (capabilities c)

let mk_descriptor d =
  Option.map
    (fun (media_type, name, long_name, properties, mime_types, profiles) ->
      {
        media_type;
        name;
        long_name;
        properties = Array.to_list properties;
        mime_types = Array.to_list mime_types;
        profiles = Array.to_list profiles;
      })
    d

external params_descriptor :
  'media params ->
  (Avutil.media_type
  * string
  * string option
  * Codec_properties.t array
  * string array
  * profile array)
  option = "ocaml_avcodec_params_descriptor"

let descriptor p = mk_descriptor (params_descriptor p)

type hw_config_method = Hw_config_method.t

type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}

external hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list
  = "ocaml_avcodec_hw_methods"

(** Packet. *)
module Packet = struct
  (** Packet type *)
  type 'a t

  external create : string -> 'a t = "ocaml_avcodec_create_packet"
  external content : 'a t -> string = "ocaml_avcodec_packet_content"

  type flag = [ `Keyframe | `Corrupt | `Discard | `Trusted | `Disposable ]

  external int_of_flag : flag -> int = "ocaml_avcodec_int_of_flag"
  external get_flags : 'a t -> int = "ocaml_avcodec_get_flags"

  let get_flags p =
    let flags = get_flags p in
    List.fold_left
      (fun cur flag ->
        if int_of_flag flag land flags <> 0 then flag :: cur else cur)
      []
      [`Keyframe; `Corrupt; `Discard; `Trusted; `Disposable]

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

  type _side_data =
    [ `Replaygain of replaygain
    | `Strings_metadata of string
    | `Metadata_update of string ]

  external add_side_data : 'media t -> _side_data -> unit
    = "ocaml_avcodec_packet_add_side_data"

  let concat_meta m =
    String.concat "\000" (List.map (fun (k, v) -> k ^ "\000" ^ v) m)

  let add_side_data p d =
    let d =
      match d with
        | `Replaygain r -> `Replaygain r
        | `Strings_metadata m -> `Strings_metadata (concat_meta m)
        | `Metadata_update m -> `Metadata_update (concat_meta m)
    in
    add_side_data p d

  external side_data : _ t -> _side_data array
    = "ocaml_avcodec_packet_side_data"

  let split_metadata s =
    let rec split_meta m = function
      | [_] | [] -> m
      | k :: v :: l -> split_meta ((k, v) :: m) l
    in
    split_meta [] (String.split_on_char '\000' s)

  let side_data packet =
    List.map
      (function
        | `Replaygain r -> `Replaygain r
        | `Strings_metadata s -> `Strings_metadata (split_metadata s)
        | `Metadata_update s -> `Metadata_update (split_metadata s))
      (Array.to_list (side_data packet))

  external dup : 'a t -> 'a t = "ocaml_avcodec_packet_dup"
  external get_size : 'a t -> int = "ocaml_avcodec_get_packet_size"

  external get_stream_index : 'a t -> int
    = "ocaml_avcodec_get_packet_stream_index"

  external set_stream_index : 'a t -> int -> unit
    = "ocaml_avcodec_set_packet_stream_index"

  external get_pts : 'a t -> Int64.t option = "ocaml_avcodec_get_packet_pts"

  external set_pts : 'a t -> Int64.t option -> unit
    = "ocaml_avcodec_set_packet_pts"

  external get_dts : 'a t -> Int64.t option = "ocaml_avcodec_get_packet_dts"

  external set_dts : 'a t -> Int64.t option -> unit
    = "ocaml_avcodec_set_packet_dts"

  external get_duration : 'a t -> Int64.t option
    = "ocaml_avcodec_get_packet_duration"

  external set_duration : 'a t -> Int64.t option -> unit
    = "ocaml_avcodec_set_packet_duration"

  external get_position : 'a t -> Int64.t option
    = "ocaml_avcodec_get_packet_position"

  external set_position : 'a t -> Int64.t option -> unit
    = "ocaml_avcodec_set_packet_position"

  external to_bytes : 'a t -> bytes = "ocaml_avcodec_packet_to_bytes"
end

(* These functions receive AVCodecParameters and AVCodec on the C side. *)
external create_decoder : ?params:'a params -> 'b -> 'a decoder
  = "ocaml_avcodec_create_decoder"

(** Audio codecs. *)
module Audio = struct
  (** Audio codec ids *)
  type 'mode t = (audio, 'mode) codec

  type id = Codec_id.audio

  external audio_descriptor :
    id ->
    (Avutil.media_type
    * string
    * string option
    * Codec_properties.t array
    * string array
    * profile array)
    option = "ocaml_avcodec_audio_descriptor"

  let descriptor id = mk_descriptor (audio_descriptor id)
  let codec_ids = Codec_id.audio
  let get_name = get_name
  let get_description = get_description

  let encoders =
    List.filter_map
      (function
        | c, Some id, true when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  let decoders =
    List.filter_map
      (function
        | c, Some id, false when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  external frame_size : audio encoder -> int = "ocaml_avcodec_frame_size"
  external get_id : _ t -> id = "ocaml_avcodec_get_audio_codec_id"
  external string_of_id : id -> string = "ocaml_avcodec_get_audio_codec_id_name"

  external find_encoder_by_name : string -> [ `Encoder ] t
    = "ocaml_avcodec_find_audio_encoder_by_name"

  external find_encoder : id -> [ `Encoder ] t
    = "ocaml_avcodec_find_audio_encoder"

  external find_decoder_by_name : string -> [ `Decoder ] t
    = "ocaml_avcodec_find_audio_decoder_by_name"

  external find_decoder : id -> [ `Decoder ] t
    = "ocaml_avcodec_find_audio_decoder"

  external get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list
    = "ocaml_avcodec_get_supported_channel_layouts"

  let get_supported_channel_layouts codec =
    List.rev (get_supported_channel_layouts codec)

  let find_best_channel_layout codec default =
    try
      let channel_layouts = get_supported_channel_layouts codec in
      if
        List.exists
          (fun layout -> Avutil.Channel_layout.compare layout default)
          channel_layouts
      then default
      else (match channel_layouts with h :: _ -> h | [] -> default)
    with Not_found -> default

  external get_supported_sample_formats : _ t -> Avutil.Sample_format.t list
    = "ocaml_avcodec_get_supported_sample_formats"

  let get_supported_sample_formats codec =
    List.rev (get_supported_sample_formats codec)

  let find_best_sample_format codec default =
    try
      let formats = get_supported_sample_formats codec in
      if List.mem default formats then default
      else (match formats with h :: _ -> h | [] -> default)
    with Not_found -> default

  external get_supported_sample_rates : _ t -> int list
    = "ocaml_avcodec_get_supported_sample_rates"

  let get_supported_sample_rates codec =
    List.rev (get_supported_sample_rates codec)

  let find_best_sample_rate codec default =
    let rates = get_supported_sample_rates codec in
    if List.mem default rates then default
    else (match rates with h :: _ -> h | [] -> default)

  external get_params_id : audio params -> id
    = "ocaml_avcodec_parameters_get_audio_codec_id"

  external get_channel_layout : audio params -> Avutil.Channel_layout.t
    = "ocaml_avcodec_parameters_get_channel_layout"

  external get_nb_channels : audio params -> int
    = "ocaml_avcodec_parameters_get_nb_channels"

  external get_sample_format : audio params -> Avutil.Sample_format.t
    = "ocaml_avcodec_parameters_get_sample_format"

  external get_bit_rate : audio params -> int
    = "ocaml_avcodec_parameters_get_bit_rate"

  external get_sample_rate : audio params -> int
    = "ocaml_avcodec_parameters_get_sample_rate"

  let create_decoder = create_decoder

  external sample_format : audio decoder -> Sample_format.t
    = "ocaml_avcodec_sample_format"

  external create_encoder :
    int ->
    [ `Encoder ] t ->
    Channel_layout.t ->
    (string * string) array ->
    audio encoder * string array = "ocaml_avcodec_create_audio_encoder"

  let create_encoder ?opts ~channel_layout ~sample_rate ~sample_format
      ~time_base codec =
    let opts = opts_default opts in
    let _opts =
      mk_audio_opts ~opts ~channel_layout ~sample_rate ~sample_format ~time_base
        ()
    in
    let encoder, unused =
      create_encoder
        (Sample_format.get_id sample_format)
        codec channel_layout (mk_opts_array _opts)
    in
    filter_opts unused opts;
    encoder
end

(** Video codecs. *)
module Video = struct
  type 'mode t = (video, 'mode) codec
  type id = Codec_id.video

  external video_descriptor :
    id ->
    (Avutil.media_type
    * string
    * string option
    * Codec_properties.t array
    * string array
    * profile array)
    option = "ocaml_avcodec_video_descriptor"

  let descriptor id = mk_descriptor (video_descriptor id)
  let codec_ids = Codec_id.video

  let encoders =
    List.filter_map
      (function
        | c, Some id, true when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  let decoders =
    List.filter_map
      (function
        | c, Some id, false when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  let get_name = get_name
  let get_description = get_description

  external get_id : _ t -> id = "ocaml_avcodec_get_video_codec_id"
  external string_of_id : id -> string = "ocaml_avcodec_get_video_codec_id_name"

  external find_encoder_by_name : string -> [ `Encoder ] t
    = "ocaml_avcodec_find_video_encoder_by_name"

  external find_encoder : id -> [ `Encoder ] t
    = "ocaml_avcodec_find_video_encoder"

  external find_decoder_by_name : string -> [ `Decoder ] t
    = "ocaml_avcodec_find_video_decoder_by_name"

  external find_decoder : id -> [ `Decoder ] t
    = "ocaml_avcodec_find_video_decoder"

  external get_supported_frame_rates : _ t -> Avutil.rational list
    = "ocaml_avcodec_get_supported_frame_rates"

  let get_supported_frame_rates codec =
    List.rev (get_supported_frame_rates codec)

  let find_best_frame_rate codec default =
    let frame_rates = get_supported_frame_rates codec in
    if List.mem default frame_rates then default
    else (match frame_rates with h :: _ -> h | [] -> default)

  external get_supported_color_spaces : _ t -> Avutil.Color_space.t list
    = "ocaml_avcodec_get_supported_color_spaces"

  let get_supported_color_spaces codec =
    List.rev (get_supported_color_spaces codec)

  external get_supported_color_ranges : _ t -> Avutil.Color_range.t list
    = "ocaml_avcodec_get_supported_color_ranges"

  let get_supported_color_ranges codec =
    List.rev (get_supported_color_ranges codec)

  external get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list
    = "ocaml_avcodec_get_supported_pixel_formats"

  let get_supported_pixel_formats codec =
    List.rev (get_supported_pixel_formats codec)

  let find_best_pixel_format ?(hwaccel = false) codec default =
    let formats = get_supported_pixel_formats codec in
    if List.mem default formats then default
    else (
      let formats =
        if hwaccel then formats
        else
          List.filter
            (fun f ->
              not (List.mem `Hwaccel Avutil.Pixel_format.((descriptor f).flags)))
            formats
      in
      match formats with p :: _ -> p | [] -> default)

  external get_params_id : video params -> id
    = "ocaml_avcodec_parameters_get_video_codec_id"

  external get_width : video params -> int
    = "ocaml_avcodec_parameters_get_width"

  external get_height : video params -> int
    = "ocaml_avcodec_parameters_get_height"

  external get_sample_aspect_ratio : video params -> Avutil.rational
    = "ocaml_avcodec_parameters_get_sample_aspect_ratio"

  external get_pixel_format : video params -> Avutil.Pixel_format.t option
    = "ocaml_avcodec_parameters_get_pixel_format"

  external get_pixel_aspect : video params -> Avutil.rational option
    = "ocaml_avcodec_parameters_get_pixel_aspect"

  external get_bit_rate : video params -> int
    = "ocaml_avcodec_parameters_get_bit_rate"

  let create_decoder = create_decoder

  type hardware_context =
    [ `Device_context of HwContext.device_context
    | `Frame_context of HwContext.frame_context ]

  external create_encoder :
    ?device_context:Avutil.HwContext.device_context ->
    ?frame_context:Avutil.HwContext.frame_context ->
    int ->
    [ `Encoder ] t ->
    (string * string) array ->
    video encoder * string array = "ocaml_avcodec_create_video_encoder"

  let create_encoder ?opts ?frame_rate ?hardware_context ~pixel_format ~width
      ~height ~time_base codec =
    let opts = opts_default opts in
    let _opts =
      mk_video_opts ~opts ?frame_rate ~pixel_format ~width ~height ~time_base ()
    in
    let device_context, frame_context =
      match hardware_context with
        | None -> (None, None)
        | Some (`Device_context hardware_context) ->
            (Some hardware_context, None)
        | Some (`Frame_context frame_context) -> (None, Some frame_context)
    in
    let encoder, unused =
      create_encoder ?device_context ?frame_context
        (Pixel_format.get_id pixel_format)
        codec (mk_opts_array _opts)
    in
    filter_opts unused opts;
    encoder
end

(** Subtitle codecs. *)
module Subtitle = struct
  type 'mode t = (subtitle, 'mode) codec
  type id = Codec_id.subtitle

  external subtitle_descriptor :
    id ->
    (Avutil.media_type
    * string
    * string option
    * Codec_properties.t array
    * string array
    * profile array)
    option = "ocaml_avcodec_subtitle_descriptor"

  let descriptor id = mk_descriptor (subtitle_descriptor id)
  let codec_ids = Codec_id.subtitle

  let encoders =
    List.filter_map
      (function
        | c, Some id, true when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  let decoders =
    List.filter_map
      (function
        | c, Some id, false when List.mem id codec_ids -> Some (Obj.magic c)
        | _ -> None)
      all_codecs

  let get_name = get_name
  let get_description = get_description

  external get_id : _ t -> id = "ocaml_avcodec_get_subtitle_codec_id"

  external string_of_id : id -> string
    = "ocaml_avcodec_get_subtitle_codec_id_name"

  external find_encoder_by_name : string -> [ `Encoder ] t
    = "ocaml_avcodec_find_subtitle_encoder_by_name"

  external find_encoder : id -> [ `Encoder ] t
    = "ocaml_avcodec_find_subtitle_encoder"

  external find_decoder_by_name : string -> [ `Decoder ] t
    = "ocaml_avcodec_find_subtitle_decoder_by_name"

  external find_decoder : id -> [ `Decoder ] t
    = "ocaml_avcodec_find_subtitle_decoder"

  external get_params_id : subtitle params -> id
    = "ocaml_avcodec_parameters_get_subtitle_codec_id"
end

module Unknown = struct
  type 'mode t = ([ `Data ], 'mode) codec
  type id = Codec_id.unknown

  let codec_ids = Codec_id.unknown

  external get_params_id : [ `Data ] params -> id
    = "ocaml_avcodec_parameters_get_unknown_codec_id"

  external string_of_id : id -> string
    = "ocaml_avcodec_get_unknown_codec_id_name"
end

external _send_packet : 'media decoder -> 'media Packet.t -> unit
  = "ocaml_avcodec_send_packet"

external _receive_frame : 'media decoder -> 'media frame option
  = "ocaml_avcodec_receive_frame"

external _flush_decoder : 'media decoder -> unit = "ocaml_avcodec_flush_decoder"

let rec receive_frame decoder f =
  match _receive_frame decoder with
    | Some frame ->
        f frame;
        receive_frame decoder f
    | None -> ()

let decode decoder f packet =
  _send_packet decoder packet;
  receive_frame decoder f

let flush_decoder decoder f =
  try
    _flush_decoder decoder;
    receive_frame decoder f
  with Avutil.Error `Eof -> ()

external _send_frame : 'media encoder -> 'media frame -> unit
  = "ocaml_avcodec_send_frame"

external _receive_packet : 'media encoder -> 'media Packet.t option
  = "ocaml_avcodec_receive_packet"

external _flush_encoder : 'media encoder -> unit = "ocaml_avcodec_flush_encoder"

let rec receive_packet encoder f =
  match _receive_packet encoder with
    | Some packet ->
        f packet;
        receive_packet encoder f
    | None -> ()

let encode encoder f frame =
  _send_frame encoder frame;
  receive_packet encoder f

let flush_encoder encoder f =
  (* First flush remaining packets. *)
  receive_packet encoder f;
  _flush_encoder encoder;
  receive_packet encoder f

type id = Codec_id.codec_id

external string_of_id : id -> string = "ocaml_avcodec_get_codec_id_name"

module BitstreamFilter = struct
  type filter = { name : string; codecs : id list; options : Avutil.Options.t }
  type 'a t
  type cursor

  external get_next :
    cursor option -> (string * id array * Avutil.Options.t * cursor) option
    = "ocaml_avcodec_bsf_next"

  let get_next cursor =
    Option.map
      (fun (name, codecs, options, cursor) ->
        ({ name; codecs = Array.to_list codecs; options }, cursor))
      (get_next cursor)

  let filters =
    let rec f cursor filters =
      match get_next cursor with
        | None -> filters
        | Some (filter, cursor) -> f (Some cursor) (filter :: filters)
    in
    f None []

  external init :
    (string * string) array ->
    string ->
    'a params ->
    'a t * 'a params * string array = "ocaml_avcodec_bsf_init"

  let init ?opts { name; _ } params =
    let opts = opts_default opts in
    let filter, params, unused = init (mk_opts_array opts) name params in
    filter_opts unused opts;
    (filter, params)

  external send_packet : 'a t -> 'a Packet.t -> unit
    = "ocaml_avcodec_bsf_send_packet"

  external receive_packet : 'a t -> 'a Packet.t
    = "ocaml_avcodec_bsf_receive_packet"

  external send_eof : 'a t -> unit = "ocaml_avcodec_bsf_send_eof"
end
