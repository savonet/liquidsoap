(** Bindings to libavcodec. The interface is the one of spec/avcodec.md §4. *)

open Avutil

val version : version

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

val flag_qscale : int
val params : 'media encoder -> 'media params
val descriptor : 'media params -> descriptor option
val time_base : 'media encoder -> Avutil.rational
val name : _ codec -> string

type capability = Codec_capabilities.t

val capabilities : ([< `Audio | `Video ], _) codec -> capability list

type hw_config_method = Hw_config_method.t

type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}

val hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list

module Packet : sig
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

  val add_side_data : 'media t -> side_data -> unit
  val side_data : 'media t -> side_data list
  val dup : 'media t -> 'media t
  val get_flags : 'media t -> flag list
  val set_flags : 'media t -> flag list -> unit
  val get_size : 'media t -> int
  val get_stream_index : 'media t -> int
  val set_stream_index : 'media t -> int -> unit
  val get_pts : 'media t -> Int64.t option
  val set_pts : 'media t -> Int64.t option -> unit
  val get_dts : 'media t -> Int64.t option
  val set_dts : 'media t -> Int64.t option -> unit
  val get_duration : 'media t -> Int64.t option
  val set_duration : 'media t -> Int64.t option -> unit
  val get_position : 'media t -> Int64.t option
  val set_position : 'media t -> Int64.t option -> unit
  val to_bytes : 'media t -> bytes
  val content : 'media t -> string
  val create : string -> 'media t
end

module Audio : sig
  type 'mode t = (audio, 'mode) codec
  type id = Codec_id.audio

  val descriptor : id -> descriptor option
  val codec_ids : Codec_id.audio list
  val encoders : encode t list
  val decoders : decode t list
  val find_encoder_by_name : string -> encode t
  val find_encoder : id -> encode t
  val find_decoder_by_name : string -> decode t
  val find_decoder : id -> decode t
  val get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list
  val get_supported_sample_formats : _ t -> Avutil.Sample_format.t list
  val get_supported_sample_rates : _ t -> int list

  val find_best_channel_layout :
    _ t -> Avutil.Channel_layout.t -> Avutil.Channel_layout.t

  val find_best_sample_format :
    _ t -> Avutil.Sample_format.t -> Avutil.Sample_format.t

  val find_best_sample_rate : _ t -> int -> int
  val create_decoder : ?params:audio params -> decode t -> audio decoder
  val sample_format : audio decoder -> Sample_format.t

  val create_encoder :
    ?opts:opts ->
    channel_layout:Channel_layout.t ->
    sample_rate:int ->
    sample_format:Avutil.Sample_format.t ->
    time_base:Avutil.rational ->
    encode t ->
    audio encoder

  val frame_size : audio encoder -> int
  val get_name : _ codec -> string
  val get_description : _ codec -> string
  val string_of_id : id -> string
  val get_id : _ t -> id
  val get_params_id : audio params -> id
  val get_channel_layout : audio params -> Avutil.Channel_layout.t
  val get_nb_channels : audio params -> int
  val get_sample_format : audio params -> Avutil.Sample_format.t
  val get_bit_rate : audio params -> int
  val get_sample_rate : audio params -> int
end

module Video : sig
  type 'mode t = (video, 'mode) codec
  type id = Codec_id.video

  val descriptor : id -> descriptor option
  val codec_ids : Codec_id.video list
  val encoders : encode t list
  val decoders : decode t list
  val find_encoder_by_name : string -> encode t
  val find_encoder : id -> encode t
  val find_decoder_by_name : string -> decode t
  val find_decoder : id -> decode t
  val get_supported_frame_rates : _ t -> Avutil.rational list
  val get_supported_color_spaces : _ t -> Avutil.Color_space.t list
  val get_supported_color_ranges : _ t -> Avutil.Color_range.t list
  val get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list
  val find_best_frame_rate : _ t -> Avutil.rational -> Avutil.rational

  val find_best_pixel_format :
    ?hwaccel:bool -> _ t -> Avutil.Pixel_format.t -> Avutil.Pixel_format.t

  val create_decoder : ?params:video params -> decode t -> video decoder

  type hardware_context =
    [ `Device_context of HwContext.device_context
    | `Frame_context of HwContext.frame_context ]

  val create_encoder :
    ?opts:opts ->
    ?frame_rate:Avutil.rational ->
    ?hardware_context:hardware_context ->
    pixel_format:Avutil.Pixel_format.t ->
    width:int ->
    height:int ->
    time_base:Avutil.rational ->
    encode t ->
    video encoder

  val get_name : _ codec -> string
  val get_description : _ codec -> string
  val string_of_id : id -> string
  val get_id : _ t -> id
  val get_params_id : video params -> id
  val get_width : video params -> int
  val get_height : video params -> int
  val get_sample_aspect_ratio : video params -> Avutil.rational
  val get_pixel_format : video params -> Avutil.Pixel_format.t option
  val get_pixel_aspect : video params -> Avutil.rational option
  val get_bit_rate : video params -> int
end

module Subtitle : sig
  type 'mode t = (subtitle, 'mode) codec
  type id = Codec_id.subtitle

  val descriptor : id -> descriptor option
  val codec_ids : Codec_id.subtitle list
  val encoders : encode t list
  val decoders : decode t list
  val find_encoder_by_name : string -> encode t
  val find_encoder : id -> encode t
  val find_decoder_by_name : string -> decode t
  val find_decoder : id -> decode t
  val get_name : _ codec -> string
  val get_description : _ codec -> string
  val string_of_id : id -> string
  val get_id : _ t -> id
  val get_params_id : subtitle params -> id
end

module Unknown : sig
  type 'mode t = ([ `Data ], 'mode) codec
  type id = Codec_id.unknown

  val codec_ids : Codec_id.unknown list
  val string_of_id : id -> string
  val get_params_id : [ `Data ] params -> id
end

type id = Codec_id.codec_id

val string_of_id : id -> string

module BitstreamFilter : sig
  type filter = private {
    name : string;
    codecs : id list;
    options : Avutil.Options.t;
  }

  type 'a t

  val filters : filter list
  val init : ?opts:opts -> filter -> 'a params -> 'a t * 'a params
  val send_packet : 'a t -> 'a Packet.t -> unit
  val send_eof : 'a t -> unit
  val receive_packet : 'a t -> 'a Packet.t
end

val decode : 'media decoder -> ('media frame -> unit) -> 'media Packet.t -> unit
val flush_decoder : 'media decoder -> ('media frame -> unit) -> unit
val encode : 'media encoder -> ('media Packet.t -> unit) -> 'media frame -> unit
val flush_encoder : 'media encoder -> ('media Packet.t -> unit) -> unit
