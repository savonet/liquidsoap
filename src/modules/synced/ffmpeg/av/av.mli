(** Bindings to libavformat. The interface is the one of spec/avformat.md §4. *)

open Avutil

val avformat_version : version
val container_options : Options.t

module Format : sig
  val get_input_name : (input, _) format -> string
  val get_input_long_name : (input, _) format -> string
  val get_output_name : (output, _) format -> string
  val get_output_long_name : (output, _) format -> string
  val find_input_format : string -> (input, 'a) format option

  val guess_output_format :
    ?short_name:string ->
    ?filename:string ->
    ?mime:string ->
    unit ->
    (output, 'a) format option

  val get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id
  val get_video_codec_id : (output, video) format -> Avcodec.Video.id
  val get_subtitle_codec_id : (output, subtitle) format -> Avcodec.Subtitle.id
end

type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}

val open_input :
  ?interrupt:(unit -> bool) ->
  ?format:(input, _) format ->
  ?opts:opts ->
  ?configure_audio_stream:(audio Avcodec.params -> audio stream_config) ->
  ?configure_video_stream:(video Avcodec.params -> video stream_config) ->
  ?configure_subtitle_stream:(subtitle Avcodec.params -> subtitle stream_config) ->
  string ->
  input container

type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type seek = int -> Unix.seek_command -> int

val open_input_stream :
  ?format:(input, _) format ->
  ?opts:opts ->
  ?seek:seek ->
  read ->
  input container

val get_input_duration :
  ?format:Time_format.t -> input container -> Int64.t option

val get_input_metadata : input container -> (string * string) list
val get_input_format : input container -> (input, _) format option
val input_obj : input container -> Options.obj

type ('line, 'media, 'mode) stream

val get_audio_streams :
  'a container -> (int * ('a, audio, 'b) stream * audio Avcodec.params) list

val get_video_streams :
  'a container -> (int * ('a, video, 'b) stream * video Avcodec.params) list

val get_subtitle_streams :
  'a container ->
  (int * ('a, subtitle, 'b) stream * subtitle Avcodec.params) list

val get_data_streams :
  'a container ->
  (int * ('a, [ `Data ], 'b) stream * [ `Data ] Avcodec.params) list

val find_best_audio_stream :
  input container -> int * (input, audio, 'a) stream * audio Avcodec.params

val find_best_video_stream :
  input container -> int * (input, video, 'a) stream * video Avcodec.params

val find_best_subtitle_stream :
  input container ->
  int * (input, subtitle, 'a) stream * subtitle Avcodec.params

val get_input : (input, _, _) stream -> input container
val get_index : (_, _, _) stream -> int
val get_output : (output, _, _) stream -> output container
val get_codec_params : (_, 'media, _) stream -> 'media Avcodec.params
val get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option
val set_avg_frame_rate : (_, video, _) stream -> Avutil.rational option -> unit
val get_time_base : (_, _, _) stream -> Avutil.rational
val set_time_base : (_, _, _) stream -> Avutil.rational -> unit
val get_frame_size : (output, audio, _) stream -> int
val get_pixel_aspect : (_, video, _) stream -> Avutil.rational option

val get_duration :
  ?format:Time_format.t -> (input, _, _) stream -> Int64.t option

val get_metadata : (input, _, _) stream -> (string * string) list

type packet_result =
  [ `Audio_packet of int * audio Avcodec.Packet.t
  | `Video_packet of int * video Avcodec.Packet.t
  | `Subtitle_packet of int * subtitle Avcodec.Packet.t
  | `Data_packet of int * [ `Data ] Avcodec.Packet.t ]

type frame_result =
  [ `Audio_frame of int * audio frame
  | `Video_frame of int * video frame
  | `Subtitle_frame of int * Avutil.Subtitle.frame ]

type input_result = [ packet_result | frame_result ]

val read_input :
  ?on_unhandled_packet:(packet_result -> unit) ->
  ?audio_packet:(input, audio, [ `Packet ]) stream list ->
  ?audio_frame:(input, audio, [ `Frame ]) stream list ->
  ?video_packet:(input, video, [ `Packet ]) stream list ->
  ?video_frame:(input, video, [ `Frame ]) stream list ->
  ?subtitle_packet:(input, subtitle, [ `Packet ]) stream list ->
  ?subtitle_frame:(input, subtitle, [ `Frame ]) stream list ->
  ?data_packet:(input, [ `Data ], [ `Packet ]) stream list ->
  input container ->
  input_result

type seek_flag =
  | Seek_flag_backward
  | Seek_flag_byte
  | Seek_flag_any
  | Seek_flag_frame

val seek :
  ?flags:seek_flag list ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t ->
  ?max_ts:Int64.t ->
  fmt:Time_format.t ->
  ts:Int64.t ->
  input container ->
  unit

val open_output :
  ?interrupt:(unit -> bool) ->
  ?format:(output, _) format ->
  ?interleaved:bool ->
  ?opts:opts ->
  string ->
  output container

val open_output_format :
  ?interleaved:bool -> ?opts:opts -> (output, _) format -> output container

val open_output_stream :
  ?opts:opts ->
  ?interleaved:bool ->
  ?seek:seek ->
  write ->
  (output, _) format ->
  output container

val output_started : output container -> bool
val set_output_metadata : output container -> (string * string) list -> unit
val set_metadata : (_, _, _) stream -> (string * string) list -> unit

val new_stream_copy :
  params:'mode Avcodec.params ->
  output container ->
  (output, 'mode, [ `Packet ]) stream

type uninitialized_stream_copy

val new_uninitialized_stream_copy :
  output container -> uninitialized_stream_copy

val initialize_stream_copy :
  params:'mode Avcodec.params ->
  uninitialized_stream_copy ->
  (output, 'mode, [ `Packet ]) stream

val new_audio_stream :
  ?opts:opts ->
  channel_layout:Channel_layout.t ->
  sample_rate:int ->
  sample_format:Avutil.Sample_format.t ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Audio.t ->
  output container ->
  (output, audio, [ `Frame ]) stream

val new_video_stream :
  ?opts:opts ->
  ?frame_rate:Avutil.rational ->
  ?hardware_context:Avcodec.Video.hardware_context ->
  pixel_format:Avutil.Pixel_format.t ->
  width:int ->
  height:int ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Video.t ->
  output container ->
  (output, video, [ `Frame ]) stream

val new_subtitle_stream :
  ?opts:opts ->
  ?header:string ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Subtitle.t ->
  output container ->
  (output, subtitle, [ `Frame ]) stream

val new_data_stream :
  time_base:Avutil.rational ->
  codec:Avcodec.Unknown.id ->
  output container ->
  (output, [ `Data ], [ `Packet ]) stream

val codec_attr : _ stream -> string option
val bitrate : _ stream -> int option

val write_packet :
  (output, 'media, [ `Packet ]) stream ->
  Avutil.rational ->
  'media Avcodec.Packet.t ->
  unit

val write_frame :
  ?on_keyframe:(unit -> unit) ->
  (output, 'media, [ `Frame ]) stream ->
  'media frame ->
  unit

val write_subtitle_frame :
  (output, subtitle, [ `Frame ]) stream -> Avutil.Subtitle.frame -> unit

val flush : output container -> unit
val tell : _ container -> int option
val close : _ container -> unit
