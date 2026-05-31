open Avutil

external init : unit -> unit = "ocaml_av_init" [@@noalloc]

let () = init ()

external avformat_version : unit -> int = "ocaml_avformat_version" [@@noalloc]

let avformat_version =
  let v = avformat_version () in
  Avutil.{ major = v lsr 16; minor = (v lsr 8) land 0xff; micro = v land 0xff }

external container_options : unit -> Options.t = "ocaml_av_container_options"

let container_options = container_options ()

(* Format *)
module Format = struct
  external get_input_name : (input, _) format -> string
    = "ocaml_av_input_format_get_name"

  external get_input_long_name : (input, _) format -> string
    = "ocaml_av_input_format_get_long_name"

  external find_input_format : string -> (input, 'a) format
    = "ocaml_av_find_input_format"

  let find_input_format name =
    try Some (find_input_format name) with Not_found -> None

  external get_output_name : (output, _) format -> string
    = "ocaml_av_output_format_get_name"

  external get_output_long_name : (output, _) format -> string
    = "ocaml_av_output_format_get_long_name"

  external get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id
    = "ocaml_av_output_format_get_audio_codec_id"

  external get_video_codec_id : (output, video) format -> Avcodec.Video.id
    = "ocaml_av_output_format_get_video_codec_id"

  external get_subtitle_codec_id :
    (output, subtitle) format -> Avcodec.Subtitle.id
    = "ocaml_av_output_format_get_subtitle_codec_id"

  external guess_output_format :
    string -> string -> string -> (output, 'a) format option
    = "ocaml_av_output_format_guess"

  let guess_output_format ?(short_name = "") ?(filename = "") ?(mime = "") () =
    guess_output_format short_name filename mime
end

external ocaml_av_cleanup_av : _ container -> unit = "ocaml_av_cleanup_av"

type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}

(* Input *)
external open_input :
  string ->
  (input, _) format option ->
  (unit -> bool) option ->
  (string * string) array ->
  (audio Avcodec.params ->
  (audio, Avcodec.decode) Avcodec.codec option * (string * string) array)
  option ->
  (video Avcodec.params ->
  (video, Avcodec.decode) Avcodec.codec option * (string * string) array)
  option ->
  (subtitle Avcodec.params ->
  (subtitle, Avcodec.decode) Avcodec.codec option * (string * string) array)
  option ->
  input container * string array
  = "ocaml_av_open_input_bytecode" "ocaml_av_open_input"

let wrap_configure_stream fn params =
  match fn params with
    | { codec; opts } -> (codec, mk_opts_array (opts_default opts))

let open_input ?interrupt ?format ?opts ?configure_audio_stream
    ?configure_video_stream ?configure_subtitle_stream url =
  let opts = opts_default opts in
  let ret, unused =
    open_input url format interrupt (mk_opts_array opts)
      (Option.map wrap_configure_stream configure_audio_stream)
      (Option.map wrap_configure_stream configure_video_stream)
      (Option.map wrap_configure_stream configure_subtitle_stream)
  in
  Gc.finalise ocaml_av_cleanup_av ret;
  filter_opts unused opts;
  ret

type avio
type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type _seek = int -> int -> int
type seek = int -> Unix.seek_command -> int

let seek_of_int = function
  | 0 -> Unix.SEEK_SET
  | 1 -> Unix.SEEK_CUR
  | 2 -> Unix.SEEK_END
  | _ -> assert false

external ocaml_av_create_io :
  read option -> write option -> _seek option -> avio = "ocaml_av_create_io"

external caml_av_io_close : avio -> unit = "caml_av_io_close"

let ocaml_av_create_io read write seek =
  let avio = ocaml_av_create_io read write seek in
  Gc.finalise caml_av_io_close avio;
  avio

let _seek_of_seek = function
  | None -> None
  | Some fn -> Some (fun a m -> fn a (seek_of_int m))

let ocaml_av_create_read_io ?seek read =
  ocaml_av_create_io (Some read) None (_seek_of_seek seek)

external ocaml_av_open_input_stream :
  avio ->
  (input, _) format option ->
  (string * string) array ->
  input container * string array = "ocaml_av_open_input_stream"

let ocaml_av_open_input_stream ?format ?opts avio =
  let opts = opts_default opts in
  let ret, unused =
    ocaml_av_open_input_stream avio format (mk_opts_array opts)
  in
  Gc.finalise ocaml_av_cleanup_av ret;
  filter_opts unused opts;
  ret

let open_input_stream ?format ?opts ?seek read =
  let avio = ocaml_av_create_read_io ?seek read in
  let input = ocaml_av_open_input_stream ?format ?opts avio in
  input

external _get_duration :
  input container -> int -> Time_format.t -> Int64.t option
  = "ocaml_av_get_duration"

let get_input_duration ?(format = `Second) i =
  match _get_duration i (-1) format with Some 0L -> None | v -> v

external _get_metadata : input container -> int -> (string * string) list
  = "ocaml_av_get_metadata"

let get_input_metadata i = List.rev (_get_metadata i (-1))

external get_input_format : input container -> (input, _) format option
  = "ocaml_av_get_input_format"

external input_obj : input container -> 'a = "ocaml_av_input_obj"

let input_obj c = Obj.magic (input_obj c, c)

(* Input Stream *)
type ('a, 'b, 'c) stream = { container : 'a container; index : int }
type media_type = MT_audio | MT_video | MT_data | MT_subtitle

let mk_stream container index = { container; index }

external get_codec_params : (_, 'm, _) stream -> 'm Avcodec.params
  = "ocaml_av_get_stream_codec_parameters"

external get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option
  = "ocaml_av_get_stream_avg_frame_rate"

external set_avg_frame_rate :
  (_, video, _) stream -> Avutil.rational option -> unit
  = "ocaml_av_set_stream_avg_frame_rate"

external get_container_stream_time_base :
  index:int -> _ container -> Avutil.rational
  = "ocaml_av_get_container_stream_time_base"

external get_time_base : (_, _, _) stream -> Avutil.rational
  = "ocaml_av_get_stream_time_base"

external set_time_base : (_, _, _) stream -> Avutil.rational -> unit
  = "ocaml_av_set_stream_time_base"

external get_frame_size : (_, audio, _) stream -> int
  = "ocaml_av_get_stream_frame_size"

external get_pixel_aspect : (_, video, _) stream -> Avutil.rational option
  = "ocaml_av_get_stream_pixel_aspect"

external _get_streams : _ container -> media_type -> int list
  = "ocaml_av_get_streams"

let get_streams container media_type =
  _get_streams container media_type
  |> List.rev_map (fun i ->
      let s = mk_stream container i in
      (i, s, get_codec_params s))

let get_audio_streams container = get_streams container MT_audio
let get_video_streams container = get_streams container MT_video
let get_subtitle_streams container = get_streams container MT_subtitle
let get_data_streams container = get_streams container MT_data

external _find_best_stream : input container -> media_type -> int
  = "ocaml_av_find_best_stream"

let find_best_stream c t =
  let i = _find_best_stream c t in
  let s = mk_stream c i in
  (i, s, get_codec_params s)

let find_best_audio_stream c = find_best_stream c MT_audio
let find_best_video_stream c = find_best_stream c MT_video
let find_best_subtitle_stream c = find_best_stream c MT_subtitle
let get_input s = s.container
let get_index s = s.index

let get_duration ?(format = `Second) s =
  _get_duration s.container s.index format

let get_metadata s = List.rev (_get_metadata s.container s.index)

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

(** Reads the selected streams if any or all streams otherwise. *)
external read_input :
  (packet_result -> unit) option ->
  (int * Avutil.media_type) array ->
  int array ->
  input container ->
  input_result = "ocaml_av_read_input"

let _get_packet media_type input =
  List.map (fun { index; container } ->
      if container != input then
        raise (Failure "Inconsistent stream and input!");
      (index, media_type))

let _get_frame input =
  List.map (fun { index; container } ->
      if container != input then
        raise (Failure "Inconsistent stream and input!");
      index)

let read_input ?on_unhandled_packet ?(audio_packet = []) ?(audio_frame = [])
    ?(video_packet = []) ?(video_frame = []) ?(subtitle_packet = [])
    ?(subtitle_frame = []) ?(data_packet = []) input =
  let packet =
    Array.of_list
      (_get_packet `Audio input audio_packet
      @ _get_packet `Video input video_packet
      @ _get_packet `Subtitle input subtitle_packet
      @ _get_packet `Data input data_packet)
  in
  let frame =
    Array.of_list
      (_get_frame input audio_frame
      @ _get_frame input video_frame
      @ _get_frame input subtitle_frame)
  in
  read_input on_unhandled_packet packet frame input

type seek_flag =
  | Seek_flag_backward
  | Seek_flag_byte
  | Seek_flag_any
  | Seek_flag_frame

external seek :
  flags:seek_flag array ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t ->
  ?max_ts:Int64.t ->
  fmt:Time_format.t ->
  ts:Int64.t ->
  input container ->
  unit = "ocaml_av_seek_bytecode" "ocaml_av_seek_native"

let seek ?(flags = []) = seek ~flags:(Array.of_list flags)

(* Output *)
external open_output :
  ?interrupt:(unit -> bool) ->
  ?format:(output, _) format ->
  string ->
  bool ->
  (string * string) array ->
  output container * string array = "ocaml_av_open_output"

let open_output ?interrupt ?format ?(interleaved = true) ?opts fname =
  let opts = opts_default opts in
  let ret, unused =
    open_output ?interrupt ?format fname interleaved (mk_opts_array opts)
  in
  filter_opts unused opts;
  Gc.finalise ocaml_av_cleanup_av ret;
  ret

external ocaml_av_open_output_stream :
  (output, _) format ->
  avio ->
  bool ->
  (string * string) array ->
  output container * string array = "ocaml_av_open_output_stream"

let open_output_stream ?opts ?(interleaved = true) ?seek write format =
  let opts = opts_default opts in
  let avio = ocaml_av_create_io None (Some write) (_seek_of_seek seek) in
  let output, unused =
    ocaml_av_open_output_stream format avio interleaved (mk_opts_array opts)
  in
  Gc.finalise ocaml_av_cleanup_av output;
  filter_opts unused opts;
  output

external reopen_output_stream : output container -> unit
  = "ocaml_av_reopen_output_stream"

external output_started : output container -> bool = "ocaml_av_header_written"

external _set_metadata : _ container -> int -> (string * string) array -> unit
  = "ocaml_av_set_metadata"

let set_output_metadata o tags = _set_metadata o (-1) (Array.of_list tags)
let set_input_metadata o tags = _set_metadata o (-1) (Array.of_list tags)
let set_metadata s tags = _set_metadata s.container s.index (Array.of_list tags)
let get_output s = s.container

type uninitialized_stream_copy = output container * int

external new_uninitialized_stream_copy : output container -> int
  = "ocaml_av_new_uninitialized_stream_copy"

let new_uninitialized_stream_copy container =
  (container, new_uninitialized_stream_copy container)

external initialize_stream_copy :
  output container -> int -> _ Avcodec.params -> unit
  = "ocaml_av_initialize_stream_copy"

let initialize_stream_copy ~params (container, index) =
  initialize_stream_copy container index params;
  mk_stream container index

let new_stream_copy ~params container =
  initialize_stream_copy ~params (new_uninitialized_stream_copy container)

external new_audio_stream :
  _ container ->
  int ->
  [ `Encoder ] Avcodec.Audio.t ->
  Channel_layout.t ->
  (string * string) array ->
  int * string array = "ocaml_av_new_audio_stream"

let new_audio_stream ?opts ~channel_layout ~sample_rate ~sample_format
    ~time_base ~codec container =
  let opts =
    mk_audio_opts ?opts ~channel_layout ~sample_rate ~sample_format ~time_base
      ()
  in
  let ret, unused =
    new_audio_stream container
      (Sample_format.get_id sample_format)
      codec channel_layout (mk_opts_array opts)
  in
  filter_opts unused opts;
  mk_stream container ret

external new_video_stream :
  ?device_context:Avutil.HwContext.device_context ->
  ?frame_context:Avutil.HwContext.frame_context ->
  _ container ->
  [ `Encoder ] Avcodec.Video.t ->
  (string * string) array ->
  int * string array = "ocaml_av_new_video_stream"

let new_video_stream ?opts ?frame_rate ?hardware_context ~pixel_format ~width
    ~height ~time_base ~codec container =
  let opts =
    mk_video_opts ?opts ?frame_rate ~pixel_format ~width ~height ~time_base ()
  in
  let device_context, frame_context =
    match hardware_context with
      | None -> (None, None)
      | Some (`Device_context hardware_context) -> (Some hardware_context, None)
      | Some (`Frame_context frame_context) -> (None, Some frame_context)
  in
  let ret, unused =
    new_video_stream ?device_context ?frame_context container codec
      (mk_opts_array opts)
  in
  filter_opts unused opts;
  let s = mk_stream container ret in
  set_avg_frame_rate s frame_rate;
  s

external new_subtitle_stream :
  _ container ->
  [ `Encoder ] Avcodec.Subtitle.t ->
  Avutil.rational ->
  string option ->
  (string * string) array ->
  int * string array = "ocaml_av_new_subtitle_stream"

let new_subtitle_stream ?opts ?header ~time_base ~codec container =
  let header =
    match header with
      | Some _ -> header
      | None ->
          let id = Avcodec.Subtitle.get_id codec in
          let has_text_sub =
            match Avcodec.Subtitle.descriptor id with
              | Some d -> List.mem `Text_sub d.Avcodec.properties
              | None -> false
          in
          if has_text_sub then Some (Avutil.Subtitle.header_ass_default ())
          else None
  in
  let opts = opts_default opts in
  let ret, unused =
    new_subtitle_stream container codec time_base header (mk_opts_array opts)
  in
  filter_opts unused opts;
  mk_stream container ret

external new_data_stream :
  _ container -> Avcodec.Unknown.id -> Avutil.rational -> int
  = "ocaml_av_new_data_stream"

let new_data_stream ~time_base ~codec container =
  let ret = new_data_stream container codec time_base in
  mk_stream container ret

external codec_attr : _ stream -> string option = "ocaml_av_codec_attr"
external bitrate : _ stream -> int option = "ocaml_av_stream_bitrate"

external write_packet :
  (output, 'media, [ `Packet ]) stream ->
  Avutil.rational ->
  'media Avcodec.Packet.t ->
  unit = "ocaml_av_write_stream_packet"

external write_frame :
  ?on_keyframe:(unit -> unit) -> (output, _, [ `Frame ]) stream -> _ -> unit
  = "ocaml_av_write_stream_frame"

let write_subtitle_frame stream frame = write_frame stream frame

external flush : output container -> unit = "ocaml_av_flush"
external tell : _ container -> int option = "ocaml_av_tell"
external close : _ container -> unit = "ocaml_av_close"
