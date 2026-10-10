open Avutil

external version : unit -> version = "ocaml_av_version"
external init : unit -> unit = "ocaml_av_init"
external container_options : unit -> Options.t = "ocaml_av_container_options"

let avformat_version = version ()
let () = init ()
let container_options = container_options ()
let () = Callback.register "ocaml_av_string_of_exception" Printexc.to_string
let failure message = raise (Error (`Failure message))

(* The option bindings of a caller's table, least recent first: the C side
   lets a later binding of a key replace an earlier one. *)
let bindings = function
  | None -> [||]
  | Some opts ->
      Array.of_list (Hashtbl.fold (fun key v l -> (key, v) :: l) opts [])

let report_unused opts unused = Option.iter (filter_opts unused) opts

module Format = struct
  external get_input_name : (input, _) format -> string
    = "ocaml_av_input_format_name"

  external get_input_long_name : (input, _) format -> string
    = "ocaml_av_input_format_long_name"

  external get_output_name : (output, _) format -> string
    = "ocaml_av_output_format_name"

  external get_output_long_name : (output, _) format -> string
    = "ocaml_av_output_format_long_name"

  external find_input_format : string -> (input, 'a) format option
    = "ocaml_av_find_input_format"

  external guess_output_format :
    string -> string -> string -> (output, 'a) format option
    = "ocaml_av_guess_output_format"

  let guess_output_format ?(short_name = "") ?(filename = "") ?(mime = "") () =
    guess_output_format short_name filename mime

  external get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id
    = "ocaml_av_output_format_audio_codec"

  external get_video_codec_id : (output, video) format -> Avcodec.Video.id
    = "ocaml_av_output_format_video_codec"

  external get_subtitle_codec_id :
    (output, subtitle) format -> Avcodec.Subtitle.id
    = "ocaml_av_output_format_subtitle_codec"
end

type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}

type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type seek = int -> Unix.seek_command -> int

(* The media kinds a stream is listed and tagged under; the order is the
   one of the C side. *)
type kind = Audio | Video | Subtitle | Data | Other

type source =
  | Url of { url : string; interrupt : (unit -> bool) option }
  | Custom of { read : read; seek : seek option }

type target =
  | No_file
  | To_url of { url : string; interrupt : (unit -> bool) option }
  | To_custom of { write : write; seek : seek option }

external alloc_container : unit -> 'line container = "ocaml_av_alloc_container"
external release : _ container -> unit = "ocaml_av_release"
external close : _ container -> unit = "ocaml_av_close"

(* A closed container whose release by collection is registered before
   anything is opened in it. *)
let new_container () =
  let container = alloc_container () in
  Gc.finalise release container;
  container

external open_input_source :
  input container ->
  source ->
  (input, _) format option ->
  (string * value) array ->
  string array = "ocaml_av_open_input"

external stream_kinds : _ container -> kind array = "ocaml_av_stream_kinds"

external stream_parameters : _ container -> int -> 'media Avcodec.params
  = "ocaml_av_stream_parameters"

external configure_stream :
  input container ->
  int ->
  (_, Avcodec.decode) Avcodec.codec option ->
  (string * value) array ->
  unit = "ocaml_av_configure_stream"

external find_stream_info : input container -> unit
  = "ocaml_av_find_stream_info"

let open_source ?format ?opts source =
  let container = new_container () in
  report_unused opts (open_input_source container source format (bindings opts));
  container

let open_input ?interrupt ?format ?opts ?configure_audio_stream
    ?configure_video_stream ?configure_subtitle_stream url =
  let container = open_source ?format ?opts (Url { url; interrupt }) in
  let configure index configure =
    Option.iter
      (fun configure ->
        let { codec; opts } = configure (stream_parameters container index) in
        configure_stream container index codec (bindings opts))
      configure
  in
  (try
     Array.iteri
       (fun index -> function
         | Audio -> configure index configure_audio_stream
         | Video -> configure index configure_video_stream
         | Subtitle -> configure index configure_subtitle_stream
         | Data | Other -> ())
       (stream_kinds container);
     find_stream_info container
   with exn ->
     release container;
     raise exn);
  container

let open_input_stream ?format ?opts ?seek read =
  let container = open_source ?format ?opts (Custom { read; seek }) in
  (try find_stream_info container
   with exn ->
     release container;
     raise exn);
  container

external input_duration : input container -> Time_format.t -> Int64.t option
  = "ocaml_av_input_duration"

let get_input_duration ?(format = `Second) container =
  input_duration container format

external metadata : _ container -> int -> (string * string) list
  = "ocaml_av_metadata"

let get_input_metadata container = metadata container (-1)

external get_input_format : input container -> (input, _) format option
  = "ocaml_av_input_format"

external input_obj : input container -> Options.obj = "ocaml_av_input_object"

type ('line, 'media, 'mode) stream = {
  container : 'line container;
  index : int;
}

let streams_of_kind kind container =
  let streams = ref [] in
  Array.iteri
    (fun index stream_kind ->
      if stream_kind = kind then
        streams :=
          (index, { container; index }, stream_parameters container index)
          :: !streams)
    (stream_kinds container);
  List.rev !streams

let get_audio_streams container = streams_of_kind Audio container
let get_video_streams container = streams_of_kind Video container
let get_subtitle_streams container = streams_of_kind Subtitle container
let get_data_streams container = streams_of_kind Data container

external find_best_stream : input container -> kind -> int
  = "ocaml_av_find_best_stream"

let find_best kind container =
  let index = find_best_stream container kind in
  (index, { container; index }, stream_parameters container index)

let find_best_audio_stream container = find_best Audio container
let find_best_video_stream container = find_best Video container
let find_best_subtitle_stream container = find_best Subtitle container
let get_input stream = stream.container
let get_index stream = stream.index
let get_output stream = stream.container
let get_codec_params stream = stream_parameters stream.container stream.index

external stream_avg_frame_rate : _ container -> int -> rational option
  = "ocaml_av_stream_avg_frame_rate"

external stream_set_avg_frame_rate :
  _ container -> int -> rational option -> unit
  = "ocaml_av_stream_set_avg_frame_rate"

external stream_time_base : _ container -> int -> rational
  = "ocaml_av_stream_time_base"

external stream_set_time_base : _ container -> int -> rational -> unit
  = "ocaml_av_stream_set_time_base"

external stream_frame_size : _ container -> int -> int
  = "ocaml_av_stream_frame_size"

external stream_pixel_aspect : _ container -> int -> rational option
  = "ocaml_av_stream_pixel_aspect"

external stream_duration : _ container -> int -> Time_format.t -> Int64.t option
  = "ocaml_av_stream_duration"

let get_avg_frame_rate stream =
  stream_avg_frame_rate stream.container stream.index

let set_avg_frame_rate stream rate =
  stream_set_avg_frame_rate stream.container stream.index rate

let get_time_base stream = stream_time_base stream.container stream.index

let set_time_base stream time_base =
  stream_set_time_base stream.container stream.index time_base

let get_frame_size stream = stream_frame_size stream.container stream.index
let get_pixel_aspect stream = stream_pixel_aspect stream.container stream.index

let get_duration ?(format = `Second) stream =
  stream_duration stream.container stream.index format

let get_metadata stream = metadata stream.container stream.index

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

(* The steps of [read_input], spec/avformat.md §4.3. The C side keeps what
   outlives a call: which decoder was last fed and may hold frames, and
   whether the input ended. Packets and frames come tagged with the kind of
   their stream, which the casts below turn into the result's type. *)

type untyped_packet
type untyped_frame

external read_packet : input container -> (int * kind * untyped_packet) option
  = "ocaml_av_read_packet"

external receive_pending :
  input container -> (int * kind * untyped_frame) option
  = "ocaml_av_receive_pending"

external drain_stream :
  input container -> int -> (int * kind * untyped_frame) option
  = "ocaml_av_drain_stream"

external decode_packet : input container -> int -> untyped_packet -> unit
  = "ocaml_av_decode_packet"

external decode_subtitle :
  input container -> int -> untyped_packet -> Subtitle.frame option
  = "ocaml_av_decode_subtitle"

external typed_packet : untyped_packet -> _ Avcodec.Packet.t = "%identity"
external typed_frame : untyped_frame -> _ frame = "%identity"

let packet_result index kind packet : packet_result option =
  match kind with
    | Audio -> Some (`Audio_packet (index, typed_packet packet))
    | Video -> Some (`Video_packet (index, typed_packet packet))
    | Subtitle -> Some (`Subtitle_packet (index, typed_packet packet))
    | Data -> Some (`Data_packet (index, typed_packet packet))
    | Other -> None

let frame_result (index, kind, frame) : input_result option =
  match kind with
    | Audio -> Some (`Audio_frame (index, typed_frame frame))
    | Video -> Some (`Video_frame (index, typed_frame frame))
    | Subtitle | Data | Other -> None

let indexes container streams =
  List.map
    (fun stream ->
      if stream.container != container then
        failure "the stream belongs to another container";
      stream.index)
    streams

let read_input ?on_unhandled_packet ?(audio_packet = []) ?(audio_frame = [])
    ?(video_packet = []) ?(video_frame = []) ?(subtitle_packet = [])
    ?(subtitle_frame = []) ?(data_packet = []) container =
  let indexes streams = indexes container streams in
  let packets =
    indexes audio_packet @ indexes video_packet @ indexes subtitle_packet
    @ indexes data_packet
  in
  let frames =
    indexes audio_frame @ indexes video_frame @ indexes subtitle_frame
  in
  let rec drain = function
    | [] -> raise (Error `Eof)
    | index :: rest -> (
        match Option.bind (drain_stream container index) frame_result with
          | Some result -> result
          | None -> drain rest)
  in
  let rec read () =
    match read_packet container with
      | None -> drain frames
      | Some (index, kind, packet) when List.mem index packets -> (
          match packet_result index kind packet with
            | Some result -> (result :> input_result)
            | None -> read ())
      | Some (index, Subtitle, packet) when List.mem index frames -> (
          match decode_subtitle container index packet with
            | Some subtitle -> `Subtitle_frame (index, subtitle)
            | None -> read ())
      | Some (index, _, packet) when List.mem index frames ->
          decode_packet container index packet;
          pending ()
      | Some (index, kind, packet) ->
          Option.iter
            (fun handle -> Option.iter handle (packet_result index kind packet))
            on_unhandled_packet;
          read ()
  and pending () =
    match Option.bind (receive_pending container) frame_result with
      | Some result -> result
      | None -> read ()
  in
  pending ()

type seek_flag =
  | Seek_flag_backward
  | Seek_flag_byte
  | Seek_flag_any
  | Seek_flag_frame

external seek_container :
  input container ->
  seek_flag list ->
  int ->
  Time_format.t ->
  Int64.t option * Int64.t option * Int64.t ->
  unit = "ocaml_av_seek"

let seek ?(flags = []) ?stream ?min_ts ?max_ts ~fmt ~ts container =
  let index =
    match stream with
      | None -> -1
      | Some stream -> List.hd (indexes container [stream])
  in
  seek_container container flags index fmt (min_ts, max_ts, ts)

external open_output_target :
  output container ->
  target ->
  (output, _) format option ->
  bool ->
  (string * value) array ->
  string array = "ocaml_av_open_output"

let open_target ?format ?(interleaved = true) ?opts target =
  let container = new_container () in
  report_unused opts
    (open_output_target container target format interleaved (bindings opts));
  container

let open_output ?interrupt ?format ?interleaved ?opts url =
  open_target ?format ?interleaved ?opts (To_url { url; interrupt })

let open_output_format ?interleaved ?opts format =
  open_target ~format ?interleaved ?opts No_file

let open_output_stream ?opts ?interleaved ?seek write format =
  open_target ~format ?interleaved ?opts (To_custom { write; seek })

external output_started : output container -> bool = "ocaml_av_output_started"

external set_container_metadata :
  _ container -> int -> (string * string) list -> unit = "ocaml_av_set_metadata"

let set_output_metadata container metadata =
  set_container_metadata container (-1) metadata

let set_metadata stream metadata =
  set_container_metadata stream.container stream.index metadata

type uninitialized_stream_copy = { output : output container; reserved : int }

external reserve_stream_copy : output container -> int
  = "ocaml_av_reserve_stream_copy"

external initialize_reserved_copy :
  output container -> int -> _ Avcodec.params -> unit
  = "ocaml_av_initialize_stream_copy"

let new_uninitialized_stream_copy output =
  { output; reserved = reserve_stream_copy output }

let initialize_stream_copy ~params { output; reserved } =
  initialize_reserved_copy output reserved params;
  { container = output; index = reserved }

let new_stream_copy ~params output =
  initialize_stream_copy ~params (new_uninitialized_stream_copy output)

type audio_encoding = {
  channel_layout : Channel_layout.t;
  sample_rate : int;
  sample_format : Sample_format.t;
  audio_time_base : rational;
  audio_side_data : Frame_side_data.raw list;
}

external add_audio_stream :
  output container ->
  (string * value) array ->
  audio_encoding ->
  [ `Encoder ] Avcodec.Audio.t ->
  int * string array = "ocaml_av_new_audio_stream"

type video_encoding = {
  frame_rate : rational option;
  hardware_context : Avcodec.Video.hardware_context option;
  pixel_format : Pixel_format.t;
  width : int;
  height : int;
  video_time_base : rational;
  video_side_data : Frame_side_data.raw list;
}

external add_video_stream :
  output container ->
  (string * value) array ->
  video_encoding ->
  [ `Encoder ] Avcodec.Video.t ->
  int * string array = "ocaml_av_new_video_stream"

external add_subtitle_stream :
  output container ->
  (string * value) array ->
  string * rational ->
  [ `Encoder ] Avcodec.Subtitle.t ->
  int * string array = "ocaml_av_new_subtitle_stream"

external add_data_stream :
  output container -> rational -> Avcodec.Unknown.id -> int
  = "ocaml_av_new_data_stream"

let encoding_stream opts container (index, unused) =
  report_unused opts unused;
  { container; index }

let new_audio_stream ?opts ?(side_data = []) ~channel_layout ~sample_rate
    ~sample_format ~time_base ~codec container =
  encoding_stream opts container
    (add_audio_stream container (bindings opts)
       {
         channel_layout;
         sample_rate;
         sample_format;
         audio_time_base = time_base;
         audio_side_data = side_data;
       }
       codec)

let new_video_stream ?opts ?(side_data = []) ?frame_rate ?hardware_context
    ~pixel_format ~width ~height ~time_base ~codec container =
  encoding_stream opts container
    (add_video_stream container (bindings opts)
       {
         frame_rate;
         hardware_context;
         pixel_format;
         width;
         height;
         video_time_base = time_base;
         video_side_data = side_data;
       }
       codec)

(* An empty header is none: the C side gives the header to text codecs
   only. *)
let new_subtitle_stream ?opts ?header ~time_base ~codec container =
  let header = Option.value header ~default:(Subtitle.header_ass_default ()) in
  encoding_stream opts container
    (add_subtitle_stream container (bindings opts) (header, time_base) codec)

let new_data_stream ~time_base ~codec container =
  { container; index = add_data_stream container time_base codec }

external stream_codec_attr : _ container -> int -> string option
  = "ocaml_av_codec_attr"

external stream_bitrate : _ container -> int -> int option = "ocaml_av_bitrate"

let codec_attr stream = stream_codec_attr stream.container stream.index
let bitrate stream = stream_bitrate stream.container stream.index

external write_stream_packet :
  output container -> int -> rational -> _ Avcodec.Packet.t -> unit
  = "ocaml_av_write_packet"

let write_packet stream time_base packet =
  write_stream_packet stream.container stream.index time_base packet

(* The steps of [write_frame]: a send answers [false] when the encoder wants
   its output read first; a receive holds the packet on the C side until
   [write_encoded], so that [on_keyframe] runs before the muxer gets it. *)

type received = Nothing | Packet | Key_packet

external send_frame : output container -> int -> _ frame option -> bool
  = "ocaml_av_stream_send_frame"

external receive_packet : output container -> int -> received
  = "ocaml_av_stream_receive_packet"

external write_encoded : output container -> int -> unit
  = "ocaml_av_write_encoded"

let rec write_available ?on_keyframe container index =
  match receive_packet container index with
    | Nothing -> ()
    | Packet ->
        write_encoded container index;
        write_available ?on_keyframe container index
    | Key_packet ->
        Fun.protect
          ~finally:(fun () -> write_encoded container index)
          (fun () -> Option.iter (fun notify -> notify ()) on_keyframe);
        write_available ?on_keyframe container index

let write_frame ?on_keyframe { container; index } frame =
  let rec send () =
    write_available ?on_keyframe container index;
    if not (send_frame container index (Some frame)) then send ()
  in
  send ();
  write_available ?on_keyframe container index

external write_subtitle : output container -> int -> Subtitle.frame -> unit
  = "ocaml_av_write_subtitle"

let write_subtitle_frame stream subtitle =
  write_subtitle stream.container stream.index subtitle

external flush : output container -> unit = "ocaml_av_flush"
external tell : _ container -> int option = "ocaml_av_tell"
