open Ctypes
open Foreign

module type Config = sig
  val filename : string
end

exception Library_not_found
exception Library_initialized of string

type source = { source_name : string; source_url : string }

let strlen = foreign "strlen" (ptr char @-> returning int)

(*
let malloc = foreign "malloc" (size_t @-> returning (ptr void))

let malloc typ =
  let ptr = malloc (Unsigned.Size_t.of_int (sizeof typ)) in
  if is_null ptr then failwith "out of memory!";
  from_voidp typ ptr

let memcpy =
  foreign "memcpy" (ptr void @-> ptr void @-> size_t @-> returning void)

let memcpy : 'a. 'a ptr -> 'a ptr -> unit =
 fun dst src ->
  memcpy (to_voidp dst) (to_voidp src)
    (Unsigned.Size_t.of_int (sizeof (reference_type dst)))
*)

let opt_str_ptr = function
  | None -> from_voidp char null
  | Some s ->
      let s = CArray.of_string s in
      CArray.start s

let opt_int64 = function None -> Int64.max_int | Some i -> i
let source_struct : [ `Source ] structure typ = structure "NDIlib_source_t"
let source_p_ndi_name = field source_struct "p_ndi_name" (ptr char)
let source_p_url_address = field source_struct "p_url_address" (ptr char)
let () = seal source_struct

let source_name source =
  let name = getf !@source source_p_ndi_name in
  if is_null name then "" else string_from_ptr name ~length:(strlen name)

let source_url source =
  let url = getf !@source source_p_url_address in
  if is_null url then "" else string_from_ptr url ~length:(strlen url)

let find_create_struct : [ `Find_create ] structure typ =
  structure "NDIlib_find_create_t"

let find_create_show_local_sources =
  field find_create_struct "show_local_sources" bool

let find_create_p_groups = field find_create_struct "p_groups" (ptr char)
let find_create_p_extra_ips = field find_create_struct "p_extra_ips" (ptr char)
let () = seal find_create_struct
let find_instance = typedef void "NDIlib_find_instance_t"

let send_create_struct : [ `Send_create ] structure typ =
  structure "NDIlib_send_create_t"

let send_create_p_ndi_name = field send_create_struct "p_ndi_name" (ptr char)
let send_create_p_groups = field send_create_struct "p_groups" (ptr char)
let send_create_clock_video = field send_create_struct "clock_video" bool
let send_create_clock_audio = field send_create_struct "clock_audio" bool
let () = seal send_create_struct
let send_instance = typedef void "NDIlib_send_instance_t"

module Frame = struct
  (*
  type frame_type =
    [ `None | `Video | `Audio | `Metadata | `Error | `Status_change ]

  let frame_type = function
    | `None -> 0
    | `Video -> 1
    | `Audio -> 2
    | `Metadata -> 3
    | `Error -> 4
    | `Status_change -> 100
*)

  let four_cc a b c d =
    Char.code a
    lor (Char.code b lsl 8)
    lor (Char.code c lsl 16)
    lor (Char.code d lsl 24)

  module Video = struct
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

    let four_cc_code = function `I420 -> four_cc 'I' '4' '2' '0'
    let format_code = function `Progressive -> 1

    let video_frame_struct : [ `Video_frame_v2 ] structure typ =
      structure "NDIlib_video_frame_v2_t"

    let xres_field = field video_frame_struct "xres" int
    let yres_field = field video_frame_struct "yres" int
    let four_cc_field = field video_frame_struct "FourCC" int
    let frame_rate_N_field = field video_frame_struct "frame_rate_N" int
    let frame_rate_D_field = field video_frame_struct "frame_rate_D" int

    let picture_aspect_ratio_field =
      field video_frame_struct "picture_aspect_ratio" float

    let format_field = field video_frame_struct "format" int
    let timecode_field = field video_frame_struct "timecode" int64_t
    let data_field = field video_frame_struct "data" (ptr uint8_t)

    let line_stride_in_bytes_field =
      field video_frame_struct "line_stride_in_bytes" int

    let metadata_field = field video_frame_struct "metadata" (ptr char)
    let timestamp_field = field video_frame_struct "timestamp" int64_t
    let () = seal video_frame_struct

    let frame
        {
          xres;
          yres;
          frame_rate_N;
          frame_rate_D;
          picture_aspect_ratio;
          format;
          timecode;
          data;
          metadata;
          timestamp;
        } =
      let frame = make video_frame_struct in
      setf frame xres_field xres;
      setf frame yres_field yres;
      setf frame frame_rate_N_field frame_rate_N;
      setf frame frame_rate_D_field frame_rate_D;
      setf frame picture_aspect_ratio_field
        (Option.value ~default:0. picture_aspect_ratio);
      setf frame format_field (format_code format);
      setf frame timecode_field (opt_int64 timecode);
      setf frame metadata_field (opt_str_ptr metadata);
      setf frame timestamp_field (opt_int64 timestamp);

      let four_cc, data, stride =
        match data with
          | `I420 { data; stride } ->
              let four_cc = four_cc_code `I420 in
              let data =
                coerce (ptr int) (ptr uint8_t) (bigarray_start array1 data)
              in
              (four_cc, data, stride)
      in
      setf frame four_cc_field four_cc;
      setf frame data_field data;
      setf frame line_stride_in_bytes_field stride;

      frame
  end

  module Audio = struct
    type fltp = {
      data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
      stride : int;
    }

    type data = [ `Fltp of fltp ]

    let four_cc_code = function `Fltp -> four_cc 'F' 'L' 'T' 'p'

    type t = {
      sample_rate : int;
      channels : int;
      samples : int;
      timecode : int64 option;
      data : data;
      metadata : string option;
      timestamp : int64 option;
    }

    let audio_frame_struct : [ `Audio_frame_v3 ] structure typ =
      structure "NDIlib_audio_frame_v3_t"

    let sample_rate_field = field audio_frame_struct "sample_rate" int
    let no_channels_field = field audio_frame_struct "no_channels" int
    let no_samples_field = field audio_frame_struct "no_samples" int
    let timecode_field = field audio_frame_struct "timecode" int64_t
    let four_cc_field = field audio_frame_struct "FourCC" int
    let data_field = field audio_frame_struct "data" (ptr uint8_t)

    let channel_stride_in_bytes_field =
      field audio_frame_struct "channel_stride_in_bytes" int

    let p_metadata_field = field audio_frame_struct "p_metadata" (ptr char)
    let timestamp_field = field audio_frame_struct "timestamp" int64_t
    let () = seal audio_frame_struct

    let frame
        { sample_rate; channels; samples; timecode; data; metadata; timestamp }
        =
      let frame = make audio_frame_struct in
      setf frame sample_rate_field sample_rate;
      setf frame no_channels_field channels;
      setf frame no_samples_field samples;
      setf frame timecode_field (opt_int64 timecode);
      let four_cc, data, stride =
        match data with
          | `Fltp { data; stride } ->
              let four_cc = four_cc_code `Fltp in
              let data =
                coerce (ptr float) (ptr uint8_t) (bigarray_start array1 data)
              in
              (four_cc, data, stride)
      in
      setf frame four_cc_field four_cc;
      setf frame data_field data;
      setf frame channel_stride_in_bytes_field stride;

      setf frame p_metadata_field (opt_str_ptr metadata);
      setf frame timestamp_field (opt_int64 timestamp);
      frame
  end
end

module C (Conf : Config) = struct
  let lib =
    try Dl.dlopen ~filename:Conf.filename ~flags:[Dl.RTLD_NOW]
    with _ -> raise Library_not_found

  let foreign = foreign ~from:lib
  let initialize = foreign "NDIlib_initialize" (void @-> returning void)
  let destroy = foreign "NDIlib_destroy" (void @-> returning void)
  let version = foreign "NDIlib_version" (void @-> returning string)

  let find_create_v2 =
    foreign "NDIlib_find_create_v2"
      (ptr find_create_struct @-> returning (ptr_opt find_instance))

  let find_destroy =
    foreign "NDIlib_find_destroy" (ptr find_instance @-> returning void)

  let find_wait_for_sources =
    foreign "NDIlib_find_wait_for_sources"
      (ptr find_instance @-> uint32_t @-> returning bool)

  let get_current_sources =
    foreign "NDIlib_find_get_current_sources"
      (ptr find_instance @-> ptr uint32_t @-> returning (ptr source_struct))

  let find ?(show_local_sources = true) ?groups ?extra_ips ?(timeout = 500) () =
    let find_create = make find_create_struct in
    setf find_create find_create_show_local_sources show_local_sources;
    setf find_create find_create_p_groups (opt_str_ptr groups);
    setf find_create find_create_p_extra_ips (opt_str_ptr extra_ips);
    match find_create_v2 (addr find_create) with
      | None -> failwith "Error while creating find_create instance!"
      | Some f ->
          Gc.finalise find_destroy f;
          while
            not (find_wait_for_sources f (Unsigned.UInt32.of_int timeout))
          do
            ()
          done;
          let nb_sources = allocate uint32_t (Unsigned.UInt32.of_int 0) in
          let source = get_current_sources f nb_sources in
          let nb_sources = Unsigned.UInt32.to_int !@nb_sources in
          let rec get pos sources =
            if pos < nb_sources then (
              let s = source +@ pos in
              get (pos + 1)
                ({ source_name = source_name s; source_url = source_url s }
                :: sources))
            else sources
          in
          get 0 []

  let send_create =
    foreign "NDIlib_send_create"
      (ptr send_create_struct @-> returning (ptr send_instance))

  let send_destroy =
    foreign "NDIlib_send_destroy" (ptr send_instance @-> returning void)

  let send_create ?groups ?(clock_video = false) ?(clock_audio = false) ?name ()
      =
    let send_create_settings = make send_create_struct in
    setf send_create_settings send_create_p_ndi_name (opt_str_ptr name);
    setf send_create_settings send_create_p_groups (opt_str_ptr groups);
    setf send_create_settings send_create_clock_video clock_video;
    setf send_create_settings send_create_clock_audio clock_audio;
    let send_instance = send_create (addr send_create_settings) in
    send_instance

  let send_audio_v3 =
    foreign "NDIlib_send_send_audio_v3"
      (ptr void @-> ptr Frame.Audio.audio_frame_struct @-> returning void)

  let send_video_v2 =
    foreign "NDIlib_send_send_video_v2"
      (ptr void @-> ptr Frame.Video.video_frame_struct @-> returning void)
end

module type C = module type of C (struct
  let filename = "foo"
end)

type t = { _module : (module C) }

let initialized = Atomic.make None

let init ~filename () =
  (match Atomic.get initialized with
    | Some f when f <> filename -> raise (Library_initialized f)
    | _ -> Atomic.set initialized (Some filename));
  try
    let module C = C (struct
      let filename = filename
    end) in
    C.initialize ();
    let _module = (module C : C) in
    let finalise _ = C.destroy () in
    let handler = { _module = (module C : C) } in
    Gc.finalise finalise handler;
    handler
  with _ -> raise Library_not_found

let version { _module } =
  let module C = (val _module : C) in
  C.version ()

let find ?show_local_sources ?groups ?extra_ips ?timeout { _module } =
  let module C = (val _module : C) in
  let extra_ips = Option.map (fun ips -> String.concat "," ips) extra_ips in
  C.find ?show_local_sources ?groups ?extra_ips ?timeout ()

module Send = struct
  type sender = { sender_module : (module C); sender : unit ptr }

  let init ?clock_audio ?clock_video ?groups ?name { _module } =
    let module C = (val _module : C) in
    {
      sender_module = _module;
      sender = C.send_create ?clock_audio ?clock_video ?groups ?name ();
    }

  let send_audio { sender_module; sender } frame =
    let module C = (val sender_module : C) in
    let frame = Frame.Audio.frame frame in
    C.send_audio_v3 sender (addr frame)

  let send_video { sender_module; sender } frame =
    let module C = (val sender_module : C) in
    let frame = Frame.Video.frame frame in
    C.send_video_v2 sender (addr frame)

  let destroy { sender_module; sender } =
    let module C = (val sender_module : C) in
    C.send_destroy sender
end
