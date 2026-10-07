(* Conformance of avutil: spec/tests.md §1, and the entries of
   spec/known-complexity.md that avutil answers alone ([kc.*]).

   Requirements that need a container, a codec or a filter (1.3 on a real
   container, 1.5 on registered classes, 1.7 on real operations, 1.13 while
   encoding) are completed by the test programs of those libraries. *)

open Avutil
open Harness

external error_codes : unit -> (int * string) array = "test_error_codes"
external raise_error : int -> 'a = "test_raise_error"
external wrap_null_frame : unit -> video frame = "test_wrap_null_frame"
external wrap_null_subtitle : unit -> Subtitle.frame = "test_wrap_null_subtitle"
external pixel_format_of_id : int -> Pixel_format.t = "test_pixel_format_of_id"
external media_types : unit -> media_type array = "test_media_types"
external time_format_units : Time_format.t -> int = "test_time_format_units"
external rational_roundtrip : rational -> rational = "test_rational_roundtrip"
external bigarray_kind : Sample_format.t -> int = "test_bigarray_kind"
external bigarray_kinds : unit -> int array = "test_bigarray_kinds"
external parse_double : string -> float = "test_parse_double"

external options_dictionary : (string * value) array -> (string * string) list
  = "test_options_dictionary"

external unused_options : (string * value) array -> string -> string array
  = "test_unused_options"

external log : int -> string -> unit = "test_log"
external log_from_threads : int -> int -> unit = "test_log_from_threads"
external call_from_c_thread : (unit -> unit) -> unit = "test_call_from_c_thread"

external set_video_properties : video frame -> unit
  = "test_set_video_properties"

external plane_size : video frame -> int -> int = "test_plane_size"
external touch_frame : _ frame -> unit = "test_touch_frame"
external hardware_frame : unit -> video frame = "test_hardware_frame"
external option_class : unit -> Options.t = "test_option_class"
external no_option_class : unit -> Options.t = "test_no_option_class"
external option_object : bool -> Options.obj = "test_option_object"
external finalized_option_owners : unit -> int = "test_finalized_option_owners"
external control_overflow : int -> unit = "control_overflow"
external control_leak : unit -> unit = "control_leak"
external control_unrooted : unit -> string * int = "control_unrooted"

let is_failure = function Error (`Failure _) -> true | _ -> false
let is_error = function Error _ -> true | _ -> false
let is_not_found = function Not_found -> true | _ -> false

(* Waits for [condition], which another thread makes true. *)
let wait_until condition =
  let deadline = Unix.gettimeofday () +. 30. in
  while (not (condition ())) && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  check (condition ()) "a condition another thread sets became true"

let color_names (type a) ~(name : a -> string) ~(from_name : string -> a option)
    (all : a list) =
  let named = List.filter (fun v -> name v <> "") all in
  check (List.length named > 2) "FFmpeg names several values";
  List.iter
    (fun v ->
      match from_name (name v) with
        | Some found -> equal (name v) (name found) ("from_name " ^ name v)
        | None -> check false ("from_name finds " ^ name v))
    named;
  equal None (from_name "no such name") "an unknown name gives None"

let requirement_1_1 () =
  color_names ~name:Color_space.name ~from_name:Color_space.from_name
    Avutil__Color_space.t;
  color_names ~name:Color_range.name ~from_name:Color_range.from_name
    Avutil__Color_range.t;
  color_names ~name:Color_primaries.name ~from_name:Color_primaries.from_name
    Avutil__Color_primaries.t;
  color_names ~name:Color_trc.name ~from_name:Color_trc.from_name
    Avutil__Color_trc.t;
  color_names ~name:Chroma_location.name ~from_name:Chroma_location.from_name
    Avutil__Chroma_location.t;
  equal (Some `Bt709) (Color_space.from_name "bt709") "a literal constructor";
  equal "bt709" (Color_space.name `Bt709) "a literal name"

(* Every constructor converts to C and back to the first constructor declared
   with its C value. *)
let round_trips ~get_id ~find_id all =
  List.iter
    (fun v ->
      let first = List.find (fun other -> get_id other = get_id v) all in
      equal first (find_id (get_id v)) "a constructor converts to C and back")
    all

let requirement_1_2 () =
  round_trips ~get_id:Pixel_format.get_id ~find_id:Pixel_format.find_id
    Avutil__Pixel_format.t;
  round_trips ~get_id:Sample_format.get_id ~find_id:Sample_format.find_id
    Avutil__Sample_format.t;
  equal 0 (Pixel_format.get_id `Yuv420p) "the C value of yuv420p";
  equal (-1) (Pixel_format.get_id `None) "the C value of the none pixel format";
  equal 1 (Sample_format.get_id `S16) "the C value of s16";
  equal `Rgb24 (Pixel_format.of_string "rgb24") "a pixel format by name";
  equal (Some "rgb24") (Pixel_format.to_string `Rgb24) "a pixel format name";
  equal None (Pixel_format.to_string `None) "the none pixel format has no name";
  equal `Fltp (Sample_format.find "fltp") "a sample format by name";
  equal (Some "fltp") (Sample_format.get_name `Fltp) "a sample format name";
  equal None (Sample_format.get_name `None) "the none sample format has no name";
  raises is_not_found "Pixel_format.find_id" (fun () ->
      Pixel_format.find_id 99999);
  raises is_not_found "Sample_format.find_id" (fun () ->
      Sample_format.find_id 99999);
  raises is_not_found "Sample_format.find" (fun () -> Sample_format.find "nope");
  raises is_failure "Pixel_format.of_string" (fun () ->
      Pixel_format.of_string "nope");
  raises is_failure "a C value with no constructor" (fun () ->
      pixel_format_of_id 99999)

let requirement_1_3 () =
  let obj = option_object false in
  equal "changed" (Options.get_string ~name:"string_option" obj) "get_string";
  equal "-4"
    (Options.get_string ~name:"int_option" obj)
    "get_string of a number";
  equal (-4) (Options.get_int ~name:"int_option" obj) "get_int";
  equal Int64.max_int (Options.get_int64 ~name:"int64_option" obj) "get_int64";
  equal 0.75 (Options.get_float ~name:"double_option" obj) "get_float";
  equal { num = 3; den = 4 }
    (Options.get_rational ~name:"rational_option" obj)
    "get_rational";
  equal (640, 360)
    (Options.get_image_size ~name:"size_option" obj)
    "get_image_size";
  equal `Rgb24 (Options.get_pixel_fmt ~name:"pixel_option" obj) "get_pixel_fmt";
  equal `Fltp
    (Options.get_sample_fmt ~name:"sample_option" obj)
    "get_sample_fmt";
  equal
    { num = 30000; den = 1001 }
    (Options.get_video_rate ~name:"rate_option" obj)
    "get_video_rate";
  check
    (Channel_layout.compare
       (Channel_layout.find "FR+FL")
       (Options.get_channel_layout ~name:"layout_option" obj))
    "get_channel_layout";
  equal
    [("first", "1"); ("second", "2")]
    (Options.get_dictionary ~name:"dict_option" obj)
    "get_dictionary";
  raises is_failure "an int64 outside the integer range" (fun () ->
      Options.get_int ~name:"int64_option" obj);
  raises
    (function Error `Option_not_found -> true | _ -> false)
    "an unknown option"
    (fun () -> Options.get_int ~name:"no_such_option" obj);
  raises
    (function Error (`Failure "Container closed!") -> true | _ -> false)
    "a closed owner"
    (fun () -> Options.get_int ~name:"int_option" (option_object true))

let requirement_1_4 () =
  let obj = option_object false in
  let not_found = function Error `Option_not_found -> true | _ -> false in
  raises not_found "search_children omitted" (fun () ->
      Options.get_int ~name:"child_only" obj);
  raises not_found "search_children false" (fun () ->
      Options.get_int ~search_children:false ~name:"child_only" obj);
  equal 42
    (Options.get_int ~search_children:true ~name:"child_only" obj)
    "search_children true"

let unbounded default : _ Options.entry =
  { default; min = None; max = None; values = [] }

let requirement_1_5 () =
  let options = Options.opts (option_class ()) in
  let spec name =
    (List.find (fun (o : Options.opt) -> o.name = name) options).spec
  in
  equal
    [
      "int_option";
      "int64_option";
      "uint64_option";
      "flags_option";
      "double_option";
      "float_option";
      "string_option";
      "rational_option";
      "size_option";
      "pixel_option";
      "sample_option";
      "rate_option";
      "duration_option";
      "color_option";
      "layout_option";
      "bool_option";
      "auto_bool_option";
      "dict_option";
      "array_option";
      "child_only";
    ]
    (List.map (fun (o : Options.opt) -> o.name) options)
    "options in FFmpeg's order, then the child's; other types left out";
  equal
    (`Int
       {
         Options.default = Some 3;
         min = Some (-10);
         max = Some 10;
         values = [("low", 1); ("high", 2)];
       })
    (spec "int_option") "an integer option and its named constants";
  equal
    (`Flags
       {
         Options.default = Some 1L;
         min = Some 0L;
         max = Some 3L;
         values = [("first", 1L); ("second", 2L)];
       })
    (spec "flags_option") "a flags option";
  equal
    (`Double
       {
         Options.default = Some 0.5;
         min = Some (-1.);
         max = Some 1.;
         values = [("half", 0.5)];
       })
    (spec "double_option") "a double option";
  equal
    (`Rational
       {
         Options.default = Some { num = 1; den = 2 };
         min = Some { num = 0; den = 1 };
         max = Some { num = 10; den = 1 };
         values = [];
       })
    (spec "rational_option") "a rational option";
  equal (`String (unbounded (Some "default"))) (spec "string_option") "a string";
  equal (`Image_size (unbounded (Some "320x240"))) (spec "size_option") "a size";
  equal (`Video_rate (unbounded (Some "25"))) (spec "rate_option") "a rate";
  equal (`Color (unbounded (Some "red"))) (spec "color_option") "a colour";
  equal (`Dict (unbounded None)) (spec "dict_option") "an absent default";
  equal
    (`Pixel_fmt (unbounded (Some `Yuv420p)))
    (spec "pixel_option") "a format";
  equal (`Sample_fmt (unbounded None)) (spec "sample_option") "no format";
  equal (`Bool (unbounded (Some true))) (spec "bool_option") "a boolean";
  equal (`Bool (unbounded None)) (spec "auto_bool_option") "a negative boolean";
  equal
    (`Array
       (`Int
          { Options.default = None; min = Some 0; max = Some 100; values = [] }))
    (spec "array_option") "an array option";
  (match spec "layout_option" with
    | `Channel_layout { default = Some layout; _ } ->
        check
          (Channel_layout.compare layout Channel_layout.stereo)
          "a layout default"
    | _ -> check false "a layout option");
  let first = List.hd options and second = List.nth options 1 in
  equal (Some "an integer") first.help "a help text";
  equal None second.help "an empty help text is absent";
  equal [`Encoding_param; `Audio_param] second.flags "flags in ascending order";
  equal [] (Options.opts (no_option_class ())) "no class lists no option"

let requirement_1_6 () =
  let options = Options.opts (option_class ()) in
  let spec name =
    (List.find (fun (o : Options.opt) -> o.name = name) options).spec
  in
  equal
    (`Int64
       {
         Options.default = Some 5L;
         min = Some Int64.min_int;
         max = Some Int64.max_int;
         values = [];
       })
    (spec "int64_option") "the bounds of a full-range 64-bit option";
  equal
    (`UInt64
       {
         Options.default = Some 6L;
         min = Some 0L;
         max = Some Int64.max_int;
         values = [];
       })
    (spec "uint64_option") "a bound above the 64-bit range saturates"

let requirement_1_7 () =
  let rendered =
    options_dictionary
      [|
        ("a", `String "x");
        ("b", `Int 42);
        ("c", `Int64 (-7L));
        ("d", `Float 0.1);
        ("a", `String "y");
      |]
  in
  equal
    [("a", "y"); ("b", "42"); ("c", "-7")]
    (List.sort compare (List.filter (fun (key, _) -> key <> "d") rendered))
    "values are rendered and a later binding wins";
  equal 0.1
    (parse_double (List.assoc "d" rendered))
    "FFmpeg parses a float back";
  let opts = Hashtbl.create 16 in
  Hashtbl.replace opts "valid" (`Int 1);
  for i = 1 to 20000 do
    Hashtbl.replace opts (Printf.sprintf "unknown%d" i) (`Int i)
  done;
  let bindings = Hashtbl.fold (fun key v l -> (key, v) :: l) opts [] in
  let unused = unused_options (Array.of_list bindings) "valid" in
  equal 20000 (Array.length unused) "every unused key is reported";
  filter_opts unused opts;
  equal 20000 (Hashtbl.length opts) "the table holds the unused keys only";
  check (not (Hashtbl.mem opts "valid")) "the consumed key is gone";
  for i = 1 to 20000 do
    equal
      (Some (`Int i))
      (Hashtbl.find_opt opts (Printf.sprintf "unknown%d" i))
      "an unused key keeps its value"
  done;
  let opts = Hashtbl.create 2 in
  Hashtbl.add opts "a" (`Int 0);
  Hashtbl.add opts "a" (`Int 1);
  Hashtbl.add opts "b" (`String "x");
  equal ["a=1"; "b=x"]
    (List.sort compare (String.split_on_char ',' (string_of_opts opts)))
    "string_of_opts counts a key once, with its most recent binding"

let video_frame () = Video.create_frame 64 48 `Yuv420p

let requirement_1_8 () =
  let frame = video_frame () in
  equal None (Frame.pts frame) "a new frame has no timestamp";
  set_video_properties frame;
  equal (Some 1234L) (Frame.pts frame) "pts as FFmpeg holds it";
  equal (Some 4321L) (Frame.best_effort_timestamp frame) "best-effort timestamp";
  equal (Some 5678L) (Frame.pkt_dts frame) "pkt_dts";
  equal (Some 90L) (Frame.duration frame) "duration";
  Frame.set_pts frame (Some 42L);
  equal (Some 42L) (Frame.pts frame) "pts after set_pts";
  equal (Some 42L)
    (Frame.best_effort_timestamp frame)
    "best-effort after set_pts";
  Frame.set_pts frame None;
  equal None (Frame.pts frame) "pts cleared";
  equal None (Frame.best_effort_timestamp frame) "best-effort cleared";
  Frame.set_pkt_dts frame (Some 7L);
  equal (Some 7L) (Frame.pkt_dts frame) "set_pkt_dts";
  Frame.set_pkt_dts frame None;
  equal None (Frame.pkt_dts frame) "pkt_dts cleared";
  Frame.set_duration frame (Some 3L);
  equal (Some 3L) (Frame.duration frame) "set_duration";
  Frame.set_duration frame None;
  equal None (Frame.duration frame) "duration cleared"

let requirement_1_9 () =
  let frame = video_frame () in
  equal [] (Frame.metadata frame) "a new frame has no metadata";
  set_video_properties frame;
  equal [("native", "value")] (Frame.metadata frame) "metadata set by FFmpeg";
  Frame.set_metadata frame [("a", "1"); ("b", "2")];
  equal [("a", "1"); ("b", "2")] (Frame.metadata frame) "set_metadata replaces";
  Frame.set_metadata frame [("b", "3")];
  equal [("b", "3")] (Frame.metadata frame) "a second set_metadata replaces";
  Frame.set_metadata frame [("k", "1"); ("k", "2")];
  equal
    [("k", "2")]
    (Frame.metadata frame) "a repeated key keeps its last value";
  Frame.set_metadata frame [];
  equal [] (Frame.metadata frame) "the empty list clears"

(* The planes of a fresh [width]x[height] frame, kept after the frame is
   dropped. *)
let visited_planes (format, plane_count) width height =
  let frame = Video.create_frame width height format in
  let descriptor = Pixel_format.descriptor format in
  let kept = ref [||] in
  let returned =
    Video.frame_visit ~make_writable:true (fun planes -> kept := planes) frame
  in
  check (returned == frame) "frame_visit returns its frame";
  equal plane_count (Array.length !kept) "one pair per plane of the format";
  Array.iteri
    (fun i (data, linesize) ->
      let plane_height =
        if i = 0 then height else -(-height asr descriptor.log2_chroma_h)
      in
      equal (Video.frame_get_linesize frame i) linesize "the stride";
      equal (linesize * plane_height) (Bigarray.Array1.dim data)
        "a plane is its line size times its height";
      equal (plane_size frame i) (Bigarray.Array1.dim data)
        "a plane has the size FFmpeg computes")
    !kept;
  !kept

let requirement_1_10 () =
  List.iter
    (fun format ->
      let planes = visited_planes format 322 243 in
      collect ();
      Array.iter
        (fun (data, _) ->
          Bigarray.Array1.fill data 9;
          equal 9
            data.{Bigarray.Array1.dim data - 1}
            "a plane outlives its frame, readable and writable")
        planes)
    [
      (`Yuv420p, 3);
      (`Yuv422p, 3);
      (`Nv12, 2);
      (`Gray8, 1);
      (`Rgb24, 1);
      (`Pal8, 1);
    ];
  let frame = video_frame () in
  let first = ref [||] and second = ref [||] in
  ignore (Video.frame_visit ~make_writable:false (fun p -> first := p) frame);
  ignore (Video.frame_visit ~make_writable:false (fun p -> second := p) frame);
  (fst !first.(0)).{0} <- 1;
  equal 1 (fst !second.(0)).{0} "planes share the frame's buffer";
  ignore (Video.frame_visit ~make_writable:true (fun p -> second := p) frame);
  equal 1 (fst !second.(0)).{0} "make_writable copies the shared data";
  (fst !second.(0)).{0} <- 2;
  equal 1
    (fst !first.(0)).{0}
    "an earlier plane keeps the buffer it was built on";
  raises
    (fun exn -> exn = Exit)
    "the visitor's exception"
    (fun () ->
      Video.frame_visit ~make_writable:false (fun _ -> raise Exit) frame);
  raises is_failure "a plane index too large" (fun () ->
      Video.frame_get_linesize frame 3);
  raises is_failure "a negative plane index" (fun () ->
      Video.frame_get_linesize frame (-1));
  raises is_failure "frame_visit on a hardware frame" (fun () ->
      Video.frame_visit ~make_writable:false ignore (hardware_frame ()));
  raises is_failure "a width below 1" (fun () ->
      Video.create_frame 0 48 `Yuv420p);
  raises is_error "a frame with no pixel format" (fun () ->
      Video.create_frame 64 48 `None)

let data_of_string s =
  let data = create_data (String.length s) in
  String.iteri (fun i c -> data.{i} <- Char.code c) s;
  data

let text_content : Subtitle.content =
  {
    format = 1;
    start_display_time = 100;
    end_display_time = 2500;
    pts = Some 123456L;
    rectangles =
      [
        {
          pict = None;
          flags = [];
          rect_type = `Ass;
          text = "";
          ass =
            "Dialogue: 0,0:00:00.10,0:00:02.50,Default,,0,0,0,,Bonjour\\Nà tous";
        };
        {
          pict = None;
          flags = [`Forced];
          rect_type = `Text;
          text = "plain";
          ass = "";
        };
      ];
  }

let bitmap_content planes : Subtitle.content =
  {
    format = 0;
    start_display_time = 0;
    end_display_time = 4294967295;
    pts = None;
    rectangles =
      [
        {
          pict = Some { x = 10; y = 20; w = 4; h = 2; nb_colors = 4; planes };
          flags = [];
          rect_type = `Bitmap;
          text = "";
          ass = "";
        };
      ];
  }

let bitmap_planes ?(pixels = "01230123") () =
  ( [|
      data_of_string pixels;
      data_of_string (String.make 1024 'p');
      create_data 0;
      create_data 0;
    |],
    [| 4; 0; 0; 0 |] )

let requirement_1_11 () =
  let round_trip content =
    let frame = Subtitle.create_frame content in
    equal content
      (Subtitle.get_content frame)
      "get_content (create_frame c) = c";
    equal content.pts (Subtitle.get_pts frame) "get_pts"
  in
  round_trip text_content;
  round_trip (bitmap_content (bitmap_planes ()));
  round_trip { text_content with rectangles = [] };
  let rejected description content =
    raises is_failure description (fun () -> Subtitle.create_frame content)
  in
  let data, linesizes = bitmap_planes () in
  rejected "three planes" (bitmap_content (Array.sub data 0 3, linesizes));
  rejected "three line sizes" (bitmap_content (data, Array.sub linesizes 0 3));
  rejected "a plane of a wrong size"
    (bitmap_content (bitmap_planes ~pixels:"0123012" ()));
  rejected "an empty first plane" (bitmap_content (bitmap_planes ~pixels:"" ()));
  rejected "a negative display time"
    { text_content with start_display_time = -1 };
  rejected "a display time above 32 bits"
    { text_content with end_display_time = 4294967296 };
  rejected "a failure after a rectangle was built"
    {
      text_content with
      rectangles =
        text_content.rectangles
        @ (bitmap_content (bitmap_planes ~pixels:"0" ())).rectangles;
    };
  rejected "a failure before the last rectangle is built"
    {
      text_content with
      rectangles =
        (bitmap_content (bitmap_planes ~pixels:"0" ())).rectangles
        @ text_content.rectangles;
    };
  raises is_failure "a null subtitle" wrap_null_subtitle

let requirement_1_12 () =
  let through_frame layout =
    Audio.frame_get_channel_layout (Audio.create_frame `Fltp layout 44100 16)
  in
  let round_trip layout =
    let copy = through_frame layout in
    check (Channel_layout.compare layout copy) "a layout round-trips";
    equal
      (Channel_layout.get_description layout)
      (Channel_layout.get_description copy)
      "its description round-trips";
    equal
      (Channel_layout.get_mask layout)
      (Channel_layout.get_mask copy)
      "its mask too"
  in
  let custom = Channel_layout.find "FR+FL" in
  round_trip Channel_layout.stereo;
  round_trip Channel_layout.five_point_one;
  round_trip custom;
  equal "stereo"
    (Channel_layout.get_description Channel_layout.stereo)
    "description";
  equal 2 (Channel_layout.get_nb_channels Channel_layout.stereo) "channel count";
  equal 6 (Channel_layout.get_nb_channels Channel_layout.five_point_one) "5.1";
  equal (Some 3L)
    (Channel_layout.get_mask Channel_layout.stereo)
    "the stereo mask";
  equal (Some 4L) (Channel_layout.get_mask Channel_layout.mono) "the mono mask";
  equal None (Channel_layout.get_mask custom) "a custom order has no mask";
  equal 2 (Channel_layout.get_nb_channels custom) "a custom layout's channels";
  check
    (not (Channel_layout.compare custom Channel_layout.stereo))
    "layouts differ";
  check
    (Channel_layout.compare
       (Channel_layout.get_default 2)
       Channel_layout.stereo)
    "the default layout of two channels";
  check
    (List.exists
       (Channel_layout.compare Channel_layout.stereo)
       Channel_layout.standard_layouts)
    "stereo is a standard layout";
  check (List.length Channel_layout.standard_layouts > 10) "standard layouts";
  raises is_not_found "find of an unknown name" (fun () ->
      Channel_layout.find "no such layout");
  raises is_not_found "get_default 0" (fun () -> Channel_layout.get_default 0);
  raises is_not_found "get_default of a count with no standard layout"
    (fun () -> Channel_layout.get_default 31)

let log_info = 32
let log_error = 16

let recorder () =
  let messages = ref [] in
  ( (fun message -> messages := message :: !messages),
    fun () -> List.rev !messages )

let requirement_1_13 () =
  let threads = 4 and count = 500 in
  let record, recorded = recorder () in
  Log.set_level `Debug;
  Log.set_callback record;
  log_from_threads threads count;
  Log.clear_callback ();
  let messages = recorded () in
  equal (threads * count) (List.length messages)
    "every message is delivered once";
  for thread = 0 to threads - 1 do
    let prefix = Printf.sprintf "thread %d " thread in
    equal
      (List.init count (Printf.sprintf "thread %d message %d\n" thread))
      (List.filter (String.starts_with ~prefix) messages)
      "the messages of a thread are in order"
  done;
  log_from_threads 1 3;
  Thread.delay 0.2;
  equal (threads * count)
    (List.length (recorded ()))
    "none after clear_callback";
  let record, recorded = recorder () in
  Log.set_level `Error;
  Log.set_callback record;
  log log_info "below the level\n";
  log log_error "at the level\n";
  Log.clear_callback ();
  equal ["at the level\n"] (recorded ())
    "a message above the log level is dropped"

let requirement_1_14 () =
  Log.set_level `Info;
  let record, recorded = recorder () in
  Log.set_callback record;
  for i = 1 to 5000 do
    log log_info (Printf.sprintf "held %d\n" i)
  done;
  Log.clear_callback ();
  equal
    (List.init 5000 (fun i -> Printf.sprintf "held %d\n" (i + 1)))
    (recorded ()) "messages logged with the runtime lock held are delivered";
  let record, recorded = recorder () in
  Log.set_callback record;
  log log_info "par";
  log log_info "tial\n";
  log log_info (String.make 5000 'x');
  Log.clear_callback ();
  (match recorded () with
    | ["par"; "tial\n"; long] ->
        check (String.length long < 1024) "a long message is truncated"
    | _ ->
        check false
          "set_callback right after clear_callback; one message per call");
  let calls = ref 0 in
  Log.set_callback (fun _ ->
      incr calls;
      failwith "a raising log callback");
  log log_info "one\n";
  log log_info "two\n";
  log log_info "three\n";
  Log.clear_callback ();
  equal 3 !calls "a callback that raises does not stop delivery";
  let first = ref [] and replaced = ref false in
  let record, recorded = recorder () in
  Log.set_callback (fun message ->
      first := message :: !first;
      Log.set_callback record;
      replaced := true);
  log log_info "before\n";
  wait_until (fun () -> !replaced);
  log log_info "after\n";
  Log.clear_callback ();
  equal ["before\n"] !first "a callback may replace itself";
  equal ["after\n"] (recorded ()) "the replacement receives what follows";
  let received = ref [] and queued = ref false in
  Log.set_callback (fun message ->
      if !received = [] then (
        wait_until (fun () -> !queued);
        Log.clear_callback ());
      received := message :: !received);
  log log_info "a\n";
  log log_info "b\n";
  log log_info "c\n";
  queued := true;
  wait_until (fun () -> List.length !received = 3);
  equal ["c\n"; "b\n"; "a\n"] !received
    "clear_callback from the callback still delivers what was queued";
  let record, recorded = recorder () in
  Log.set_callback record;
  log log_info "later\n";
  Log.clear_callback ();
  equal ["later\n"] (recorded ()) "a later callback gets later messages only"

let errors : error list =
  [
    `Bsf_not_found;
    `Decoder_not_found;
    `Demuxer_not_found;
    `Encoder_not_found;
    `Eof;
    `Exit;
    `Filter_not_found;
    `Invalid_data;
    `Muxer_not_found;
    `Option_not_found;
    `Patch_welcome;
    `Protocol_not_found;
    `Stream_not_found;
    `Bug;
    `Eagain;
    `Unknown;
    `Experimental;
  ]

let requirement_1_15 () =
  let codes = error_codes () in
  equal (List.length errors + 1) (Array.length codes) "one code per constructor";
  Array.iteri
    (fun i (code, text) ->
      let expected =
        Option.value (List.nth_opt errors i) ~default:(`Other code)
      in
      raises
        (function
          | Error e -> e = expected && string_of_error e = text | _ -> false)
        (Printf.sprintf "the error of code %d" code)
        (fun () -> raise_error code))
    codes;
  equal "a message" (string_of_error (`Failure "a message")) "a failure's text";
  equal
    ("Avutil.Error(" ^ string_of_error `Eof ^ ")")
    (Printexc.to_string (Error `Eof))
    "the printer of uncaught errors"

let interface () =
  check (version.major >= 59) "the libavutil version";
  equal
    (Printf.sprintf "%d.%d.%d" version.major version.minor version.micro)
    (version_string version) "version_string";
  equal 3. (expr_parse_and_eval "1+2") "expr_parse_and_eval";
  raises is_error "an invalid expression" (fun () -> expr_parse_and_eval "1+");
  equal 16 (Bigarray.Array1.dim (create_data 16)) "create_data";
  raises is_failure "a negative data length" (fun () -> create_data (-1));
  equal "3/4" (string_of_rational { num = 3; den = 4 }) "string_of_rational";
  equal { num = 1; den = 1000000 } (time_base ()) "time_base";
  equal 118 qp2lambda "qp2lambda";
  let header = Subtitle.header_ass_default () in
  let lines = String.split_on_char '\n' header in
  equal 14 (List.length lines) "the ASS header has thirteen lines";
  check
    (List.for_all
       (String.ends_with ~suffix:"\r")
       (List.filteri (fun i _ -> i < 13) lines))
    "each line ends with CR LF";
  equal "[Script Info]\r" (List.hd lines) "its first line";
  equal
    "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, \
     Effect, Text\r"
    (List.nth lines 12) "its last line"

let audio_frames () =
  let frame =
    Audio.create_frame `S16 Channel_layout.five_point_one 48000 1024
  in
  equal `S16 (Audio.frame_get_sample_format frame) "sample format";
  equal 48000 (Audio.frame_get_sample_rate frame) "sample rate";
  equal 6 (Audio.frame_get_channels frame) "channels";
  equal 1024 (Audio.frame_nb_samples frame) "sample count";
  raises is_failure "a sample count below 1" (fun () ->
      Audio.create_frame `S16 Channel_layout.stereo 48000 0);
  raises is_failure "a sample rate out of range" (fun () ->
      Audio.create_frame `S16 Channel_layout.stereo (1 lsl 40) 16);
  raises is_error "a frame with no sample format" (fun () ->
      Audio.create_frame `None Channel_layout.stereo 48000 16)

let video_properties () =
  let frame = Video.create_frame 322 243 `Yuv422p in
  equal 322 (Video.frame_get_width frame) "width";
  equal 243 (Video.frame_get_height frame) "height";
  equal `Yuv422p (Video.frame_get_pixel_format frame) "pixel format";
  equal None
    (Video.frame_get_pixel_aspect frame)
    "an unknown aspect ratio is absent";
  equal `Unspecified (Video.frame_get_color_space frame) "default colour space";
  set_video_properties frame;
  equal
    (Some { num = 4; den = 3 })
    (Video.frame_get_pixel_aspect frame)
    "aspect ratio";
  equal `Bt709 (Video.frame_get_color_space frame) "colour space";
  equal `Jpeg (Video.frame_get_color_range frame) "colour range";
  equal `Bt2020 (Video.frame_get_color_primaries frame) "colour primaries";
  equal `Smpte2084 (Video.frame_get_color_trc frame) "transfer characteristic";
  equal `Topleft (Video.frame_get_chroma_location frame) "chroma location";
  check (Video.frame_get_linesize frame 0 mod 32 = 0) "buffers aligned to 32";
  raises is_failure "a null frame" wrap_null_frame

let pixel_formats () =
  let descriptor = Pixel_format.descriptor `Yuv420p in
  equal "yuv420p" descriptor.name "descriptor name";
  equal 3 descriptor.nb_components "components";
  equal (1, 1)
    (descriptor.log2_chroma_w, descriptor.log2_chroma_h)
    "subsampling";
  equal [`Planar] descriptor.flags "flags";
  equal
    [
      { Pixel_format.plane = 0; step = 1; offset = 0; shift = 0; depth = 8 };
      { plane = 1; step = 1; offset = 0; shift = 0; depth = 8 };
      { plane = 2; step = 1; offset = 0; shift = 0; depth = 8 };
    ]
    descriptor.comp "components as FFmpeg describes them";
  equal None descriptor.alias "no alias";
  equal 12 (Pixel_format.bits descriptor) "bits per pixel";
  equal [`Be; `Rgb] (Pixel_format.descriptor `Rgb48be).flags
    "flags in ascending bit order";
  equal (Some "gray8,y8") (Pixel_format.descriptor `Gray8).alias "an alias";
  List.iter
    (fun (format, planes) ->
      equal planes (Pixel_format.planes format) "the plane count of a format")
    [
      (`Gray8, 1);
      (`Yuv420p, 3);
      (`Yuv422p, 3);
      (`Nv12, 2);
      (`Rgb24, 1);
      (`Pal8, 1);
      (`Yuva420p, 4);
    ];
  raises is_not_found "the descriptor of the none format" (fun () ->
      Pixel_format.descriptor `None);
  raises is_error "the planes of the none format" (fun () ->
      Pixel_format.planes `None)

let services () =
  equal
    [| `Unknown; `Video; `Audio; `Data; `Subtitle; `Attachment |]
    (media_types ()) "media types";
  equal
    [1; 1000; 1000000; 1000000000]
    (List.map time_format_units
       [`Second; `Millisecond; `Microsecond; `Nanosecond])
    "time format units";
  equal { num = -3; den = 7 }
    (rational_roundtrip { num = -3; den = 7 })
    "rationals";
  raises is_failure "a rational member out of range" (fun () ->
      rational_roundtrip { num = 1 lsl 40; den = 1 });
  let kinds = bigarray_kinds () in
  List.iteri
    (fun i formats ->
      List.iter
        (fun format -> equal kinds.(i) (bigarray_kind format) "a bigarray kind")
        formats)
    [
      [`U8; `U8p];
      [`S16; `S16p];
      [`S32; `S32p];
      [`S64; `S64p];
      [`Flt; `Fltp];
      [`Dbl; `Dblp];
    ];
  raises is_failure "a format with no bigarray kind" (fun () ->
      bigarray_kind `None)

let resident_megabytes () =
  let ic =
    try open_in "/proc/self/statm" with Sys_error _ -> skip "no /proc"
  in
  let pages =
    Scanf.sscanf (input_line ic) "%d %d" (fun _ resident -> resident)
  in
  close_in ic;
  pages * 4096 / 1048576

(* 4 GB of frames in all, each dropped at once: resident memory stays far
   below when the collector is told what a frame holds. The loop allocates
   nothing else on the OCaml heap, which would pace the collector by itself. *)
let native_memory () =
  let start = resident_megabytes () in
  for i = 1 to 3000 do
    touch_frame (Sys.opaque_identity (Video.create_frame 1280 720 `Yuv420p));
    if i mod 500 = 0 && resident_megabytes () - start > 1000 then
      failwith "dropped frames are not collected"
  done;
  check (resident_megabytes () - start <= 1000) "resident memory stays bounded"

(* Every failure path of every constructor, many times: a leak detector sees
   what one of them leaves behind. *)
let error_paths () =
  for _ = 1 to 200 do
    ignore (try Some (Video.create_frame 64 48 `None) with Error _ -> None);
    ignore
      (try Some (Audio.create_frame `None Channel_layout.stereo 48000 16)
       with Error _ -> None);
    ignore
      (try Some (Channel_layout.find "no such layout") with Not_found -> None);
    ignore (try Some (Channel_layout.get_default 31) with Not_found -> None);
    ignore
      (try
         Some
           (Subtitle.create_frame
              {
                text_content with
                rectangles =
                  text_content.rectangles
                  @ (bitmap_content (bitmap_planes ~pixels:"0" ())).rectangles;
              })
       with Error _ -> None);
    ignore
      (try Some (Options.get_int ~name:"missing" (option_object false))
       with Error _ -> None);
    ignore (Channel_layout.find "FR+FL");
    ignore
      (Options.get_channel_layout ~name:"layout_option" (option_object false))
  done;
  check true "the failure paths ran"

let lock_on_failure () =
  let stop = Atomic.make false and counter = Atomic.make 0 in
  let allocator =
    Thread.create
      (fun () ->
        while not (Atomic.get stop) do
          ignore (Sys.opaque_identity (Bytes.create 256));
          Atomic.incr counter;
          Thread.yield ()
        done)
      ()
  in
  let opts = Hashtbl.create 2 in
  Hashtbl.replace opts "an_option" (`String "a value");
  for _ = 1 to 50 do
    raises is_error "a device that does not exist" (fun () ->
        HwContext.create_device_context ~device:"/nonexistent/device" ~opts
          `Vaapi)
  done;
  equal
    [("an_option", `String "a value")]
    (List.of_seq (Hashtbl.to_seq opts))
    "the option table is untouched";
  let before = Atomic.get counter in
  wait_until (fun () -> Atomic.get counter > before);
  Atomic.set stop true;
  Thread.join allocator;
  check true "other threads run after the failures"

let c_thread () =
  let calls = ref 0 in
  call_from_c_thread (fun () -> incr calls);
  call_from_c_thread (fun () ->
      incr calls;
      ignore (Sys.opaque_identity (Video.create_frame 64 48 `Yuv420p));
      Gc.full_major ());
  equal 2 !calls "closures called from threads created in C"

let owner_kept_alive () =
  let finalized = finalized_option_owners () in
  let obj = Sys.opaque_identity (option_object false) in
  collect ();
  equal finalized
    (finalized_option_owners ())
    "an option object keeps its owner";
  equal (-4) (Options.get_int ~name:"int_option" obj) "and reads it";
  ignore (Sys.opaque_identity (ref obj))

let dependent_handles () =
  let finalized = finalized_option_owners () in
  owner_kept_alive ();
  collect ();
  equal (finalized + 1)
    (finalized_option_owners ())
    "the owner goes with its object"

let requirements =
  [
    ("1.1", requirement_1_1);
    ("1.2", requirement_1_2);
    ("1.3", requirement_1_3);
    ("1.4", requirement_1_4);
    ("1.5", requirement_1_5);
    ("1.6", requirement_1_6);
    ("1.7", requirement_1_7);
    ("1.8", requirement_1_8);
    ("1.9", requirement_1_9);
    ("1.10", requirement_1_10);
    ("1.11", requirement_1_11);
    ("1.12", requirement_1_12);
    ("1.13", requirement_1_13);
    ("1.14", requirement_1_14);
    ("1.15", requirement_1_15);
    ("avutil.interface", interface);
    ("avutil.audio-frames", audio_frames);
    ("avutil.video-properties", video_properties);
    ("avutil.pixel-formats", pixel_formats);
    ("avutil.services", services);
    ("kc.native-memory", native_memory);
    ("kc.error-paths", error_paths);
    ("kc.lock-on-failure", lock_on_failure);
    ("kc.c-thread", c_thread);
    ("kc.dependent-handles", dependent_handles);
    ( "control.asan-overflow",
      fun () ->
        control_overflow 320;
        check true "ran" );
    ( "control.lsan-leak",
      fun () ->
        control_leak ();
        check true "ran" );
    ( "control.gc-unrooted",
      fun () ->
        equal
          ("a value held across an allocation", 0)
          (control_unrooted ()) "a value held across an allocation" );
  ]
