(* Conformance of side data: spec/tests.md §13. It spans avutil, avcodec, av
   and avfilter, so it has a program section of its own. *)

open Avutil
open Harness
module Packet = Avcodec.Packet
module Packet_side_data = Avcodec.Packet_side_data

let is_failure = function Error (`Failure _) -> true | _ -> false

let close ?(within = 1e-3) expected actual =
  Float.abs (expected -. actual) < within

let rotation_of matrix =
  match Display_matrix.rotation matrix with
    | Some angle -> angle
    | None -> failwith "singular matrix"

(* Clockwise quarter turn: what a phone held upright records. *)
let clockwise = Display_matrix.make (-90.)
let frame_matrix matrix = Frame_side_data.encode (`Display_matrix matrix)
let packet_matrix matrix = Packet_side_data.encode (`Display_matrix matrix)

let requirement_13_1 () =
  List.iter
    (fun angle ->
      check
        (close angle (rotation_of (Display_matrix.make angle)))
        (Printf.sprintf "rotation (make %g)" angle))
    [0.; 90.; -90.; 33.5; -179.];
  check
    (close 180. (Float.abs (rotation_of (Display_matrix.make 180.))))
    "a half turn is 180 or -180";
  raises is_failure "a matrix of 8 elements" (fun () ->
      Display_matrix.of_array (Array.make 8 0l));
  raises is_failure "a rotation that is not finite" (fun () ->
      Display_matrix.make Float.nan);
  let singular = Display_matrix.of_array (Array.make 9 0l) in
  equal None (Display_matrix.rotation singular) "a singular matrix";
  equal [] (Display_matrix.transforms singular) "nothing to do when singular";
  equal (Some clockwise)
    (Display_matrix.of_payload (Display_matrix.to_payload clockwise))
    "the payload round-trips";
  equal None (Display_matrix.of_payload "short") "a payload too short"

let requirement_13_2 () =
  equal
    (Some (`Display_matrix clockwise))
    (Frame_side_data.decode (frame_matrix clockwise))
    "frame display matrix";
  List.iter
    (fun (name, content) ->
      equal (Some content)
        (Packet_side_data.decode (Packet_side_data.encode content))
        name)
    [
      ( "replaygain",
        `Replaygain
          {
            Packet_side_data.track_gain = -3;
            track_peak = 4;
            album_gain = Int32.to_int Int32.min_int;
            album_peak = 0xFFFFFFFF;
          } );
      ("strings", `Strings_metadata [("title", "a"); ("artist", "b c")]);
      ("update", `Metadata_update []);
      ("display matrix", `Display_matrix clockwise);
      ( "cropping",
        `Frame_cropping { top = 1; bottom = 2; left = 3; right = 0xFFFFFFFF } );
    ];
  equal
    (Some (`Metadata_update [("title", "b")]))
    (Packet_side_data.decode
       (Packet_side_data.encode
          (`Metadata_update [("Title", "a"); ("title", "b")])))
    "a later pair replaces one of the same key, whatever its case";
  raises is_failure "a cropping beyond 32 unsigned bits" (fun () ->
      Packet_side_data.encode
        (`Frame_cropping { top = -1; bottom = 0; left = 0; right = 0 }));
  let entries = [packet_matrix clockwise] in
  equal (Some clockwise)
    (Packet_side_data.display_matrix entries)
    "the matrix of a list";
  equal None (Packet_side_data.cropping entries) "no cropping in the list";
  check (Packet_side_data.name `Displaymatrix <> "") "a packet kind has a name";
  check (Frame_side_data.name `Displaymatrix <> "") "a frame kind has a name";
  check
    (List.mem `Global (Frame_side_data.props `Displaymatrix))
    "the display matrix applies to a whole stream"

let requirement_13_3 () =
  let frame = Video.create_frame 16 8 `Yuv420p in
  equal [] (Frame.side_data frame) "a new frame has no side data";
  Frame.add_side_data frame (frame_matrix clockwise);
  equal [frame_matrix clockwise] (Frame.side_data frame) "added";
  let upside_down = Display_matrix.make 180. in
  Frame.add_side_data frame (frame_matrix upside_down);
  equal [frame_matrix upside_down] (Frame.side_data frame) "a kind is replaced";
  let copy = Frame.dup frame in
  Frame.remove_side_data frame `Displaymatrix;
  Frame.remove_side_data frame `Displaymatrix;
  equal [] (Frame.side_data frame) "removed";
  equal (Some upside_down)
    (Frame_side_data.display_matrix (Frame.side_data copy))
    "dup copies the side data and shares none of it"

(* The parameters of an mpeg4 encoder hold an entry the bindings do not
   translate. *)
let untyped_entry () =
  let encoder =
    Avcodec.Video.create_encoder ~pixel_format:`Yuv420p ~width:160 ~height:120
      ~time_base:Test_av.video_time_base
      (Avcodec.Video.find_encoder_by_name "mpeg4")
  in
  match
    List.find_opt
      (fun entry -> Packet_side_data.decode entry = None)
      (Avcodec.params_side_data (Avcodec.params encoder))
  with
    | Some entry -> entry
    | None -> skip "the mpeg4 encoder gives no untyped side data"

let requirement_13_4 () =
  let packet : video Packet.t = Packet.create "x" in
  let entry = packet_matrix clockwise in
  Packet.add_raw_side_data packet entry;
  equal [entry] (Packet.raw_side_data packet) "added";
  equal
    [`Display_matrix clockwise]
    (Packet.side_data packet) "the typed view decodes the raw one";
  Packet.add_raw_side_data packet (packet_matrix (Display_matrix.make 180.));
  equal 1 (List.length (Packet.raw_side_data packet)) "a kind is carried once";
  let untyped = untyped_entry () in
  Packet.add_raw_side_data packet untyped;
  check
    (List.mem untyped (Packet.raw_side_data packet))
    "an untyped entry keeps its bytes";
  equal 1 (List.length (Packet.side_data packet)) "and has no typed view";
  Packet.remove_side_data packet `Displaymatrix;
  equal [untyped] (Packet.raw_side_data packet) "removed"

let video_params path =
  let input = Av.open_input path in
  let _, _, params = Av.find_best_video_stream input in
  Av.close input;
  params

let stream_rotation path =
  Option.map rotation_of
    (Packet_side_data.display_matrix
       (Avcodec.params_side_data (video_params path)))

(* Copies the video stream of [source] with [params]. *)
let remux ~params source name =
  let path = Test_av.file name in
  let input = Av.open_input source in
  let _, stream, _ = Av.find_best_video_stream input in
  let output = Av.open_output path in
  let copy = Av.new_stream_copy ~params output in
  let rec write () =
    match Av.read_input ~video_packet:[stream] input with
      | `Video_packet (_, packet) ->
          Av.write_packet copy (Av.get_time_base stream) packet;
          write ()
      | _ -> write ()
      | exception Error `Eof -> ()
  in
  write ();
  Av.close output;
  Av.close input;
  path

let rotated_video name =
  let source = Test_av.make_video ~bindings:[] "upright.mp4" in
  let params = video_params source in
  remux source name
    ~params:
      (Avcodec.params_with_side_data params
         (packet_matrix clockwise :: Avcodec.params_side_data params))

let requirement_13_5 () =
  let source = Test_av.make_video ~bindings:[] "upright.mp4" in
  let params = video_params source in
  let before = Avcodec.params_side_data params in
  let entry = packet_matrix clockwise in
  let rotated =
    Avcodec.params_with_side_data params (entry :: entry :: before)
  in
  equal before (Avcodec.params_side_data params) "the argument is unchanged";
  equal (entry :: before)
    (Avcodec.params_side_data rotated)
    "the list, a kind kept once";
  equal None (stream_rotation source) "no rotation to start with";
  check
    (close (-90.)
       (Option.get
          (stream_rotation (remux ~params:rotated source "rotated.mp4"))))
    "the rotation is read back from the written file";
  equal []
    (Avcodec.params_side_data (Avcodec.params_with_side_data rotated []))
    "an empty list removes everything"

let requirement_13_6 () =
  let side_data = [frame_matrix clockwise] in
  let encoder =
    Avcodec.Video.create_encoder ~side_data ~pixel_format:`Yuv420p ~width:160
      ~height:120 ~time_base:Test_av.video_time_base
      (Avcodec.Video.find_encoder_by_name "mpeg4")
  in
  equal (Some clockwise)
    (Packet_side_data.display_matrix
       (Avcodec.params_side_data (Avcodec.params encoder)))
    "the encoder's parameters hold the matrix";
  let path = Test_av.file "encoded.mp4" in
  let output = Av.open_output path in
  let stream =
    Av.new_video_stream ~side_data ~frame_rate:Test_av.frame_rate
      ~pixel_format:`Yuv420p ~width:160 ~height:120
      ~time_base:Test_av.video_time_base
      ~codec:(Avcodec.Video.find_encoder_by_name "mpeg4")
      output
  in
  for index = 0 to 9 do
    Av.write_frame stream (Test_av.picture index)
  done;
  Av.close output;
  check
    (close (-90.) (Option.get (stream_rotation path)))
    "the stream of the written file holds the rotation"

let decoded_frames path =
  let input = Av.open_input path in
  let _, stream, _ = Av.find_best_video_stream input in
  let rec read frames =
    match Av.read_input ~video_frame:[stream] input with
      | `Video_frame (_, frame) -> read (frame :: frames)
      | _ -> read frames
      | exception Error `Eof -> List.rev frames
  in
  let frames = read [] in
  Av.close input;
  frames

let requirement_13_7 () =
  let frames = decoded_frames (rotated_video "to-decode.mp4") in
  check (frames <> []) "frames were decoded";
  List.iter
    (fun frame ->
      match Frame_side_data.display_matrix (Frame.side_data frame) with
        | Some matrix ->
            check (close (-90.) (rotation_of matrix)) "the frame's rotation"
        | None -> check false "a decoded frame has no display matrix")
    frames

let flipped ?hflip ?vflip angle =
  Display_matrix.transforms (Display_matrix.make ?hflip ?vflip angle)

let requirement_13_8 () =
  equal [] (flipped 0.) "identity";
  equal [`Transpose `Clock] (flipped (-90.)) "a quarter turn clockwise";
  equal [`Transpose `Cclock] (flipped 90.) "a quarter turn counter-clockwise";
  equal [`Hflip; `Vflip] (flipped 180.) "a half turn";
  equal [`Vflip] (flipped ~vflip:true 0.) "a vertical flip";
  equal [`Rotate 33.] (flipped (-33.)) "any other angle";
  let open Avfilter.Utils in
  equal
    ("transpose", [`Pair ("dir", `String "clock")])
    (filter_of_transform (`Transpose `Clock))
    "transpose";
  equal ("hflip", []) (filter_of_transform `Hflip) "hflip";
  equal ("vflip", []) (filter_of_transform `Vflip) "vflip";
  equal "rotate" (fst (filter_of_transform (`Rotate 33.))) "rotate";
  let cropping = { top = 2; bottom = 4; left = 6; right = 8 } in
  equal
    (Some
       ( "crop",
         [
           `Pair ("w", `String "iw-6-8");
           `Pair ("h", `String "ih-2-4");
           `Pair ("x", `Int 6);
           `Pair ("y", `Int 2);
         ] ))
    (filter_of_cropping cropping)
    "crop";
  equal None
    (filter_of_cropping { top = 0; bottom = 0; left = 0; right = 0 })
    "nothing to crop";
  let layout ?cropping ?display_matrix () =
    display_layout ?cropping ?display_matrix ~pixel_aspect:{ num = 4; den = 3 }
      ~width:320 ~height:240 ()
  in
  equal
    (`Layout
       {
         width = 320;
         height = 240;
         pixel_aspect = Some { num = 4; den = 3 };
         filters = [];
       })
    (layout ()) "a picture with nothing to apply";
  equal
    (`Layout
       {
         width = 234;
         height = 306;
         pixel_aspect = Some { num = 3; den = 4 };
         filters =
           Option.to_list (filter_of_cropping cropping)
           @ [filter_of_transform (`Transpose `Clock)];
       })
    (layout ~cropping ~display_matrix:clockwise ())
    "cropped, then turned: the size and the pixel aspect follow";
  equal
    (`Undecided (`Odd_rotation 33.))
    (layout ~display_matrix:(Display_matrix.make (-33.)) ())
    "an odd rotation is left to the caller";
  let too_wide = { top = 0; bottom = 0; left = 200; right = 120 } in
  equal
    (`Undecided (`Invalid_cropping too_wide))
    (layout ~cropping:too_wide ())
    "a cropping that leaves no picture is left to the caller"

let width = 4
let height = 2

(* A gray picture whose pixel at (x, y) is [offset + (y * width) + x]. *)
let gray ?matrix ?(offset = 0) index =
  let frame = Video.create_frame width height `Gray8 in
  ignore
    (Video.frame_visit ~make_writable:true
       (fun planes ->
         let data, stride = planes.(0) in
         for y = 0 to height - 1 do
           for x = 0 to width - 1 do
             data.{(y * stride) + x} <- offset + (y * width) + x
           done
         done)
       frame);
  Frame.set_pts frame (Some (Int64.of_int index));
  Option.iter (fun m -> Frame.add_side_data frame (frame_matrix m)) matrix;
  frame

let pixel frame x y =
  let value = ref 0 in
  ignore
    (Video.frame_visit ~make_writable:false
       (fun planes ->
         let data, stride = planes.(0) in
         value := data.{(y * stride) + x})
       frame);
  !value

let size frame = (Video.frame_get_width frame, Video.frame_get_height frame)

let converted ?on_undecided inputs =
  let converter =
    Avfilter.Utils.init_display_converter ?on_undecided
      ~time_base:Test_av.video_time_base ()
  in
  let frames = ref [] in
  let deliver frame = frames := frame :: !frames in
  List.iter
    (fun frame ->
      Avfilter.Utils.convert_display converter deliver (`Frame frame))
    inputs;
  Avfilter.Utils.convert_display converter deliver `Flush;
  List.rev !frames

let requirement_13_9 () =
  let input = gray ~matrix:clockwise 7 in
  let output =
    match converted [input] with
      | [frame] -> frame
      | frames -> failwith (Printf.sprintf "%d frames" (List.length frames))
  in
  equal (height, width) (size output) "the picture is turned";
  equal [] (Frame.side_data output) "the delivered frame has no matrix";
  equal (Some 7L) (Frame.pts output) "the timestamp is kept";
  equal
    [frame_matrix clockwise]
    (Frame.side_data input) "the input is unchanged";
  for y = 0 to width - 1 do
    for x = 0 to height - 1 do
      equal
        (pixel input y (height - 1 - x))
        (pixel output x y)
        (Printf.sprintf "pixel (%d, %d) of the clockwise turn" x y)
    done
  done;
  let plain = gray 0 in
  let passed = List.hd (converted [plain]) in
  equal (width, height) (size passed) "a frame with no matrix keeps its size";
  ignore
    (Video.frame_visit ~make_writable:false
       (fun planes -> (fst planes.(0)).{0} <- 200)
       passed);
  equal 200 (pixel plain 0 0) "and shares its data with the input";
  check (passed == plain) "a frame with nothing to remove is not cloned";
  let identity = gray ~matrix:(Display_matrix.make 0.) 0 in
  let upright = List.hd (converted [identity]) in
  equal [] (Frame.side_data upright) "an identity matrix is removed";
  equal 1 (List.length (Frame.side_data identity)) "from a copy of the frame";
  let odd = Display_matrix.make (-33.) in
  let asked = ref [] in
  let undecided answer =
    converted
      ~on_undecided:(fun question ->
        asked := question :: !asked;
        answer)
      [gray ~matrix:odd 0; gray ~matrix:odd 1]
  in
  let left = undecided None in
  equal [`Odd_rotation 33.] !asked "the caller is asked once for the run";
  equal [Some odd; Some odd]
    (List.map
       (fun f -> Frame_side_data.display_matrix (Frame.side_data f))
       left)
    "no decision leaves the frames as they came, matrix included";
  let flipped = undecided (Some [("hflip", [])]) in
  equal [[]; []]
    (List.map Frame.side_data flipped)
    "a decision is applied and the matrix removed";
  equal
    (pixel (gray 0) (width - 1) 0)
    (pixel (List.hd flipped) 0 0)
    "through the filters the caller gave";
  let sizes =
    List.map size
      (converted
         [gray ~matrix:clockwise 0; gray 1; gray ~matrix:clockwise ~offset:8 2])
  in
  equal
    [(height, width); (width, height); (height, width)]
    sizes "a change of matrix delivers every frame, in order"

let requirement_13_10 () =
  let unbuildable =
    converted
      ~on_undecided:(fun _ -> Some [("no_such_filter", [])])
      [gray ~matrix:(Display_matrix.make (-33.)) 0]
  in
  equal [1]
    (List.map (fun f -> List.length (Frame.side_data f)) unbuildable)
    "filters that cannot be built leave the frame as it came";
  let format = Video.frame_format (gray 0) in
  equal (width, height) (format.width, format.height) "the size of the frame";
  equal `Gray8 format.pixel_format "its pixel format";
  check
    (Video.same_frame_format format (Video.frame_format (gray 1)))
    "two frames made alike have the same format";
  check
    (not
       (Video.same_frame_format format
          (Video.frame_format (Video.create_frame height width `Gray8))))
    "another size is another format";
  let wide = { format with pixel_aspect = Some { num = 4; den = 3 } } in
  check
    (not (Video.same_frame_format format wide))
    "another pixel aspect is another format";
  check
    (Video.same_frame_format ~ignore:[`Pixel_aspect] format wide)
    "unless the caller leaves the pixel aspect out";
  equal
    [
      `Pair ("video_size", `String (Printf.sprintf "%dx%d" width height));
      `Pair ("pix_fmt", `Int (Pixel_format.get_id `Gray8));
      `Pair ("time_base", `Rational Test_av.video_time_base);
      `Pair ("pixel_aspect", `Rational { num = 4; den = 3 });
    ]
    (Avfilter.Utils.video_buffer_args ~time_base:Test_av.video_time_base wide)
    "the buffer arguments of a format with no colour information";
  let audio rate =
    Audio.frame_format (Audio.create_frame `S16 Channel_layout.stereo rate 16)
  in
  check
    (Audio.same_frame_format (audio 44100) (audio 44100))
    "two audio frames made alike have the same format";
  check
    (not (Audio.same_frame_format (audio 44100) (audio 48000)))
    "another rate is another format"

let requirements =
  [
    ("13.1", requirement_13_1);
    ("13.2", requirement_13_2);
    ("13.3", requirement_13_3);
    ("13.4", requirement_13_4);
    ("13.5", requirement_13_5);
    ("13.6", requirement_13_6);
    ("13.7", requirement_13_7);
    ("13.8", requirement_13_8);
    ("13.9", requirement_13_9);
    ("13.10", requirement_13_10);
  ]
