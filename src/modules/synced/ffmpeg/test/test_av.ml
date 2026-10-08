(* Conformance of av: spec/tests.md §3, the requirements of §1 that need a
   container, and the entries of spec/known-complexity.md that av answers
   ([kc.*]). Every fixture is synthesised by the requirement that uses it,
   in a directory of its own. *)

open Avutil
open Harness

let is_error = function Error _ -> true | _ -> false
let is_failure = function Error (`Failure _) -> true | _ -> false
let is_eof = function Error `Eof -> true | _ -> false

let is_closed = function
  | Error (`Failure "Container closed!") -> true
  | _ -> false

let opts bindings = Hashtbl.of_seq (List.to_seq bindings)
let keys table = List.sort compare (List.of_seq (Hashtbl.to_seq_keys table))

let directory =
  lazy
    (let path = Filename.temp_file "ffmpeg-test" "" in
     Sys.remove path;
     Sys.mkdir path 0o700;
     at_exit (fun () ->
         Array.iter
           (fun file -> Sys.remove (Filename.concat path file))
           (Sys.readdir path);
         Sys.rmdir path);
     path)

let file name = Filename.concat (Lazy.force directory) name
let read_file path = In_channel.with_open_bin path In_channel.input_all
let frame_rate = { num = 25; den = 1 }
let video_time_base = { num = 1; den = 25 }

(* [noise] makes a picture that does not compress. *)
let picture ?(width = 160) ?(height = 120) ?(noise = false)
    ?(pixel_format = `Yuv420p) index =
  let frame = Video.create_frame width height pixel_format in
  let sample plane i =
    if noise then ((i + index) * 2654435761) lsr 11
    else i + (index * 7) + (plane * 31)
  in
  ignore
    (Video.frame_visit ~make_writable:true
       (Array.iteri (fun plane (data, _) ->
            for i = 0 to Bigarray.Array1.dim data - 1 do
              data.{i} <- sample plane i land 0xff
            done))
       frame);
  Frame.set_pts frame (Some (Int64.of_int index));
  frame

external zero_frame : _ frame -> unit = "test_zero_frame"

let silence index =
  let frame = Audio.create_frame `Fltp Channel_layout.stereo 44100 1024 in
  zero_frame frame;
  Frame.set_pts frame (Some (Int64.of_int (index * 1024)));
  frame

(* A video stream with B-frames: its decoder holds frames. *)
let video_stream ?(codec = "mpeg4") ?(bindings = [("bf", `Int 2)])
    ?(width = 160) ?(height = 120) ?(pixel_format = `Yuv420p) output =
  Av.new_video_stream ~opts:(opts bindings) ~frame_rate ~pixel_format ~width
    ~height ~time_base:video_time_base
    ~codec:(Avcodec.Video.find_encoder_by_name codec)
    output

let audio_stream ?(codec = "aac") ?(sample_format = `Fltp) output =
  Av.new_audio_stream ~channel_layout:Channel_layout.stereo ~sample_rate:44100
    ~sample_format ~time_base:{ num = 1; den = 44100 }
    ~codec:(Avcodec.Audio.find_encoder_by_name codec)
    output

(* A video of [frames] frames at 25 frames per second. *)
let make_video ?(frames = 30) ?format ?codec ?bindings ?pixel_format name =
  let path = file name in
  let output = Av.open_output ?format path in
  let stream = video_stream ?codec ?bindings ?pixel_format output in
  for index = 0 to frames - 1 do
    Av.write_frame stream (picture ?pixel_format index)
  done;
  Av.close output;
  path

let cue start_ms end_ms text : Subtitle.frame =
  Subtitle.create_frame
    {
      format = 1;
      start_display_time = 0;
      end_display_time = end_ms - start_ms;
      pts = Some (Int64.of_int (start_ms * 1000));
      rectangles =
        [
          {
            pict = None;
            flags = [];
            rect_type = `Ass;
            text = "";
            ass = Printf.sprintf "0,0,Default,,0,0,0,,%s" text;
          };
        ];
    }

(* One audio, one video and one text subtitle stream. *)
let make_movie name =
  let path = file name in
  let output = Av.open_output path in
  let video = video_stream output in
  let audio = audio_stream output in
  let subtitle =
    Av.new_subtitle_stream ~time_base:{ num = 1; den = 1000 }
      ~codec:(Avcodec.Subtitle.find_encoder_by_name "ass")
      output
  in
  for index = 0 to 49 do
    Av.write_frame video (picture index);
    Av.write_frame audio (silence (2 * index));
    Av.write_frame audio (silence ((2 * index) + 1));
    if index mod 20 = 0 then
      Av.write_subtitle_frame subtitle
        (cue (index * 40) ((index * 40) + 500) (Printf.sprintf "cue %d" index))
  done;
  Av.close output;
  path

let video_frames ?(streams = fun input -> Av.get_video_streams input) input =
  let video_frame = List.map (fun (_, stream, _) -> stream) (streams input) in
  let rec count frames =
    match Av.read_input ~video_frame input with
      | `Video_frame (_, frame) -> count (Frame.pts frame :: frames)
      | _ -> count frames
      | exception Error `Eof -> List.rev frames
  in
  count []

let closed_operations description operations =
  List.iter
    (fun (name, operation) ->
      raises is_closed
        (description ^ ": " ^ name)
        (fun () -> ignore (operation ())))
    operations

let requirement_3_1 () =
  let input = Av.open_input (make_movie "movie.mkv") in
  let _, video, _ = List.hd (Av.get_video_streams input) in
  let _, audio, _ = Av.find_best_audio_stream input in
  let obj = Av.input_obj input in
  Av.close input;
  Av.close input;
  closed_operations "a closed input"
    [
      ("get_input_duration", fun () -> ignore (Av.get_input_duration input));
      ("get_input_metadata", fun () -> ignore (Av.get_input_metadata input));
      ("get_input_format", fun () -> ignore (Av.get_input_format input));
      ("input_obj", fun () -> ignore (Av.input_obj input));
      ( "an option read",
        fun () -> ignore (Options.get_int ~name:"probesize" obj) );
      ("get_audio_streams", fun () -> ignore (Av.get_audio_streams input));
      ("get_video_streams", fun () -> ignore (Av.get_video_streams input));
      ("get_subtitle_streams", fun () -> ignore (Av.get_subtitle_streams input));
      ("get_data_streams", fun () -> ignore (Av.get_data_streams input));
      ( "find_best_audio_stream",
        fun () -> ignore (Av.find_best_audio_stream input) );
      ( "find_best_video_stream",
        fun () -> ignore (Av.find_best_video_stream input) );
      ( "find_best_subtitle_stream",
        fun () -> ignore (Av.find_best_subtitle_stream input) );
      ("get_codec_params", fun () -> ignore (Av.get_codec_params video));
      ("get_avg_frame_rate", fun () -> ignore (Av.get_avg_frame_rate video));
      ("set_avg_frame_rate", fun () -> Av.set_avg_frame_rate video None);
      ("get_time_base", fun () -> ignore (Av.get_time_base audio));
      ("set_time_base", fun () -> Av.set_time_base audio video_time_base);
      ("get_pixel_aspect", fun () -> ignore (Av.get_pixel_aspect video));
      ("get_duration", fun () -> ignore (Av.get_duration audio));
      ("get_metadata", fun () -> ignore (Av.get_metadata video));
      ("set_metadata", fun () -> Av.set_metadata video []);
      ("codec_attr", fun () -> ignore (Av.codec_attr audio));
      ("bitrate", fun () -> ignore (Av.bitrate audio));
      ("read_input", fun () -> ignore (Av.read_input input));
      ("seek", fun () -> Av.seek ~fmt:`Second ~ts:0L input);
      ("tell", fun () -> ignore (Av.tell input));
    ];
  let output = Av.open_output (file "closed.mkv") in
  let video = video_stream output in
  let audio = audio_stream output in
  let copy = Av.new_uninitialized_stream_copy output in
  let parameters = Av.get_codec_params audio in
  Av.close output;
  Av.close output;
  let packet : audio Avcodec.Packet.t = Avcodec.Packet.create "x" in
  closed_operations "a closed output"
    [
      ("output_started", fun () -> ignore (Av.output_started output));
      ("set_output_metadata", fun () -> Av.set_output_metadata output []);
      ( "new_stream_copy",
        fun () -> ignore (Av.new_stream_copy ~params:parameters output) );
      ( "new_uninitialized_stream_copy",
        fun () -> ignore (Av.new_uninitialized_stream_copy output) );
      ( "initialize_stream_copy",
        fun () -> ignore (Av.initialize_stream_copy ~params:parameters copy) );
      ("new_audio_stream", fun () -> ignore (audio_stream output));
      ("new_video_stream", fun () -> ignore (video_stream output));
      ( "new_subtitle_stream",
        fun () ->
          ignore
            (Av.new_subtitle_stream ~time_base:video_time_base
               ~codec:(Avcodec.Subtitle.find_encoder_by_name "ass")
               output) );
      ( "new_data_stream",
        fun () ->
          ignore
            (Av.new_data_stream ~time_base:video_time_base ~codec:`None output)
      );
      ("get_frame_size", fun () -> ignore (Av.get_frame_size audio));
      ("write_frame", fun () -> Av.write_frame video (picture 0));
      ( "write_packet",
        fun () ->
          let _, stream, _ = List.hd (Av.get_audio_streams output) in
          Av.write_packet stream video_time_base packet );
      ("flush", fun () -> Av.flush output);
      ("tell", fun () -> ignore (Av.tell output));
      ("get_audio_streams", fun () -> ignore (Av.get_audio_streams output));
    ];
  equal 1 (Av.get_index audio) "get_index reads the stream value only";
  check (Av.get_output audio == output) "get_output too"

let requirement_3_4 () =
  let output = Av.open_output (file "empty.mkv") in
  ignore (video_stream output);
  ignore (audio_stream output);
  check (not (Av.output_started output)) "the header is not written";
  Av.flush output;
  Av.close output;
  let output = Av.open_output (file "no-stream.mkv") in
  Av.flush output;
  Av.close output;
  check true "an output with nothing written flushes and closes"

let requirement_3_6 () =
  let input = Av.open_input (make_video "reordered.mkv") in
  let expected = List.init 30 (fun i -> Some (Int64.of_int (i * 40))) in
  equal expected (video_frames input) "every frame before the end of the input";
  raises is_eof "the end of the input repeats" (fun () -> Av.read_input input);
  Av.seek ~fmt:`Second ~ts:0L input;
  equal expected (video_frames input)
    "the same frames after a seek to the start";
  Av.close input;
  let input = Av.open_input (make_video ~frames:1 "image.mkv") in
  equal 1
    (List.length (video_frames input))
    "a one-frame video yields its frame";
  let input = Av.open_input (make_video ~frames:5 "packets.mkv") in
  raises is_eof "empty selections consume the input" (fun () ->
      Av.read_input input)

let requirement_3_7 () =
  let input = Av.open_input (make_video ~frames:250 "long.mkv") in
  let _, stream, _ = List.hd (Av.get_video_streams input) in
  for _ = 1 to 3 do
    ignore (Av.read_input ~video_frame:[stream] input)
  done;
  Av.seek ~fmt:`Millisecond ~ts:6000L input;
  match Av.read_input ~video_frame:[stream] input with
    | `Video_frame (_, frame) ->
        let pts = Option.get (Frame.pts frame) in
        check
          (pts >= 5000L && pts <= 6100L)
          (Printf.sprintf
             "the first frame after a seek is at the target, not %Ld" pts)
    | _ -> check false "a video frame"

let write_file path content =
  Out_channel.with_open_bin path (fun oc -> output_string oc content)

let requirement_3_2 () =
  let movie = make_movie "movie.mkv" in
  let garbage = file "garbage.bin" in
  write_file garbage (String.make 65536 '\x00');
  let interrupt () = false in
  let matroska = Option.get (Av.Format.find_input_format "matroska") in
  let descriptors () =
    if Sys.file_exists "/proc/self/fd" then
      Array.length (Sys.readdir "/proc/self/fd")
    else 0
  in
  let opened = descriptors () in
  for _ = 1 to 10 do
    raises is_error "an unreachable URL" (fun () ->
        Av.open_input ~interrupt "/nonexistent/input.mkv");
    raises is_error "a forced format the data does not have" (fun () ->
        Av.open_input ~interrupt ~format:matroska garbage);
    raises is_error "a rejected option" (fun () ->
        Av.open_input ~interrupt
          ~opts:(opts [("probesize", `String "junk")])
          movie);
    raises is_error "a failed probe" (fun () ->
        Av.open_input ~interrupt garbage);
    raises
      (fun exn -> exn = Exit)
      "a raising configure function"
      (fun () ->
        Av.open_input ~interrupt
          ~configure_video_stream:(fun _ -> raise Exit)
          movie);
    raises is_failure "neither a URL nor a format" (fun () ->
        Av.open_input ~interrupt "");
    raises is_error "a custom input with no data" (fun () ->
        Av.open_input_stream ~seek:(fun _ _ -> 0) (fun _ _ _ -> 0));
    raises is_error "a custom input whose read raises" (fun () ->
        Av.open_input_stream (fun _ _ _ -> raise Exit));
    raises is_error "an output of no known format" (fun () ->
        Av.open_output ~interrupt (file "output.no-such-format"));
    raises is_error "an output that cannot be created" (fun () ->
        Av.open_output ~interrupt "/nonexistent/output.mkv");
    raises is_error "an output option FFmpeg rejects" (fun () ->
        Av.open_output ~interrupt
          ~opts:(opts [("fflags", `String "junk")])
          (file "o.mkv"));
    raises is_failure "a file format with no file" (fun () ->
        Av.open_output_format
          (Option.get (Av.Format.guess_output_format ~short_name:"matroska" ())))
  done;
  equal opened (descriptors ())
    "a failed open releases what it opened before it raises";
  collect ()

let requirement_3_3 () =
  let path = file "usable.mkv" in
  let output = Av.open_output path in
  raises is_error "an unsupported parameter" (fun () ->
      audio_stream ~sample_format:`S16 output);
  let table = opts [("bf", `String "junk")] in
  raises is_error "a rejected option" (fun () ->
      Av.new_video_stream ~opts:table ~pixel_format:`Yuv420p ~width:160
        ~height:120 ~time_base:video_time_base
        ~codec:(Avcodec.Video.find_encoder_by_name "mpeg4")
        output);
  equal ["bf"] (keys table) "a failed creation leaves the option table";
  equal [] (Av.get_video_streams output) "and adds no stream";
  let stream = video_stream output in
  equal 0 (Av.get_index stream) "the next stream is the first";
  for index = 0 to 4 do
    Av.write_frame stream (picture index)
  done;
  Av.close output;
  equal 5
    (List.length (video_frames (Av.open_input path)))
    "the output is usable"

let requirement_3_5 () =
  let failing = ref false in
  let write _ _ length = if !failing then -5 else length in
  let format =
    Option.get (Av.Format.guess_output_format ~short_name:"mpegts" ())
  in
  let output = Av.open_output_stream write format in
  let stream = video_stream output in
  for index = 0 to 9 do
    Av.write_frame stream (picture index)
  done;
  failing := true;
  raises is_error "a close whose final write fails" (fun () -> Av.close output);
  Av.close output;
  raises is_closed "the container is closed" (fun () ->
      Av.output_started output)

let mp2_silence ~start index =
  let frame = Audio.create_frame `S16 Channel_layout.stereo 44100 1152 in
  zero_frame frame;
  Frame.set_pts frame (Some (Int64.of_int (start + (index * 1152))));
  frame

(* Three MPEG program streams end to end, the middle one alone with audio:
   the demuxer has no header and meets the audio stream long after the
   probe, which reads the start of the file and its end. *)
let make_late_streams name =
  let video_only = file "video-only.mpg"
  and with_audio = file "with-audio.mpg" in
  let write path ~audio =
    let output = Av.open_output path in
    let video =
      video_stream ~codec:"mpeg2video"
        ~bindings:[("b", `Int 8000000)]
        ~width:640 ~height:480 output
    in
    let audio =
      if audio then Some (audio_stream ~codec:"mp2" ~sample_format:`S16 output)
      else None
    in
    for index = 0 to 49 do
      Av.write_frame video (picture ~width:640 ~height:480 ~noise:true index);
      Option.iter
        (fun audio ->
          Av.write_frame audio (mp2_silence ~start:0 (2 * index));
          Av.write_frame audio (mp2_silence ~start:0 ((2 * index) + 1)))
        audio
    done;
    Av.close output
  in
  write video_only ~audio:false;
  write with_audio ~audio:true;
  let path = file name in
  write_file path
    (read_file video_only ^ read_file with_audio ^ read_file video_only);
  path

let requirement_3_8 () =
  let path = make_late_streams "late.mpg" in
  let input =
    Av.open_input
      ~opts:(opts [("probesize", `Int 4096); ("analyzeduration", `Int 0)])
      path
  in
  let streams () =
    List.length (Av.get_audio_streams input)
    + List.length (Av.get_video_streams input)
  in
  let at_open = streams () in
  let frames = List.length (video_frames input) in
  check (frames > 100) "the video is read";
  check
    (streams () > at_open)
    (Printf.sprintf "the stream lists grow: %d then %d" at_open (streams ()));
  Av.seek ~fmt:`Second ~ts:0L input;
  check
    (video_frames input <> [])
    "a seek and a read after the streams appeared";
  Av.close input

let requirement_3_9 () =
  let path = make_movie "movie.mkv" in
  let input = Av.open_input path in
  let _, audio, _ = List.hd (Av.get_audio_streams input) in
  let _, video, _ = List.hd (Av.get_video_streams input) in
  let _, subtitle, _ = List.hd (Av.get_subtitle_streams input) in
  let _, subtitle_frames, _ = List.hd (Av.get_subtitle_streams input) in
  let audio_packets = ref 0
  and video_frames = ref 0
  and subtitle_packets = ref 0 in
  let others = ref 0 in
  let rec read () =
    match
      Av.read_input ~audio_packet:[audio] ~video_frame:[video]
        ~subtitle_packet:[subtitle] ~subtitle_frame:[subtitle_frames] input
    with
      | `Audio_packet (index, _) ->
          equal (Av.get_index audio) index "the index of a packet";
          incr audio_packets;
          read ()
      | `Video_frame _ ->
          incr video_frames;
          read ()
      | `Subtitle_packet _ ->
          incr subtitle_packets;
          read ()
      | _ ->
          incr others;
          read ()
      | exception Error `Eof -> ()
  in
  read ();
  check (!audio_packets >= 100) "packets for the packet selection";
  equal (50, 3, 0)
    (!video_frames, !subtitle_packets, !others)
    (Printf.sprintf
       "frames for the frame selection, packets for a stream in both (%d %d %d)"
       !video_frames !subtitle_packets !others);
  let audio_count = !audio_packets in
  let input = Av.open_input path in
  let _, subtitle, _ = List.hd (Av.get_subtitle_streams input) in
  let unhandled = Hashtbl.create 4 in
  let count tag =
    Hashtbl.replace unhandled tag
      (1 + Option.value (Hashtbl.find_opt unhandled tag) ~default:0)
  in
  let on_unhandled_packet = function
    | `Audio_packet _ -> count "audio"
    | `Video_packet _ -> count "video"
    | `Subtitle_packet _ -> count "subtitle"
    | `Data_packet _ -> count "data"
  in
  let cues = ref [] in
  let rec read () =
    match
      Av.read_input ~on_unhandled_packet ~subtitle_frame:[subtitle] input
    with
      | `Subtitle_frame (_, frame) ->
          cues := Subtitle.get_content frame :: !cues;
          read ()
      | _ -> read ()
      | exception Error `Eof -> ()
  in
  read ();
  equal
    [("audio", audio_count); ("video", 50)]
    (List.sort compare (List.of_seq (Hashtbl.to_seq unhandled)))
    "unselected streams reach on_unhandled_packet, typed by kind";
  equal
    [Some 0L; Some 800000L; Some 1600000L]
    (List.rev_map (fun (c : Subtitle.content) -> c.pts) !cues)
    "subtitle frames carry their time";
  check
    (List.for_all
       (fun (c : Subtitle.content) -> c.end_display_time = 500)
       !cues)
    "and their duration";
  let other = Av.open_input path in
  raises is_failure "a stream of another container" (fun () ->
      Av.read_input ~subtitle_frame:[subtitle] other);
  raises is_failure "a stream of another container in seek" (fun () ->
      Av.seek ~stream:subtitle ~fmt:`Second ~ts:0L other)

let requirement_3_10 () =
  let input =
    Av.open_input
      ~configure_video_stream:(fun _ ->
        { codec = None; opts = Some (opts [("flags", `String "no-such-flag")]) })
      (make_video "video.mkv")
  in
  let _, stream, _ = List.hd (Av.get_video_streams input) in
  raises is_error "a decoder that fails to open" (fun () ->
      Av.read_input ~video_frame:[stream] input);
  (match Av.read_input ~video_frame:[stream] input with
    | _ -> check true "a later read succeeds"
    | exception Error _ -> check true "a later read raises again");
  Av.close input

let requirement_3_11 () =
  let path = file "mp2.mkv" in
  let output = Av.open_output path in
  let audio = audio_stream ~codec:"mp2" ~sample_format:`S16 output in
  for index = 0 to 9 do
    Av.write_frame audio (mp2_silence ~start:0 index)
  done;
  Av.close output;
  let sample_format ?configure_audio_stream () =
    let input = Av.open_input ?configure_audio_stream path in
    let _, stream, _ = List.hd (Av.get_audio_streams input) in
    match Av.read_input ~audio_frame:[stream] input with
      | `Audio_frame (_, frame) -> Audio.frame_get_sample_format frame
      | _ -> `None
  in
  equal `S16p (sample_format ()) "the default decoder";
  equal `Fltp
    (sample_format
       ~configure_audio_stream:(fun parameters ->
         equal `Mp2
           (Avcodec.Audio.get_params_id parameters)
           "the parameters before probing";
         {
           codec = Some (Avcodec.Audio.find_decoder_by_name "mp2float");
           opts = None;
         })
       ())
    "the preferred decoder decodes the stream";
  let width ?configure_video_stream () =
    let input =
      Av.open_input ?configure_video_stream (make_video "lowres.mkv")
    in
    let _, stream, _ = List.hd (Av.get_video_streams input) in
    match Av.read_input ~video_frame:[stream] input with
      | `Video_frame (_, frame) -> Video.frame_get_width frame
      | _ -> 0
  in
  equal 160 (width ()) "the stream's own size";
  let table = opts [("lowres", `Int 1)] in
  equal 80
    (width
       ~configure_video_stream:(fun _ -> { codec = None; opts = Some table })
       ())
    "the stream's decoder is opened with its options";
  equal ["lowres"] (keys table) "the configuration table is not modified"

external call_from_thread : _ container -> bool -> int = "test_call_from_thread"
external try_guard : _ container -> bool = "test_try_guard"
external release_guard : _ container -> unit = "test_release_guard"

external rewrap_input_format : (input, 'a) format -> (input, 'a) format
  = "test_rewrap_input_format"

external rewrap_output_format : (output, 'a) format -> (output, 'a) format
  = "test_rewrap_output_format"

external wrap_null_format : bool -> (_, _) format = "test_wrap_null_format"

let header_services () =
  let input = Av.open_input (make_video "guarded.mkv") in
  check (try_guard input) "the guard of a free container is taken from C";
  check (not (try_guard input)) "a taken guard is not taken twice";
  raises
    (function Error (`Failure "Object in use!") -> true | _ -> false)
    "an operation on a container guarded from C"
    (fun () -> Av.get_input_metadata input);
  release_guard input;
  ignore (Av.get_input_metadata input);
  Av.close input;
  let matroska = Option.get (Av.Format.find_input_format "matroska") in
  equal
    (Av.Format.get_input_name matroska)
    (Av.Format.get_input_name (rewrap_input_format matroska))
    "an input format wrapped from C";
  let mpegts =
    Option.get (Av.Format.guess_output_format ~short_name:"mpegts" ())
  in
  equal "mpegts"
    (Av.Format.get_output_name (rewrap_output_format mpegts))
    "an output format wrapped from C";
  List.iter
    (fun output ->
      raises
        (function Error (`Failure _) -> true | _ -> false)
        "a null format"
        (fun () -> wrap_null_format output))
    [false; true]

let mpegts () =
  Option.get (Av.Format.guess_output_format ~short_name:"mpegts" ())

(* Large intra frames: one write of the muxer exceeds FFmpeg's I/O buffer. *)
let write_heavy_video output =
  let stream =
    video_stream ~codec:"mjpeg"
      ~bindings:[("b", `Int 40000000)]
      ~width:640 ~height:480 ~pixel_format:`Yuvj420p output
  in
  for index = 0 to 19 do
    Av.write_frame stream
      (picture ~width:640 ~height:480 ~noise:true ~pixel_format:`Yuvj420p index)
  done;
  Av.close output

let requirement_3_12 () =
  let path = file "heavy.ts" in
  write_heavy_video (Av.open_output path);
  let expected = read_file path in
  check
    (String.length expected > 20 * 40000)
    "the frames are larger than the I/O buffer";
  let written = Buffer.create (String.length expected) in
  let seed = ref 12345 in
  let write bytes offset length =
    seed := ((!seed * 1103515245) + 12345) land 0x3fffffff;
    let consumed = 1 + (!seed mod length) in
    Buffer.add_subbytes written bytes offset consumed;
    consumed
  in
  write_heavy_video (Av.open_output_stream write (mpegts ()));
  check
    (Buffer.contents written = expected)
    "a custom write gets the bytes a file gets, whatever it consumes per call"

(* A read closure over a string, and the number of bytes it gave. *)
let reader ?(overrun = false) content =
  let position = ref 0 in
  let read bytes offset length =
    let available = min length (String.length content - !position) in
    Bytes.blit_string content !position bytes offset available;
    position := !position + available;
    if overrun then length + 10 else available
  in
  let seek offset whence =
    (position :=
       match whence with
         | Unix.SEEK_SET -> offset
         | Unix.SEEK_CUR -> !position + offset
         | Unix.SEEK_END -> String.length content + offset);
    !position
  in
  (read, seek, position)

let requirement_3_13 () =
  let path = make_video "video.mkv" in
  let content = read_file path in
  let expected = video_frames (Av.open_input path) in
  let read, seek, position = reader content in
  equal expected
    (video_frames (Av.open_input_stream ~seek read))
    "a custom input decodes like the file";
  let read, _, position_unseekable = reader content in
  equal expected
    (video_frames (Av.open_input_stream read))
    "and without a seek function";
  equal (String.length content) !position_unseekable
    "a read that returns 0 ends the input";
  ignore position;
  let read, _, _ = reader ~overrun:true content in
  raises is_error "a read that returns more than asked" (fun () ->
      Av.open_input_stream read)

(* The log messages of [f], which runs with a callback installed. *)
let logged f =
  let messages = ref [] in
  Log.set_callback (fun message -> messages := message :: !messages);
  Fun.protect ~finally:Log.clear_callback f;
  String.concat "" (List.rev !messages)

let contains text part =
  let rec search i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || search (i + 1))
  in
  search 0

let requirement_3_14 () =
  let content = read_file (make_video "video.mkv") in
  let log =
    logged (fun () ->
        raises is_error "a read closure that raises" (fun () ->
            Av.open_input_stream (fun _ _ _ -> failwith "read failure")))
  in
  check
    (contains log "read failure")
    "the exception of a read closure is logged";
  let log =
    logged (fun () ->
        let read, _, _ = reader content in
        match
          Av.open_input_stream ~seek:(fun _ _ -> failwith "seek failure") read
        with
          | input ->
              ignore (try video_frames input with Error _ -> []);
              Av.seek ~fmt:`Second ~ts:0L input |> ignore |> fun () ->
              Av.close input
          | exception Error _ -> ())
  in
  check
    (contains log "seek failure")
    "the exception of a seek closure is logged";
  let failing = ref false in
  let output =
    Av.open_output_stream
      (fun _ _ length -> if !failing then failwith "write failure" else length)
      (mpegts ())
  in
  let stream = video_stream output in
  for index = 0 to 9 do
    Av.write_frame stream (picture index)
  done;
  failing := true;
  let log =
    logged (fun () ->
        raises is_error "a write closure that raises" (fun () ->
            Av.flush output;
            Av.close output))
  in
  check
    (contains log "write failure")
    "the exception of a write closure is logged";
  Av.close output;
  let calls = ref 0 in
  let log =
    logged (fun () ->
        match
          Av.open_input
            ~interrupt:(fun () ->
              incr calls;
              failwith "interrupt failure")
            (make_video "interrupted.mkv")
        with
          | input -> Av.close input
          | exception Error _ -> ())
  in
  check
    (!calls = 0 || contains log "interrupt failure")
    "the exception of an interrupt function is logged"

let requirement_3_15 () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 1;
  let port =
    match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> 0
  in
  let start = Unix.gettimeofday () in
  let calls = Atomic.make 0 and stop = Atomic.make false in
  let interrupt () =
    Atomic.incr calls;
    Unix.gettimeofday () -. start > 0.5
  in
  let collector =
    Thread.create
      (fun () ->
        while not (Atomic.get stop) do
          Gc.compact ();
          Thread.yield ()
        done)
      ()
  in
  let outcome =
    match
      Av.open_input ~interrupt (Printf.sprintf "tcp://127.0.0.1:%d" port)
    with
      | input ->
          Av.close input;
          `Opened
      | exception Error `Exit -> `Interrupted
      | exception Error `Protocol_not_found -> `No_protocol
      | exception Error error -> `Failed (string_of_error error)
  in
  Atomic.set stop true;
  Thread.join collector;
  Unix.close socket;
  if outcome = `No_protocol then skip "FFmpeg has no tcp protocol";
  equal `Interrupted outcome "a blocked open returns once the function says so";
  check (Atomic.get calls > 1) "the function is polled while the open blocks";
  let after = Atomic.get calls in
  Thread.delay 0.2;
  equal after (Atomic.get calls)
    "the function is not called after the failed open";
  let calls = ref 0 in
  let input =
    Av.open_input
      ~interrupt:(fun () ->
        incr calls;
        false)
      (make_video "v.mkv")
  in
  ignore (video_frames input);
  Av.close input;
  let after = !calls in
  collect ();
  Thread.delay 0.1;
  equal after !calls "nor after close returned"

(* The offsets of the I pictures of a raw MPEG-4 stream, each given as the
   offset of the headers that lead to it. *)
let key_picture_offsets content =
  let length = String.length content in
  let start_code i =
    i + 3 < length
    && content.[i] = '\000'
    && content.[i + 1] = '\000'
    && content.[i + 2] = '\001'
  in
  let rec scan i unit_start offsets =
    if i + 4 >= length then List.rev offsets
    else if not (start_code i) then scan (i + 1) unit_start offsets
    else (
      let code = Char.code content.[i + 3] in
      let unit_start = Option.value unit_start ~default:i in
      if code <> 0xb6 then scan (i + 4) (Some unit_start) offsets
      else if Char.code content.[i + 4] lsr 6 = 0 then
        scan (i + 4) None (unit_start :: offsets)
      else scan (i + 4) None offsets)
  in
  scan 0 None []

let requirement_3_16 () =
  let path = file "keys.m4v" in
  let output = Av.open_output path in
  let stream = video_stream ~bindings:[("g", `Int 5); ("bf", `Int 0)] output in
  let positions = ref [] in
  let on_keyframe () =
    Av.flush output;
    positions := Option.get (Av.tell output) :: !positions
  in
  for index = 0 to 29 do
    Av.write_frame ~on_keyframe stream (picture index)
  done;
  Av.close output;
  let keys = key_picture_offsets (read_file path) in
  check (List.length keys >= 6) "the video has several key pictures";
  let show l = String.concat "," (List.map string_of_int l) in
  equal keys (List.rev !positions)
    (Printf.sprintf
       "every position recorded by on_keyframe is the start of a key packet: \
        %s / %s"
       (show keys)
       (show (List.rev !positions)));
  let output = Av.open_output (file "raising.m4v") in
  let stream = video_stream ~bindings:[("g", `Int 5); ("bf", `Int 0)] output in
  raises
    (fun exn -> exn = Exit)
    "an exception of on_keyframe"
    (fun () ->
      Av.write_frame ~on_keyframe:(fun () -> raise Exit) stream (picture 0));
  for index = 1 to 9 do
    Av.write_frame stream (picture index)
  done;
  Av.close output;
  equal 10
    (List.length (video_frames (Av.open_input (file "raising.m4v"))))
    "the key packet is written although on_keyframe raised"

let requirement_3_17 () =
  let write_to ?opts path =
    let output = Av.open_output ?opts path in
    let stream = video_stream output in
    for index = 0 to 9 do
      Av.write_frame stream (picture index)
    done;
    Av.close output
  in
  let plain = file "plain.mkv" and exact = file "exact.mkv" in
  write_to plain;
  let table =
    opts [("fflags", `String "+bitexact"); ("no_such_option", `Int 1)]
  in
  write_to ~opts:table exact;
  let muxer = Printf.sprintf "Lavf%d." Av.avformat_version.major in
  check
    (contains (read_file plain) muxer && not (contains (read_file exact) muxer))
    "a generic container option takes effect";
  equal ["no_such_option"] (keys table) "and is not reported unused";
  let small = file "small.ts" and padded = file "padded.ts" in
  write_to small;
  write_file padded (String.make 4000000 'x');
  let table =
    opts
      [
        ("muxrate", `Int 4000000);
        ("truncate", `Int 0);
        ("no_such_option", `Int 1);
      ]
  in
  write_to ~opts:table padded;
  equal ["no_such_option"] (keys table)
    "a muxer option and a protocol option are not reported unused";
  let size path = String.length (read_file path) in
  equal 4000000 (size padded)
    "the protocol option took effect: the file is not truncated";
  let rated = file "rated.ts" in
  write_to ~opts:(opts [("muxrate", `Int 4000000)]) rated;
  check
    (size rated > 3 * size small)
    "the muxer option took effect: the stream is padded"

let ours keys metadata =
  List.sort compare
    (List.filter_map
       (fun (key, v) ->
         let key = String.lowercase_ascii key in
         if List.mem key keys then Some (key, v) else None)
       metadata)

let requirement_3_18 () =
  let path = file "tagged.mkv" in
  let output = Av.open_output path in
  let stream = video_stream output in
  Av.set_output_metadata output [("first", "1"); ("second", "2")];
  Av.set_output_metadata output [("second", "3"); ("third", "4")];
  Av.set_metadata stream [("first", "1"); ("second", "2")];
  Av.set_metadata stream [("second", "5")];
  Av.write_frame stream (picture 0);
  raises is_failure "metadata once the header is written" (fun () ->
      Av.set_output_metadata output []);
  raises is_failure "stream metadata once the header is written" (fun () ->
      Av.set_metadata stream []);
  raises is_failure "a time base once the header is written" (fun () ->
      Av.set_time_base stream video_time_base);
  raises is_failure "a stream once the header is written" (fun () ->
      video_stream output);
  check (Av.output_started output) "the first write wrote the header";
  Av.close output;
  let input = Av.open_input path in
  let _, stream, _ = List.hd (Av.get_video_streams input) in
  let names = ["first"; "second"; "third"] in
  equal
    [("second", "3"); ("third", "4")]
    (ours names (Av.get_input_metadata input))
    "the second list replaces the first on a container";
  equal
    [("second", "5")]
    (ours names (Av.get_metadata stream))
    "and on a stream"

let requirement_3_19 () =
  let source = Av.open_input (make_video "source.mkv") in
  let _, video, parameters = List.hd (Av.get_video_streams source) in
  let rate = Av.get_avg_frame_rate video in
  equal (Some frame_rate) rate "the source has a frame rate";
  let path = file "remuxed.mkv" in
  let output = Av.open_output path in
  let copy = Av.new_stream_copy ~params:parameters output in
  equal None
    (Av.get_avg_frame_rate copy)
    "a copied stream reports no frame rate";
  Av.set_avg_frame_rate copy rate;
  equal rate (Av.get_avg_frame_rate copy) "until the caller sets it";
  let time_base = Av.get_time_base video in
  let rec remux () =
    match Av.read_input ~video_packet:[video] source with
      | `Video_packet (_, packet) ->
          Av.write_packet copy time_base packet;
          remux ()
      | _ -> remux ()
      | exception Error `Eof -> ()
  in
  remux ();
  Av.close output;
  let input = Av.open_input path in
  let _, video, _ = List.hd (Av.get_video_streams input) in
  equal rate
    (Av.get_avg_frame_rate video)
    "the remuxed stream reports the frame rate";
  equal 30 (List.length (video_frames input)) "and holds every frame"

let requirement_3_20 () =
  let path = file "live.ts" in
  let output = Av.open_output path in
  let stream = video_stream ~codec:"mpeg2video" output in
  for index = 0 to 49 do
    Av.write_frame stream (picture index)
  done;
  Av.close output;
  let file_input = Av.open_input path in
  check (Av.get_input_duration file_input <> None) "a file has a duration";
  let read, _, _ = reader (read_file path) in
  let live = Av.open_input_stream read in
  equal None
    (Av.get_input_duration ~format:`Millisecond live)
    "an input that cannot seek reports no duration";
  let _, video, _ = List.hd (Av.get_video_streams live) in
  equal None (Av.get_duration ~format:`Millisecond video) "nor does its stream";
  let input =
    Av.open_input
      (make_video ~codec:"mjpeg" ~bindings:[] ~pixel_format:`Yuvj420p
         "plain.avi")
  in
  let _, video, _ = List.hd (Av.get_video_streams input) in
  equal None
    (Av.get_pixel_aspect video)
    "a stream that declares no aspect ratio"

let requirement_3_21 () =
  let path = file "hours.mkv" in
  let output = Av.open_output path in
  let stream =
    Av.new_video_stream
      ~opts:(opts [("g", `Int 1)])
      ~pixel_format:`Yuv420p ~width:160 ~height:120
      ~time_base:{ num = 1; den = 1 }
      ~codec:(Avcodec.Video.find_encoder_by_name "mpeg4")
      output
  in
  List.iter
    (fun seconds ->
      let frame = picture seconds in
      Frame.set_pts frame (Some (Int64.of_int seconds));
      Av.write_frame stream frame)
    [0; 1; 14400];
  Av.close output;
  let input = Av.open_input path in
  let microseconds =
    Option.get (Av.get_input_duration ~format:`Microsecond input)
  in
  check
    (microseconds >= 14400000000L)
    (Printf.sprintf "the input lasts more than four hours: %Ld" microseconds);
  equal
    (Some (Int64.mul microseconds 1000L))
    (Av.get_input_duration ~format:`Nanosecond input)
    "a duration beyond three hours converts exactly to nanoseconds";
  let _, video, _ = List.hd (Av.get_video_streams input) in
  Av.seek ~fmt:`Nanosecond ~ts:14400000000000L input;
  match Av.read_input ~video_frame:[video] input with
    | `Video_frame (_, frame) ->
        equal (Some 14400000L) (Frame.pts frame)
          "a seek target beyond three hours too"
    | _ -> check false "a video frame"

let requirement_3_22 () =
  let source = Av.open_input (make_video ~frames:2 "tiny.mkv") in
  let _, _, parameters = List.hd (Av.get_video_streams source) in
  let written = ref 0 in
  let write _ _ length =
    written := !written + length;
    length
  in
  let format =
    Option.get (Av.Format.guess_output_format ~short_name:"data" ())
  in
  let output = Av.open_output_stream ~interleaved:false write format in
  let stream = Av.new_stream_copy ~params:parameters output in
  let megabyte = 1 lsl 20 in
  let packet : video Avcodec.Packet.t =
    Avcodec.Packet.create (String.make megabyte 'x')
  in
  for index = 0 to 2099 do
    Avcodec.Packet.set_pts packet (Some (Int64.of_int index));
    Avcodec.Packet.set_dts packet (Some (Int64.of_int index));
    Av.write_packet stream video_time_base packet
  done;
  Av.flush output;
  equal (Some (2100 * megabyte)) (Av.tell output) "tell beyond 2 GiB is exact";
  equal (2100 * megabyte) !written "and matches what was written";
  Av.close output

(* The cues of a SubRip file: (times, text lines). *)
let cues content =
  let lines =
    String.split_on_char '\n' content
    |> List.map (fun line ->
        if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1)
        else line)
  in
  let rec parse current cues = function
    | [] -> List.rev (if current = [] then cues else List.rev current :: cues)
    | "" :: rest ->
        parse [] (if current = [] then cues else List.rev current :: cues) rest
    | line :: rest -> parse (line :: current) cues rest
  in
  List.map
    (function _ :: times :: text -> (times, text) | cue -> ("", cue))
    (parse [] [] lines)

let requirement_3_23 () =
  let source = "fixtures/sample.srt" in
  let input = Av.open_input source in
  let _, subtitles, _ = List.hd (Av.get_subtitle_streams input) in
  let path = file "copy.srt" in
  let output = Av.open_output path in
  let stream =
    Av.new_subtitle_stream ~time_base:{ num = 1; den = 1000 }
      ~codec:(Avcodec.Subtitle.find_encoder_by_name "subrip")
      output
  in
  let count = ref 0 in
  let rec copy () =
    match Av.read_input ~subtitle_frame:[subtitles] input with
      | `Subtitle_frame (_, frame) ->
          incr count;
          Av.write_subtitle_frame stream
            (Subtitle.create_frame (Subtitle.get_content frame));
          copy ()
      | _ -> copy ()
      | exception Error `Eof -> ()
  in
  copy ();
  Av.close output;
  let expected = cues (read_file source) in
  check
    (!count > 3 && List.length expected = !count)
    "every cue is read as a frame";
  equal expected
    (cues (read_file path))
    "the cues rebuilt from their content are the same, to the millisecond"

let requirement_3_24 () =
  let playlist = file "stream.m3u8" in
  let output =
    Av.open_output
      ~opts:
        (opts [("master_pl_name", `String "master.m3u8"); ("hls_time", `Int 1)])
      playlist
  in
  let audio = audio_stream output in
  for index = 0 to 99 do
    Av.write_frame audio (silence index)
  done;
  let attribute = Av.codec_attr audio in
  Av.close output;
  let master = read_file (file "master.m3u8") in
  (match attribute with
    | Some attribute ->
        check
          (contains master (Printf.sprintf "CODECS=\"%s\"" attribute))
          ("FFmpeg's HLS muxer writes the same codec string: " ^ attribute)
    | None -> check false "an AAC stream has a codec string");
  equal (Some "mp4a.40.2") attribute "the codec string of AAC-LC";
  let output = Av.open_output (file "attributes.mkv") in
  equal (Some "mp4a.40.33")
    (Av.codec_attr (audio_stream ~codec:"mp2" ~sample_format:`S16 output))
    "MP2";
  equal (Some "ac-3") (Av.codec_attr (audio_stream ~codec:"ac3" output)) "AC-3";
  let video = video_stream output in
  equal None (Av.codec_attr video) "a codec with no codec string";
  check
    (Av.bitrate (audio_stream output) <> None)
    "the bit rate of an encoded stream";
  Av.close output

let requirement_3_25 () =
  let source = Av.open_input (make_movie "movie.mkv") in
  let _, _, audio_parameters = List.hd (Av.get_audio_streams source) in
  let output = Av.open_output (file "misuse.mkv") in
  ignore (audio_stream output);
  ignore (Av.new_stream_copy ~params:audio_parameters output);
  let _, encoding_as_packets, _ = List.nth (Av.get_audio_streams output) 0 in
  let _, copy_as_frames, _ = List.nth (Av.get_audio_streams output) 1 in
  let _, copy_as_audio, _ = List.nth (Av.get_audio_streams output) 1 in
  let packet : audio Avcodec.Packet.t = Avcodec.Packet.create "x" in
  raises is_failure "write_packet on a stream that encodes" (fun () ->
      Av.write_packet encoding_as_packets video_time_base packet);
  raises is_failure "write_frame on a stream copy" (fun () ->
      Av.write_frame copy_as_frames (silence 0));
  raises is_failure "get_frame_size on a stream copy" (fun () ->
      Av.get_frame_size copy_as_audio);
  let reserved = Av.new_uninitialized_stream_copy output in
  ignore (Av.initialize_stream_copy ~params:audio_parameters reserved);
  raises is_failure "a second initialisation of a reserved stream" (fun () ->
      Av.initialize_stream_copy ~params:audio_parameters reserved);
  raises is_failure "open_output_format on a file format" (fun () ->
      Av.open_output_format (mpegts ()));
  raises is_failure "open_output_stream on a format that needs no file"
    (fun () ->
      Av.open_output_stream
        (fun _ _ length -> length)
        (Option.get (Av.Format.guess_output_format ~short_name:"null" ())));
  Av.close output

let requirement_3_26 () =
  header_services ();
  let read, _, _ = reader (read_file (make_video "video.mkv")) in
  let input = Av.open_input_stream read in
  equal 16
    (call_from_thread input false)
    "a read function called from a thread created in C";
  let polled = ref 0 in
  let input =
    Av.open_input
      ~interrupt:(fun () ->
        incr polled;
        !polled > 1000000)
      (make_video "v.mkv")
  in
  let before = !polled in
  equal 0
    (call_from_thread input true)
    "an interrupt function called from such a thread";
  equal (before + 1) !polled "ran the closure";
  Av.close input

let container_options () =
  let table = opts [("probesize", `Int 123456); ("no_such_option", `Int 1)] in
  let input = Av.open_input ~opts:table (make_video "video.mkv") in
  equal ["no_such_option"] (keys table) "open_input reports the unused key only";
  let obj = Av.input_obj input in
  equal 123456
    (Options.get_int ~name:"probesize" obj)
    "an option set at open, read back";
  equal 123456L (Options.get_int64 ~name:"probesize" obj) "as an int64";
  equal "123456" (Options.get_string ~name:"probesize" obj) "as a string";
  raises
    (function Error `Option_not_found -> true | _ -> false)
    "an option of a child object, children not searched"
    (fun () -> Options.get_string ~name:"no_such_option" obj);
  let options = Options.opts Av.container_options in
  check
    (List.exists (fun (o : Options.opt) -> o.name = "probesize") options)
    "the options of containers are listed";
  check
    (List.exists
       (fun (o : Options.opt) ->
         o.name = "fflags"
         &&
           match o.spec with
           | `Flags { values; _ } -> List.mem_assoc "bitexact" values
           | _ -> false)
       options)
    "with their named constants";
  check (Av.avformat_version.major >= 61) "the libavformat version"

let formats () =
  let matroska = Option.get (Av.Format.find_input_format "matroska") in
  check
    (contains (Av.Format.get_input_name matroska) "matroska")
    "an input format name";
  check (Av.Format.get_input_long_name matroska <> "") "its long name";
  equal None
    (Av.Format.find_input_format "no such format")
    "an unknown input format";
  let mp4 () =
    Option.get (Av.Format.guess_output_format ~short_name:"mp4" ())
  in
  equal "mp4"
    (Av.Format.get_output_name (mp4 ()))
    "an output format by short name";
  check (Av.Format.get_output_long_name (mp4 ()) <> "") "its long name";
  equal `Aac (Av.Format.get_audio_codec_id (mp4 ())) "its default audio codec";
  check
    (Av.Format.get_video_codec_id (mp4 ()) <> `None)
    "its default video codec";
  equal `None
    (Av.Format.get_subtitle_codec_id
       (Option.get (Av.Format.guess_output_format ~short_name:"mpegts" ())))
    "a format with no default subtitle codec";
  equal "matroska"
    (Av.Format.get_output_name
       (Option.get (Av.Format.guess_output_format ~filename:"x.mkv" ())))
    "an output format by file name";
  equal None (Av.Format.guess_output_format ()) "nothing to guess from";
  let input = Av.open_input (make_movie "movie.mkv") in
  check
    (contains
       (Av.Format.get_input_name (Option.get (Av.get_input_format input)))
       "matroska")
    "the format of an input";
  let _, audio, parameters = Av.find_best_audio_stream input in
  equal `Aac (Avcodec.Audio.get_params_id parameters) "the best audio stream";
  equal (Av.get_index audio)
    (let index, _, _ = List.hd (Av.get_audio_streams input) in
     index)
    "its index";
  check (Av.get_input audio == input) "its container";
  check
    (Av.get_duration ~format:`Millisecond audio <> None
    || Av.get_input_duration input <> None)
    "a duration";
  equal [] (Av.get_data_streams input) "no data stream";
  check (Av.tell input <> None) "the position of an input";
  let video_only = Av.open_input (make_video "video.mkv") in
  raises
    (function Error `Stream_not_found -> true | _ -> false)
    "no stream of the kind asked"
    (fun () -> Av.find_best_audio_stream video_only);
  let output = Av.open_output (file "data.ts") in
  let data =
    Av.new_data_stream ~time_base:video_time_base ~codec:`Bin_data output
  in
  equal 1 (List.length (Av.get_data_streams output)) "a data stream";
  equal video_time_base (Av.get_time_base data) "its time base";
  Av.close output

let in_use = function Error (`Failure "Object in use!") -> true | _ -> false

(* Two threads use one container at once: every call returns, raises the
   in-use error, or raises what FFmpeg answers. *)
let concurrent_use () =
  let hammer operation =
    let failures = Atomic.make 0 and busy = Atomic.make 0 in
    let run () =
      for _ = 1 to 200 do
        try operation () with
          | exn when in_use exn -> Atomic.incr busy
          | Error _ -> ()
          | _ -> Atomic.incr failures
      done
    in
    List.iter Thread.join (List.init 2 (fun _ -> Thread.create run ()));
    equal 0 (Atomic.get failures) "a call returns or raises the in-use error";
    Atomic.get busy
  in
  let input = Av.open_input (make_video ~frames:250 "long.mkv") in
  let _, stream, _ = List.hd (Av.get_video_streams input) in
  let busy_input =
    hammer (fun () ->
        ignore (Av.read_input ~video_frame:[stream] input);
        ignore (Av.get_input_metadata input);
        Av.seek ~fmt:`Second ~ts:0L input)
  in
  let output = Av.open_output (file "contended.mkv") in
  let video = video_stream output in
  let frame = picture 0 in
  let busy_output =
    hammer (fun () ->
        Av.write_frame video frame;
        Av.flush output)
  in
  (try Av.close output with Error _ -> ());
  check (busy_input + busy_output > 0) "the guard was met at least once"

let dependent_handles () =
  let stream () =
    let input = Av.open_input (make_video "video.mkv") in
    let _, stream, _ = List.hd (Av.get_video_streams input) in
    stream
  in
  let stream = Sys.opaque_identity (stream ()) in
  collect ();
  equal video_time_base.num 1 "a time base";
  check
    ((Av.get_time_base stream).den > 0)
    "a stream kept alone keeps its container";
  equal 30
    (List.length
       (video_frames
          ~streams:(fun _ -> [(0, stream, Av.get_codec_params stream)])
          (Av.get_input stream)))
    "and reads it";
  let abandoned () =
    let output = Av.open_output (file "abandoned.mkv") in
    let video = video_stream output in
    for index = 0 to 9 do
      Av.write_frame video (picture index)
    done
  in
  abandoned ();
  collect ();
  check true "an output collected without close is released"

let requirements =
  [
    ("3.1", requirement_3_1);
    ("3.2", requirement_3_2);
    ("3.3", requirement_3_3);
    ("3.4", requirement_3_4);
    ("3.5", requirement_3_5);
    ("3.6", requirement_3_6);
    ("3.7", requirement_3_7);
    ("3.8", requirement_3_8);
    ("3.9", requirement_3_9);
    ("3.10", requirement_3_10);
    ("3.11", requirement_3_11);
    ("3.12", requirement_3_12);
    ("3.13", requirement_3_13);
    ("3.14", requirement_3_14);
    ("3.15", requirement_3_15);
    ("3.16", requirement_3_16);
    ("3.17", requirement_3_17);
    ("3.18", requirement_3_18);
    ("3.19", requirement_3_19);
    ("3.20", requirement_3_20);
    ("3.21", requirement_3_21);
    ("3.22", requirement_3_22);
    ("3.23", requirement_3_23);
    ("3.24", requirement_3_24);
    ("3.25", requirement_3_25);
    ("3.26", requirement_3_26);
    ("av.container-options", container_options);
    ("av.formats", formats);
    ("kc.av-concurrent-use", concurrent_use);
    ("kc.av-dependent-handles", dependent_handles);
  ]
