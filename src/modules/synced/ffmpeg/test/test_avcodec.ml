(* Conformance of avcodec: spec/tests.md §2, and the entries of
   spec/known-complexity.md that avcodec answers ([kc.*]). *)

open Avutil
open Avcodec
open Harness

external registered_codecs : int -> bool -> string list
  = "test_registered_codecs"

external id_roundtrip : int -> 'id -> 'id = "test_id_roundtrip"
external supported_count : _ codec -> int -> int = "test_supported_count"

external unpack_metadata : _ Packet.t -> (string * string) list
  = "test_unpack_metadata"

external parameters_frame_rate : video params -> rational
  = "test_parameters_frame_rate"

external zero_frame : _ frame -> unit = "test_zero_frame"
external wrap_null : int -> unit = "test_wrap_null_codec_objects"

let is_error = function Error _ -> true | _ -> false
let is_failure = function Error (`Failure _) -> true | _ -> false
let is_eof = function Error `Eof -> true | _ -> false
let is_eagain = function Error `Eagain -> true | _ -> false
let in_use = function Error (`Failure "Object in use!") -> true | _ -> false
let opts bindings = Hashtbl.of_seq (List.to_seq bindings)
let keys table = List.sort compare (List.of_seq (Hashtbl.to_seq_keys table))
let frame_rate = { num = 25; den = 1 }
let video_time_base = { num = 1; den = 25 }

let picture index =
  let frame = Avutil.Video.create_frame 160 120 `Yuv420p in
  ignore
    (Avutil.Video.frame_visit ~make_writable:true
       (Array.iteri (fun plane (data, _) ->
            for i = 0 to Bigarray.Array1.dim data - 1 do
              data.{i} <- (i + (index * 7) + (plane * 31)) land 0xff
            done))
       frame);
  Frame.set_pts frame (Some (Int64.of_int index));
  frame

(* An mpeg4 encoder with B-frames: its decoder holds frames. *)
let video_encoder ?(bindings = [("bf", `Int 2)]) () =
  Video.create_encoder ~opts:(opts bindings) ~frame_rate ~pixel_format:`Yuv420p
    ~width:160 ~height:120 ~time_base:video_time_base
    (Video.find_encoder_by_name "mpeg4")

let encoded_video count =
  let encoder = video_encoder () in
  let packets = ref [] in
  let keep packet = packets := packet :: !packets in
  for index = 0 to count - 1 do
    encode encoder keep (picture index)
  done;
  flush_encoder encoder keep;
  (encoder, List.rev !packets)

let video_decoder encoder =
  Video.create_decoder ~params:(params encoder)
    (Video.find_decoder_by_name "mpeg4")

let audio_encoder ?(sample_format = `Fltp) name =
  Audio.create_encoder ~channel_layout:Channel_layout.stereo ~sample_rate:44100
    ~sample_format ~time_base:{ num = 1; den = 44100 }
    (Audio.find_encoder_by_name name)

let same_set expected actual description =
  equal
    (List.sort_uniq compare expected)
    (List.sort_uniq compare actual)
    description

let requirement_2_1 () =
  let names codecs = List.map name codecs in
  same_set (registered_codecs 0 true) (names Audio.encoders) "audio encoders";
  same_set (registered_codecs 0 false) (names Audio.decoders) "audio decoders";
  same_set (registered_codecs 1 true) (names Video.encoders) "video encoders";
  same_set (registered_codecs 1 false) (names Video.decoders) "video decoders";
  same_set (registered_codecs 2 true) (names Subtitle.encoders)
    "subtitle encoders";
  same_set
    (registered_codecs 2 false)
    (names Subtitle.decoders) "subtitle decoders";
  check (List.length Audio.decoders > 10) "FFmpeg registers audio decoders";
  List.iter (fun c -> ignore (Audio.get_id c)) Audio.encoders;
  List.iter (fun c -> ignore (Audio.get_id c)) Audio.decoders;
  List.iter (fun c -> ignore (Video.get_id c)) Video.encoders;
  List.iter (fun c -> ignore (Video.get_id c)) Video.decoders;
  List.iter (fun c -> ignore (Subtitle.get_id c)) Subtitle.encoders;
  List.iter (fun c -> ignore (Subtitle.get_id c)) Subtitle.decoders;
  check true "get_id succeeds on every listed codec"

let requirement_2_2 () =
  let round_trips family string_of_id ids =
    List.iter
      (fun id ->
        let back = id_roundtrip family id in
        equal back (id_roundtrip family back)
          "an identifier converts to C and back";
        equal (string_of_id id) (string_of_id back)
          "to an identifier of the same value")
      ids
  in
  round_trips 0 Audio.string_of_id Audio.codec_ids;
  round_trips 1 Video.string_of_id Video.codec_ids;
  round_trips 2 Subtitle.string_of_id Subtitle.codec_ids;
  round_trips 3 Unknown.string_of_id Unknown.codec_ids;
  equal `Aac (id_roundtrip 0 `Aac) "a literal identifier";
  equal `Aac
    (Audio.get_id (Audio.find_encoder `Aac))
    "an encoder found by identifier";
  equal `Mpeg4
    (Video.get_id (Video.find_decoder `Mpeg4))
    "a decoder found by identifier";
  equal "aac" (Audio.string_of_id `Aac) "the name of an identifier";
  equal "mpeg4" (string_of_id `Mpeg4) "the name of an identifier of any family";
  equal "none" (Unknown.string_of_id `None) "the none identifier";
  raises
    (function Error `Encoder_not_found -> true | _ -> false)
    "a video codec asked as an audio encoder"
    (fun () -> Audio.find_encoder_by_name "mpeg4");
  raises
    (function Error `Decoder_not_found -> true | _ -> false)
    "an unknown decoder"
    (fun () -> Video.find_decoder_by_name "no such codec")

let requirement_2_3 () =
  let aac = Audio.find_encoder_by_name "aac" in
  let mpeg2 = Video.find_encoder_by_name "mpeg2video" in
  let mjpeg = Video.find_encoder_by_name "mjpeg" in
  let counted description codec kind length =
    equal (supported_count codec kind) length description
  in
  counted "pixel formats" mjpeg 0
    (List.length (Video.get_supported_pixel_formats mjpeg));
  counted "frame rates" mpeg2 1
    (List.length (Video.get_supported_frame_rates mpeg2));
  counted "sample rates" aac 2
    (List.length (Audio.get_supported_sample_rates aac));
  counted "sample formats" aac 3
    (List.length (Audio.get_supported_sample_formats aac));
  counted "channel layouts" aac 4
    (List.length (Audio.get_supported_channel_layouts aac));
  counted "colour ranges" mjpeg 5
    (List.length (Video.get_supported_color_ranges mjpeg));
  counted "colour spaces" mjpeg 6
    (List.length (Video.get_supported_color_spaces mjpeg));
  check
    (Video.get_supported_frame_rates mpeg2 <> [])
    "mpeg2video declares frame rates";
  check (Audio.get_supported_sample_rates aac <> []) "aac declares sample rates";
  equal [`Fltp]
    (Audio.get_supported_sample_formats aac)
    "the sample formats of aac";
  equal []
    (Audio.get_supported_sample_rates (Audio.find_encoder_by_name "pcm_s16le"))
    "a codec that declares none";
  equal `Fltp (Audio.find_best_sample_format aac `S16) "best: the first listed";
  equal `Fltp
    (Audio.find_best_sample_format aac `Fltp)
    "best: the default when listed";
  equal 12345
    (Audio.find_best_sample_rate (Audio.find_encoder_by_name "pcm_s16le") 12345)
    "best: the default when nothing is listed";
  equal
    (List.hd (Audio.get_supported_sample_rates aac))
    (Audio.find_best_sample_rate aac 12345)
    "best sample rate";
  equal { num = 50; den = 2 }
    (Video.find_best_frame_rate mpeg2 { num = 50; den = 2 })
    "best frame rate: equal as fractions";
  equal
    (List.hd (Video.get_supported_frame_rates mpeg2))
    (Video.find_best_frame_rate mpeg2 { num = 17; den = 1 })
    "best frame rate: first";
  check
    (List.mem
       (Video.find_best_pixel_format mjpeg `Rgb24)
       (Video.get_supported_pixel_formats mjpeg))
    "best pixel format";
  check
    (Channel_layout.compare Channel_layout.stereo
       (Audio.find_best_channel_layout aac Channel_layout.stereo))
    "best channel layout"

let requirement_2_4 () =
  check
    (List.mem `Variable_frame_size
       (capabilities (Audio.find_encoder_by_name "pcm_s16le")))
    "a capability of an encoder";
  let decoder = capabilities (Video.find_decoder_by_name "mpeg4") in
  check
    (List.mem `Dr1 decoder && List.mem `Frame_threads decoder)
    "capabilities of a decoder";
  check (not (List.mem `Experimental decoder)) "a capability it does not have";
  List.iter
    (fun (config : hw_config) ->
      check (config.methods <> []) "a hardware method")
    (hw_configs (Video.find_decoder_by_name "h264"));
  let descriptor = Option.get (Audio.descriptor `Aac) in
  equal "aac" descriptor.name "descriptor name";
  equal `Audio descriptor.media_type "descriptor media type";
  check (List.mem `Lossy descriptor.properties) "descriptor properties";
  check (descriptor.profiles <> []) "descriptor profiles";
  check (descriptor.long_name <> None) "descriptor long name";
  equal ["image/jpeg"] (Option.get (Video.descriptor `Mjpeg)).mime_types
    "MIME types";
  equal "mpeg4" (Video.get_name (Video.find_encoder_by_name "mpeg4")) "get_name";
  check
    (Video.get_description (Video.find_encoder_by_name "mpeg4") <> "")
    "get_description"

let tasks () = Array.length (Sys.readdir "/proc/self/task")

let requirement_2_5 () =
  if not (Sys.file_exists "/proc/self/task") then skip "no /proc";
  let before = tasks () in
  let encoder = video_encoder ~bindings:[("threads", `Int 1)] () in
  let packets = ref [] in
  for index = 0 to 11 do
    encode encoder (fun packet -> packets := packet :: !packets) (picture index)
  done;
  equal before (tasks ()) "an encoder given one thread starts none";
  let decoder = video_decoder encoder in
  List.iter (decode decoder ignore) (List.rev !packets);
  check (tasks () > before + 1) "a decoder uses several threads by default";
  ignore (Sys.opaque_identity (encoder, decoder))

let requirement_2_6 () =
  let count = 30 in
  let encoder, packets = encoded_video count in
  equal count (List.length packets) "one packet per frame";
  let decoder = video_decoder encoder in
  let timestamps = ref [] and empty = ref 0 in
  let keep frame = timestamps := Frame.pts frame :: !timestamps in
  List.iter
    (fun packet ->
      let before = List.length !timestamps in
      decode decoder keep packet;
      if List.length !timestamps = before then incr empty)
    packets;
  check (!empty > 0) "a packet that produces no frame is not an error";
  check (List.length !timestamps < count) "the decoder holds frames";
  flush_decoder decoder keep;
  equal
    (List.init count (fun i -> Some (Int64.of_int i)))
    (List.rev !timestamps)
    "every frame is returned, in order, the flush included"

let requirement_2_7 () =
  let encoder, packets = encoded_video 6 in
  let decoder = video_decoder encoder in
  let frames = ref 0 in
  let count _ = incr frames in
  List.iter (decode decoder count) packets;
  flush_decoder decoder count;
  equal 6 !frames "the decoder delivered everything";
  flush_decoder decoder count;
  equal 6 !frames "a second flush delivers nothing";
  raises is_eof "decode after flush" (fun () ->
      decode decoder count (List.hd packets));
  raises is_eof "encode after flush" (fun () ->
      encode encoder ignore (picture 0));
  let packets = ref 0 in
  flush_encoder encoder (fun _ -> incr packets);
  equal 0 !packets "a second flush of an encoder delivers nothing"

let requirement_2_8 () =
  let encoder, packets = encoded_video 20 in
  let decoder = video_decoder encoder in
  let frames = ref 0 in
  List.iter (decode decoder (fun _ -> incr frames)) packets;
  let raise_once exn =
    let raised = ref false in
    fun _ ->
      incr frames;
      if not !raised then (
        raised := true;
        raise exn)
  in
  raises
    (fun exn -> exn = Exit)
    "an exception of the user function"
    (fun () -> flush_decoder decoder (raise_once Exit));
  raises is_eof "decode on a draining decoder" (fun () ->
      decode decoder ignore (List.hd packets));
  raises is_eof "Error `Eof raised by the user function" (fun () ->
      flush_decoder decoder (raise_once (Error `Eof)));
  flush_decoder decoder (fun _ -> incr frames);
  equal 20 !frames "the pending frames are delivered by the next calls";
  let encoder = video_encoder () in
  let packets = ref 0 in
  for index = 0 to 9 do
    encode encoder (fun _ -> incr packets) (picture index)
  done;
  raises
    (fun exn -> exn = Exit)
    "an exception while flushing an encoder"
    (fun () -> flush_encoder encoder (raise_once Exit));
  flush_encoder encoder (fun _ -> incr packets);
  equal 10 (!packets + !frames - 20 - 1 + 1) "the encoder's pending packets too"

let packet_properties packet =
  Packet.
    ( content packet,
      get_size packet,
      get_pts packet,
      get_dts packet,
      get_duration packet,
      get_flags packet,
      get_stream_index packet )

let requirement_2_9 () =
  let packet : video Packet.t = Packet.create "hello" in
  equal "hello" (Packet.content packet) "content";
  equal (Bytes.of_string "hello") (Packet.to_bytes packet) "to_bytes";
  equal 5 (Packet.get_size packet) "size";
  equal
    (None, None, None, None, 0, [])
    Packet.
      ( get_pts packet,
        get_dts packet,
        get_duration packet,
        get_position packet,
        get_stream_index packet,
        get_flags packet )
    "a new packet has no property";
  Packet.set_pts packet (Some 10L);
  Packet.set_dts packet (Some 9L);
  Packet.set_duration packet (Some 3L);
  Packet.set_position packet (Some 1234L);
  Packet.set_stream_index packet 2;
  Packet.set_flags packet [`Corrupt; `Keyframe];
  equal
    (Some 10L, Some 9L, Some 3L, Some 1234L, 2, [`Keyframe; `Corrupt])
    Packet.
      ( get_pts packet,
        get_dts packet,
        get_duration packet,
        get_position packet,
        get_stream_index packet,
        get_flags packet )
    "each setter then its getter";
  Packet.set_duration packet (Some 0L);
  Packet.set_position packet (Some (-1L));
  equal (None, None)
    (Packet.get_duration packet, Packet.get_position packet)
    "FFmpeg's sentinels read back as absent";
  Packet.set_flags packet [];
  equal [] (Packet.get_flags packet) "set_flags replaces";
  let copy = Packet.dup packet in
  equal (packet_properties packet) (packet_properties copy)
    "dup copies the properties";
  Packet.set_pts copy (Some 77L);
  equal (Some 10L) (Packet.get_pts packet) "the copy has properties of its own";
  let encoder, packets = encoded_video 4 in
  let first = List.hd packets in
  check
    (List.mem `Keyframe (Packet.get_flags first))
    "the first packet is a key frame";
  let before = packet_properties first in
  decode (video_decoder encoder) ignore first;
  equal before (packet_properties first)
    "a packet given to a decoder is unchanged";
  let sets =
    List.find
      (fun (f : BitstreamFilter.filter) -> f.name = "sets")
      BitstreamFilter.filters
  in
  let filter, _ = BitstreamFilter.init sets (params encoder) in
  BitstreamFilter.send_packet filter first;
  equal before (packet_properties first)
    "a packet given to a filter is unchanged";
  raises is_failure "a negative stream index beyond the C range" (fun () ->
      Packet.set_stream_index packet (1 lsl 40))

let requirement_2_10 () =
  let packet : audio Packet.t = Packet.create "x" in
  equal [] (Packet.side_data packet) "no side data";
  let gain =
    `Replaygain
      { Packet.track_gain = -3; track_peak = 4; album_gain = 5; album_peak = 6 }
  in
  let strings = `Strings_metadata [("title", "a"); ("artist", "b c")] in
  let update = `Metadata_update [("k", "v")] in
  Packet.add_side_data packet gain;
  equal [gain] (Packet.side_data packet) "side_data after add_side_data";
  Packet.add_side_data packet strings;
  Packet.add_side_data packet update;
  equal [gain; strings; update] (Packet.side_data packet)
    "each kind, in the packet's order";
  equal
    [("title", "a"); ("artist", "b c")]
    (unpack_metadata packet) "FFmpeg unpacks the dictionary the binding wrote";
  Packet.add_side_data packet (`Strings_metadata [("other", "1")]);
  equal 3 (List.length (Packet.side_data packet)) "a type is carried once";
  check
    (List.mem (`Strings_metadata [("other", "1")]) (Packet.side_data packet))
    "adding a type again replaces it";
  equal (Packet.side_data packet)
    (Packet.side_data (Packet.dup packet))
    "dup copies side data";
  let empty : audio Packet.t = Packet.create "" in
  Packet.add_side_data empty (`Metadata_update []);
  equal [`Metadata_update []] (Packet.side_data empty) "an empty dictionary"

let sets () =
  match
    List.find_opt
      (fun (f : BitstreamFilter.filter) -> f.name = "sets")
      BitstreamFilter.filters
  with
    | Some filter -> filter
    | None -> skip "the sets bitstream filter is missing"

let requirement_2_11 () =
  let sets = sets () in
  check
    (List.exists
       (fun (o : Options.opt) -> o.name = "pts")
       (Options.opts sets.options))
    "the filter's private options are listed";
  let encoder, packets = encoded_video 3 in
  let table = opts [("pts", `String "PTS+1000"); ("no_such_option", `Int 1)] in
  let filter, output = BitstreamFilter.init ~opts:table sets (params encoder) in
  equal ["no_such_option"] (keys table)
    "the private option is not reported unused";
  equal `Mpeg4 (Video.get_params_id output) "the output parameters";
  raises is_eagain "receive before any input" (fun () ->
      BitstreamFilter.receive_packet filter);
  let packet = List.hd packets in
  let pts = Option.get (Packet.get_pts packet) in
  BitstreamFilter.send_packet filter packet;
  raises is_eagain "the filter wants its next packet" (fun () ->
      BitstreamFilter.receive_packet filter);
  BitstreamFilter.send_packet filter (List.nth packets 1);
  equal
    (Some (Int64.add pts 1000L))
    (Packet.get_pts (BitstreamFilter.receive_packet filter))
    "the private option took effect";
  BitstreamFilter.send_eof filter;
  ignore (BitstreamFilter.receive_packet filter);
  raises is_eof "receive once drained" (fun () ->
      BitstreamFilter.receive_packet filter);
  let table = opts [("pts", `String "not ( an expression")] in
  raises is_error "a rejected option" (fun () ->
      BitstreamFilter.init ~opts:table sets (params encoder));
  equal ["pts"] (keys table) "a failed init leaves the table untouched"

let requirement_2_12 () =
  let encoder = video_encoder () in
  let parameters = params encoder in
  equal frame_rate
    (parameters_frame_rate parameters)
    "the parameters carry the frame rate";
  equal video_time_base (time_base encoder) "time_base";
  equal (160, 120)
    (Video.get_width parameters, Video.get_height parameters)
    "size";
  equal (Some `Yuv420p) (Video.get_pixel_format parameters) "pixel format";
  equal `Mpeg4 (Video.get_params_id parameters) "identifier";
  equal None
    (Video.get_pixel_aspect parameters)
    "an unknown aspect ratio is absent";
  equal 0 (Video.get_sample_aspect_ratio parameters).num "the raw aspect ratio";
  equal "mpeg4" (Option.get (descriptor parameters)).name
    "the descriptor of parameters";
  let table = opts [("b", `Int 300000); ("no_such_option", `Int 1)] in
  let encoder =
    Video.create_encoder ~opts:table ~pixel_format:`Yuv420p ~width:160
      ~height:120 ~time_base:video_time_base
      (Video.find_encoder_by_name "mpeg4")
  in
  equal ["no_such_option"] (keys table) "the table holds the unused key only";
  equal 300000 (Video.get_bit_rate (params encoder)) "the option took effect";
  let rejected description create =
    let table = opts [("b", `Int 300000); ("no_such_option", `Int 1)] in
    raises is_error description (fun () -> create table);
    equal ["b"; "no_such_option"] (keys table)
      "a failed creation leaves the table"
  in
  rejected "an unsupported pixel format" (fun opts ->
      Video.create_encoder ~opts ~pixel_format:`Rgb24 ~width:160 ~height:120
        ~time_base:video_time_base
        (Video.find_encoder_by_name "mjpeg"));
  rejected "an unsupported size" (fun opts ->
      Video.create_encoder ~opts ~pixel_format:`Yuv420p ~width:0 ~height:120
        ~time_base:video_time_base
        (Video.find_encoder_by_name "mpeg4"));
  rejected "an unsupported sample format" (fun opts ->
      Audio.create_encoder ~opts ~channel_layout:Channel_layout.stereo
        ~sample_rate:44100 ~sample_format:`S16
        ~time_base:{ num = 1; den = 44100 }
        (Audio.find_encoder_by_name "aac"));
  let aac = audio_encoder "aac" in
  let parameters = params aac in
  equal 1024 (Audio.frame_size aac) "a fixed frame size";
  equal 0
    (Audio.frame_size (audio_encoder ~sample_format:`S16 "pcm_s16le"))
    "any frame size";
  equal `Aac (Audio.get_params_id parameters) "audio identifier";
  equal (44100, 2, `Fltp)
    Audio.
      ( get_sample_rate parameters,
        get_nb_channels parameters,
        get_sample_format parameters )
    "audio parameters";
  check
    (Channel_layout.compare Channel_layout.stereo
       (Audio.get_channel_layout parameters))
    "audio layout";
  check (Audio.get_bit_rate parameters > 0) "audio bit rate";
  let decoder =
    Audio.create_decoder ~params:parameters (Audio.find_decoder_by_name "aac")
  in
  equal `Fltp (Audio.sample_format decoder) "the sample format of a decoder";
  check (flag_qscale > 0 && version.major >= 61) "constants read from FFmpeg"

let requirement_2_13 () = skip "no hardware device"

let audio_round_trip () =
  let encoder = audio_encoder "aac" in
  let decoder =
    Audio.create_decoder ~params:(params encoder)
      (Audio.find_decoder_by_name "aac")
  in
  let samples = ref 0 in
  let decode_packet packet =
    decode decoder
      (fun frame -> samples := !samples + Avutil.Audio.frame_nb_samples frame)
      packet
  in
  for index = 0 to 19 do
    let frame =
      Avutil.Audio.create_frame `Fltp Channel_layout.stereo 44100 1024
    in
    zero_frame frame;
    Frame.set_pts frame (Some (Int64.of_int (index * 1024)));
    encode encoder decode_packet frame
  done;
  flush_encoder encoder decode_packet;
  flush_decoder decoder (fun frame ->
      samples := !samples + Avutil.Audio.frame_nb_samples frame);
  check (!samples >= 20 * 1024) "every encoded sample is decoded";
  List.iter
    (fun kind -> raises is_failure "a null object" (fun () -> wrap_null kind))
    [0; 1; 2]

(* Two threads use one handle at once: every call returns, raises the in-use
   error, or raises what FFmpeg answers to interleaved input. *)
let concurrent_use () =
  let hammer operation =
    let failures = Atomic.make 0 and busy = Atomic.make 0 in
    let run () =
      for _ = 1 to 150 do
        try operation () with
          | exn when in_use exn -> Atomic.incr busy
          | Error _ -> ()
          | _ -> Atomic.incr failures
      done
    in
    let threads = List.init 2 (fun _ -> Thread.create run ()) in
    List.iter Thread.join threads;
    equal 0 (Atomic.get failures) "a call returns or raises the in-use error";
    Atomic.get busy
  in
  let encoder = video_encoder () in
  let frame = picture 0 in
  let busy_encoder =
    hammer (fun () ->
        encode encoder ignore frame;
        ignore (params encoder))
  in
  let source, packets = encoded_video 4 in
  let decoder = video_decoder source in
  let packet = List.hd packets in
  let busy_decoder = hammer (fun () -> decode decoder ignore packet) in
  let filter, _ = BitstreamFilter.init (sets ()) (params source) in
  let busy_filter =
    hammer (fun () ->
        BitstreamFilter.send_packet filter packet;
        ignore (BitstreamFilter.receive_packet filter))
  in
  check
    (busy_encoder + busy_decoder + busy_filter > 0)
    "the guard was met at least once"

let resident_megabytes () =
  let ic =
    try open_in "/proc/self/statm" with Sys_error _ -> skip "no /proc"
  in
  let pages =
    Scanf.sscanf (input_line ic) "%d %d" (fun _ resident -> resident)
  in
  close_in ic;
  pages * 4096 / 1048576

let native_memory () =
  let payload = String.make (1 lsl 20) 'x' in
  let start = resident_megabytes () in
  let original : video Packet.t = Packet.create payload in
  for i = 1 to 3000 do
    ignore (Sys.opaque_identity (Packet.create payload : video Packet.t));
    ignore (Sys.opaque_identity (Packet.dup original));
    if i mod 500 = 0 && resident_megabytes () - start > 1000 then
      failwith "dropped packets are not collected"
  done;
  check (resident_megabytes () - start <= 1000) "resident memory stays bounded"

(* Every failure path, many times, for a leak detector. *)
let error_paths () =
  let encoder, packets = encoded_video 3 in
  let garbage : video Packet.t = Packet.create (String.make 64 '\xff') in
  let sets = sets () in
  for _ = 1 to 100 do
    ignore
      (try Some (video_encoder ~bindings:[("bf", `String "nonsense")] ())
       with Error _ -> None);
    ignore
      (try Some (audio_encoder ~sample_format:`S16 "aac") with Error _ -> None);
    let decoder = video_decoder encoder in
    (try decode decoder ignore garbage with Error _ -> ());
    decode decoder ignore (List.hd packets);
    let filter, _ = BitstreamFilter.init sets (params encoder) in
    (try ignore (BitstreamFilter.receive_packet filter)
     with Error `Eagain -> ());
    ignore
      (try
         Some
           (BitstreamFilter.init
              ~opts:(opts [("pts", `String "(")])
              sets (params encoder))
       with Error _ -> None)
  done;
  check true "the failure paths ran"

let requirements =
  [
    ("2.1", requirement_2_1);
    ("2.2", requirement_2_2);
    ("2.3", requirement_2_3);
    ("2.4", requirement_2_4);
    ("2.5", requirement_2_5);
    ("2.6", requirement_2_6);
    ("2.7", requirement_2_7);
    ("2.8", requirement_2_8);
    ("2.9", requirement_2_9);
    ("2.10", requirement_2_10);
    ("2.11", requirement_2_11);
    ("2.12", requirement_2_12);
    ("2.13", requirement_2_13);
    ("avcodec.audio", audio_round_trip);
    ("kc.avcodec-concurrent-use", concurrent_use);
    ("kc.avcodec-native-memory", native_memory);
    ("kc.avcodec-error-paths", error_paths);
  ]
