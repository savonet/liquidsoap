(* Conformance of swresample: spec/tests.md §6.

   Every data kind is exercised through planar float arrays: samples go to
   the kind and come back, so each kind is an output and an input. *)

open Avutil
open Harness
module S = Swresample

let is_failure = function Error (`Failure _) -> true | _ -> false

(* A data kind, the sample format to give it when its own is open, and the
   precision of that format. *)
type kind =
  | Kind :
      (module S.AudioData with type t = 'a) * Sample_format.t option * float
      -> kind

let u8 = 2. /. 128.
and s16 = 2. /. 32768.
and exact = 1e-6

let kinds =
  [
    ("Bytes", Kind ((module S.Bytes), Some `S16, s16));
    ("U8Bytes", Kind ((module S.U8Bytes), None, u8));
    ("S16Bytes", Kind ((module S.S16Bytes), None, s16));
    ("S32Bytes", Kind ((module S.S32Bytes), None, exact));
    ("FltBytes", Kind ((module S.FltBytes), None, exact));
    ("DblBytes", Kind ((module S.DblBytes), None, exact));
    ("U8PlanarBytes", Kind ((module S.U8PlanarBytes), None, u8));
    ("S16PlanarBytes", Kind ((module S.S16PlanarBytes), None, s16));
    ("S32PlanarBytes", Kind ((module S.S32PlanarBytes), None, exact));
    ("FltPlanarBytes", Kind ((module S.FltPlanarBytes), None, exact));
    ("DblPlanarBytes", Kind ((module S.DblPlanarBytes), None, exact));
    ("FloatArray", Kind ((module S.FloatArray), None, exact));
    ("PlanarFloatArray", Kind ((module S.PlanarFloatArray), None, exact));
    ("U8BigArray", Kind ((module S.U8BigArray), None, u8));
    ("S16BigArray", Kind ((module S.S16BigArray), None, s16));
    ("S32BigArray", Kind ((module S.S32BigArray), None, exact));
    ("FltBigArray", Kind ((module S.FltBigArray), None, exact));
    ("DblBigArray", Kind ((module S.DblBigArray), None, exact));
    ("U8PlanarBigArray", Kind ((module S.U8PlanarBigArray), None, u8));
    ("S16PlanarBigArray", Kind ((module S.S16PlanarBigArray), None, s16));
    ("S32PlanarBigArray", Kind ((module S.S32PlanarBigArray), None, exact));
    ("FltPlanarBigArray", Kind ((module S.FltPlanarBigArray), None, exact));
    ("DblPlanarBigArray", Kind ((module S.DblPlanarBigArray), None, exact));
    ("Frame", Kind ((module S.Frame), Some `Fltp, exact));
    ("U8Frame", Kind ((module S.U8Frame), None, u8));
    ("S16Frame", Kind ((module S.S16Frame), None, s16));
    ("S32Frame", Kind ((module S.S32Frame), None, exact));
    ("FltFrame", Kind ((module S.FltFrame), None, exact));
    ("DblFrame", Kind ((module S.DblFrame), None, exact));
    ("U8PlanarFrame", Kind ((module S.U8PlanarFrame), None, u8));
    ("S16PlanarFrame", Kind ((module S.S16PlanarFrame), None, s16));
    ("S32PlanarFrame", Kind ((module S.S32PlanarFrame), None, exact));
    ("FltPlanarFrame", Kind ((module S.FltPlanarFrame), None, exact));
    ("DblPlanarFrame", Kind ((module S.DblPlanarFrame), None, exact));
  ]

(* Nine channels have no standard layout: their order is unspecified. *)
let layouts =
  [
    Channel_layout.mono;
    Channel_layout.stereo;
    Channel_layout.five_point_one;
    Channel_layout.get_default 9;
  ]

(* [samples] samples per channel, each channel a ramp of its own. *)
let ramp ?(samples = 256) layout =
  Array.init (Channel_layout.get_nb_channels layout) (fun channel ->
      Array.init samples (fun i ->
          (float (((i * (channel + 3)) + (channel * 17)) mod 200) /. 250.)
          -. 0.4))

let close ~precision expected actual =
  Array.length expected = Array.length actual
  && Array.for_all2
       (fun expected actual ->
         Array.length expected = Array.length actual
         && Array.for_all2
              (fun a b -> Float.abs (a -. b) <= precision)
              expected actual)
       expected actual

let rate = 44100

(* What a kind does with samples: [there] converts planar floats to the
   kind, [back] the reverse; [ranged] converts a range of a value of the
   kind; [twice] converts two inputs and gives the first result read before
   and after the second conversion. *)
type 'r probe = {
  probe :
    'a. (module S.AudioData with type t = 'a) -> Sample_format.t option -> 'r;
}

let with_kind (Kind (kind, format, _)) probe = probe.probe kind format

let round_trip layout input kind =
  with_kind kind
    {
      probe =
        (fun (type a) (module K : S.AudioData with type t = a) format ->
          let module There = S.Make (S.PlanarFloatArray) (K) in
          let module Back = S.Make (K) (S.PlanarFloatArray) in
          let there =
            There.create layout rate layout ?out_sample_format:format rate
          in
          let back =
            Back.create layout ?in_sample_format:format rate layout rate
          in
          Back.convert back (There.convert there input));
    }

let requirement_6_1 () =
  List.iter
    (fun layout ->
      let input = ramp layout in
      List.iter
        (fun (name, (Kind (_, _, precision) as kind)) ->
          check
            (close ~precision input (round_trip layout input kind))
            (Printf.sprintf "identity through %s with %d channels" name
               (Channel_layout.get_nb_channels layout)))
        kinds)
    layouts

let requirement_6_2 () =
  let layout = Channel_layout.stereo in
  let input = ramp layout in
  let expected = Array.map (fun channel -> Array.sub channel 40 100) input in
  List.iter
    (fun (name, (Kind (_, _, precision) as kind)) ->
      let ranged =
        with_kind kind
          {
            probe =
              (fun (type a) (module K : S.AudioData with type t = a) format ->
                let module There = S.Make (S.PlanarFloatArray) (K) in
                let module Back = S.Make (K) (S.PlanarFloatArray) in
                let there =
                  There.create layout rate layout ?out_sample_format:format rate
                in
                let back =
                  Back.create layout ?in_sample_format:format rate layout rate
                in
                Back.convert ~offset:40 ~length:100 back
                  (There.convert there input));
          }
      in
      check
        (close ~precision expected ranged)
        ("offset and length select the range of " ^ name))
    kinds;
  let module Floats = S.Make (S.PlanarFloatArray) (S.PlanarFloatArray) in
  let converter = Floats.create layout rate layout rate in
  equal
    (Array.map (fun channel -> Array.sub channel 200 56) input)
    (Floats.convert ~offset:200 converter input)
    "an omitted length is everything after the offset";
  equal [| [||]; [||] |]
    (Floats.convert ~offset:256 converter input)
    "an empty range"

let requirement_6_3 () =
  let layout = Channel_layout.stereo in
  let module Resample = S.Make (S.PlanarFloatArray) (S.PlanarFloatArray) in
  let converter = Resample.create layout 44100 layout 48000 in
  let total = ref 0 in
  for _ = 1 to 10 do
    total :=
      !total
      + Array.length
          (Resample.convert converter (ramp ~samples:4410 layout)).(0)
  done;
  check (!total < 48000) "the resampler holds a tail back";
  total := !total + Array.length (Resample.flush converter).(0);
  check
    (abs (!total - 48000) <= 1)
    (Printf.sprintf "the total is the input scaled by the rate ratio: %d" !total);
  equal [| [||]; [||] |] (Resample.flush converter) "a second flush is empty";
  equal 4800
    (Array.length (Resample.convert converter (ramp ~samples:4410 layout)).(0)
    + Array.length (Resample.flush converter).(0))
    "convert may be called again after flush";
  let module To_frame = S.Make (S.PlanarFloatArray) (S.FltPlanarFrame) in
  let to_frame = To_frame.create layout 44100 layout 48000 in
  ignore (To_frame.convert to_frame (ramp layout));
  ignore (To_frame.flush to_frame);
  let empty = To_frame.flush to_frame in
  equal 0
    (Audio.frame_nb_samples empty)
    "an empty flush is a frame of no sample";
  equal (48000, 2, `Fltp)
    Audio.
      ( frame_get_sample_rate empty,
        frame_get_channels empty,
        frame_get_sample_format empty )
    "with the output rate, layout and format";
  equal None (Frame.pts empty) "and no timestamp"

let requirement_6_4 () =
  let layout = Channel_layout.stereo in
  let first = ramp layout in
  let second = Array.map (Array.map (fun sample -> -.sample)) first in
  List.iter
    (fun (name, kind) ->
      let before, after =
        with_kind kind
          {
            probe =
              (fun (type a) (module K : S.AudioData with type t = a) format ->
                let module There = S.Make (S.PlanarFloatArray) (K) in
                let module Back = S.Make (K) (S.PlanarFloatArray) in
                let there =
                  There.create layout rate layout ?out_sample_format:format rate
                in
                let back =
                  Back.create layout ?in_sample_format:format rate layout rate
                in
                let result = There.convert there first in
                let before = Back.convert back result in
                ignore (Sys.opaque_identity (There.convert there second));
                (before, Back.convert back result));
          }
      in
      equal before after
        ("a result of " ^ name ^ " is not written by the next call"))
    kinds

let requirement_6_5 () =
  let layout = Channel_layout.mono in
  let module Floats = S.Make (S.FloatArray) (S.FloatArray) in
  let converter = Floats.create layout rate layout rate in
  equal [| 0.25; 0.; -0.5 |]
    (Floats.convert converter [| 0.25; Float.nan; -0.5 |])
    "a NaN read from a float array is 0";
  let module From_bigarray = S.Make (S.DblBigArray) (S.FloatArray) in
  let converter = From_bigarray.create layout rate layout rate in
  let input =
    Bigarray.Array1.of_array Bigarray.float64 Bigarray.c_layout
      [| 0.25; Float.nan |]
  in
  equal [| 0.25; 0. |]
    (From_bigarray.convert converter input)
    "a NaN written to a float array is 0";
  let module Planar = S.Make (S.DblPlanarBigArray) (S.PlanarFloatArray) in
  let converter = Planar.create layout rate layout rate in
  equal
    [| [| 0.25; 0. |] |]
    (Planar.convert converter [| input |])
    "in planar arrays too"

let requirement_6_6 () =
  let layout = Channel_layout.stereo in
  let module Planar = S.Make (S.PlanarFloatArray) (S.PlanarFloatArray) in
  let converter = Planar.create layout rate layout rate in
  let input = ramp layout in
  raises is_failure "a wrong plane count" (fun () ->
      Planar.convert converter [| input.(0) |]);
  raises is_failure "planes of unequal length" (fun () ->
      Planar.convert converter [| input.(0); Array.sub input.(1) 0 10 |]);
  raises is_failure "a negative offset" (fun () ->
      Planar.convert ~offset:(-1) converter input);
  raises is_failure "a negative length" (fun () ->
      Planar.convert ~length:(-1) converter input);
  raises is_failure "an oversized range" (fun () ->
      Planar.convert ~offset:200 ~length:100 converter input);
  raises is_failure "an offset past the end" (fun () ->
      Planar.convert ~offset:257 converter input);
  let module Bytes = S.Make (S.S16PlanarBytes) (S.PlanarFloatArray) in
  let bytes = Bytes.create layout rate layout rate in
  raises is_failure "byte planes of unequal length" (fun () ->
      Bytes.convert bytes [| Stdlib.Bytes.create 64; Stdlib.Bytes.create 32 |]);
  raises is_failure "an oversized range of bytes" (fun () ->
      Bytes.convert ~length:33 bytes
        [| Stdlib.Bytes.create 64; Stdlib.Bytes.create 64 |]);
  let module Frames = S.Make (S.FltPlanarFrame) (S.PlanarFloatArray) in
  let frames = Frames.create layout rate layout rate in
  raises is_failure "a frame of another format" (fun () ->
      Frames.convert frames (Audio.create_frame `S16 layout rate 64));
  raises is_failure "a frame of another channel count" (fun () ->
      Frames.convert frames
        (Audio.create_frame `Fltp Channel_layout.mono rate 64));
  raises is_failure "an oversized range of a frame" (fun () ->
      Frames.convert ~offset:60 ~length:10 frames
        (Audio.create_frame `Fltp layout rate 64));
  equal 256
    (Array.length (Planar.convert converter input).(0))
    "the converter stays usable after failures"

let requirement_6_7 () =
  let layout = Channel_layout.stereo in
  let module Open = S.Make (S.Bytes) (S.PlanarFloatArray) in
  raises is_failure "a missing sample format" (fun () ->
      Open.create layout rate layout rate);
  raises is_failure "a planar format for interleaved bytes" (fun () ->
      Open.create layout ~in_sample_format:`S16p rate layout rate);
  let module Fixed = S.Make (S.S16Bytes) (S.PlanarFloatArray) in
  raises is_failure "a conflicting sample format" (fun () ->
      Fixed.create layout ~in_sample_format:`Flt rate layout rate);
  ignore (Fixed.create layout ~in_sample_format:`S16 rate layout rate);
  raises is_failure "a sample rate below 1" (fun () ->
      Fixed.create layout 0 layout rate);
  let encoder =
    Avcodec.Audio.create_encoder ~channel_layout:layout ~sample_rate:rate
      ~sample_format:`Fltp ~time_base:{ num = 1; den = rate }
      (Avcodec.Audio.find_encoder_by_name "aac")
  in
  let params = Avcodec.params encoder in
  let module Wrong = S.Make (S.S16Frame) (S.PlanarFloatArray) in
  raises is_failure "from_codec with a kind of another format" (fun () ->
      Wrong.from_codec params layout rate);
  let module From = S.Make (S.FltPlanarFrame) (S.PlanarFloatArray) in
  let from_codec = From.from_codec params layout rate in
  equal 64
    (Array.length
       (From.convert from_codec (Audio.create_frame `Fltp layout rate 64)).(0))
    "from_codec";
  let module To = S.Make (S.PlanarFloatArray) (S.FltPlanarFrame) in
  equal `Fltp
    (Audio.frame_get_sample_format
       (To.convert (To.to_codec layout rate params) (ramp layout)))
    "to_codec";
  let module Both = S.Make (S.FltPlanarFrame) (S.FltPlanarFrame) in
  equal 64
    (Audio.frame_nb_samples
       (Both.convert
          (Both.from_codec_to_codec params params)
          (Audio.create_frame `Fltp layout rate 64)))
    "from_codec_to_codec";
  check (S.version.major >= 5) "the libswresample version"

let requirement_6_8 () =
  let layout = Channel_layout.stereo in
  let module Ints = S.Make (S.PlanarFloatArray) (S.S16PlanarBigArray) in
  let convert options =
    Ints.convert (Ints.create ~options layout rate layout rate) (ramp layout)
  in
  let three = [`Filter_type_cubic; `Engine_swr; `Filter_type_kaiser] in
  check
    (convert (three @ [`Dither_triangular]) <> convert three)
    "the fourth element of the options is applied";
  let module Floats = S.Make (S.PlanarFloatArray) (S.PlanarFloatArray) in
  equal 256
    (Array.length
       (Floats.convert
          (Floats.create
             ~options:[`Engine_soxr; `Engine_swr]
             layout rate layout rate)
          (ramp layout)).(0))
    "a later element of a type overrides an earlier one"

let requirement_6_9 () =
  let custom = Channel_layout.find "FR+FL" in
  let module Floats = S.Make (S.PlanarFloatArray) (S.FltPlanarFrame) in
  for _ = 1 to 20 do
    let converter = Floats.create custom rate custom rate in
    let frame = Floats.convert converter (ramp custom) in
    check
      (Channel_layout.compare custom (Audio.frame_get_channel_layout frame))
      "a custom-order layout on each side"
  done;
  collect ()

let in_use = function Error (`Failure "Object in use!") -> true | _ -> false

let concurrent_use () =
  let layout = Channel_layout.stereo in
  let module Floats = S.Make (S.FltPlanarBigArray) (S.FltPlanarBigArray) in
  let converter = Floats.create layout 44100 layout 48000 in
  let plane () =
    let plane =
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 200000
    in
    Bigarray.Array1.fill plane 0.1;
    plane
  in
  let input = [| plane (); plane () |] in
  let failures = Atomic.make 0 and busy = Atomic.make 0 in
  let run () =
    for _ = 1 to 60 do
      match Floats.convert converter input with
        | _ -> ()
        | exception exn when in_use exn -> Atomic.incr busy
        | exception _ -> Atomic.incr failures
    done
  in
  List.iter Thread.join (List.init 2 (fun _ -> Thread.create run ()));
  equal 0 (Atomic.get failures)
    "a conversion returns or raises the in-use error";
  check (Atomic.get busy > 0) "the guard was met at least once"

let requirements =
  [
    ("6.1", requirement_6_1);
    ("6.2", requirement_6_2);
    ("6.3", requirement_6_3);
    ("6.4", requirement_6_4);
    ("6.5", requirement_6_5);
    ("6.6", requirement_6_6);
    ("6.7", requirement_6_7);
    ("6.8", requirement_6_8);
    ("6.9", requirement_6_9);
    ("kc.swresample-concurrent-use", concurrent_use);
  ]
