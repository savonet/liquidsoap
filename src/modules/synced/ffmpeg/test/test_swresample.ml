(* Shape and content for every output vector kind: swapping two rows of the C
   dispatch table yields a structurally valid result with the wrong layout,
   while swapping an allocator against a store gives the right shape full of
   garbage, so neither check alone suffices. *)

let rate = 44100
let nb_samples = 1024
let mono = Avutil.Channel_layout.mono

(* A slow ramp over [-1, 1): resampling mono 44.1k to mono 44.1k is
   identity, so whatever comes out must still be the ramp. *)
let ramp =
  Array.init nb_samples (fun i ->
      (2. *. float_of_int i /. float_of_int nb_samples) -. 1.)

let close_enough a b = Float.abs (a -. b) < 0.01

let check_ramp what got =
  let n = Array.length got in
  Test_assert.checkf (n = nb_samples) "%s: %d samples, want %d" what n
    nb_samples;
  if n = nb_samples then begin
    let bad = ref 0 and worst = ref 0. in
    Array.iteri
      (fun i v ->
        if not (close_enough v ramp.(i)) then begin
          incr bad;
          if Float.abs (v -. ramp.(i)) > !worst then
            worst := Float.abs (v -. ramp.(i))
        end)
      got;
    Test_assert.checkf (!bad = 0) "%s: %d samples off the ramp (worst %.4f)"
      what !bad !worst
  end

module FromFloatArray = Swresample.Make (Swresample.FloatArray)
module ToFloatArray = FromFloatArray (Swresample.FloatArray)
module ToPlanarFloatArray = FromFloatArray (Swresample.PlanarFloatArray)
module ToFrame = FromFloatArray (Swresample.DblFrame)
module ToPlanarFrame = FromFloatArray (Swresample.DblPlanarFrame)
module ToBigArray = FromFloatArray (Swresample.DblBigArray)
module ToPlanarBigArray = FromFloatArray (Swresample.DblPlanarBigArray)
module ToBytes = FromFloatArray (Swresample.DblBytes)
module ToPlanarBytes = FromFloatArray (Swresample.DblPlanarBytes)
module FrameToFloatArray = Swresample.Make (Swresample.DblFrame)
module FrameBack = FrameToFloatArray (Swresample.FloatArray)

let bytes_to_floats b =
  Array.init (Bytes.length b / 8) (fun i -> Bytes.get_int64_le b (i * 8))
  |> Array.map Int64.float_of_bits

let ba_to_floats ba =
  Array.init (Bigarray.Array1.dim ba) (fun i -> Bigarray.Array1.get ba i)

let () =
  check_ramp "FloatArray"
    (ToFloatArray.convert (ToFloatArray.create mono rate mono rate) ramp);

  let planar =
    ToPlanarFloatArray.convert
      (ToPlanarFloatArray.create mono rate mono rate)
      ramp
  in
  Test_assert.checkf
    (Array.length planar = 1)
    "PlanarFloatArray: %d planes, want 1" (Array.length planar);
  check_ramp "PlanarFloatArray" planar.(0);

  let frame = ToFrame.convert (ToFrame.create mono rate mono rate) ramp in
  check_ramp "DblFrame"
    (FrameBack.convert (FrameBack.create mono rate mono rate) frame);

  let pframe =
    ToPlanarFrame.convert (ToPlanarFrame.create mono rate mono rate) ramp
  in
  ignore pframe;
  Test_assert.check "DblPlanarFrame converted" true;

  check_ramp "DblBigArray"
    (ba_to_floats
       (ToBigArray.convert (ToBigArray.create mono rate mono rate) ramp));

  let pba =
    ToPlanarBigArray.convert (ToPlanarBigArray.create mono rate mono rate) ramp
  in
  Test_assert.checkf
    (Array.length pba = 1)
    "DblPlanarBigArray: %d planes, want 1" (Array.length pba);
  check_ramp "DblPlanarBigArray" (ba_to_floats pba.(0));

  check_ramp "DblBytes"
    (bytes_to_floats
       (ToBytes.convert (ToBytes.create mono rate mono rate) ramp));

  let pbytes =
    ToPlanarBytes.convert (ToPlanarBytes.create mono rate mono rate) ramp
  in
  Test_assert.checkf
    (Array.length pbytes = 1)
    "DblPlanarBytes: %d planes, want 1" (Array.length pbytes);
  check_ramp "DblPlanarBytes" (bytes_to_floats pbytes.(0));

  (* At equal rates the converted count equals the allocated upper bound, so
     the kinds whose store step only fixes up a length (Frm, Ba, P_Ba) look
     correct even wired to the wrong one. Halving the rate makes the
     allocation an over-estimate, so the length has to be corrected. *)
  let half = rate / 2 in
  let expected = nb_samples / 2 in
  let near n = abs (n - expected) <= 16 in

  let ba = ToBigArray.convert (ToBigArray.create mono rate mono half) ramp in
  Test_assert.checkf
    (near (Bigarray.Array1.dim ba))
    "resampled DblBigArray: %d samples, want about %d" (Bigarray.Array1.dim ba)
    expected;

  let pba =
    ToPlanarBigArray.convert (ToPlanarBigArray.create mono rate mono half) ramp
  in
  Test_assert.checkf
    (Array.length pba = 1 && near (Bigarray.Array1.dim pba.(0)))
    "resampled DblPlanarBigArray: %d planes of %d samples, want 1 of about %d"
    (Array.length pba)
    (Bigarray.Array1.dim pba.(0))
    expected;

  let frame = ToFrame.convert (ToFrame.create mono rate mono half) ramp in
  Test_assert.checkf
    (near (Avutil.Audio.frame_nb_samples frame))
    "resampled DblFrame: %d samples, want about %d"
    (Avutil.Audio.frame_nb_samples frame)
    expected;

  Gc.full_major ();
  Test_assert.finish ()
