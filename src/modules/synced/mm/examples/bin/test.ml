open Mm

let skip_long = ref false

let () =
  Printexc.record_backtrace true;
  Arg.parse
    [("--skip-long", Arg.Set skip_long, "Skip long tests.")]
    (fun _ -> ())
    "test [options]"

let () = if not (Sys.file_exists "out") then Sys.mkdir "out" 0o755

let write fname s =
  let fname = "out/" ^ fname in
  let oc = open_out fname in
  output_string oc s;
  close_out oc

let test ?(skip = false) name f =
  Printf.printf "- %s... %!" name;
  if skip then Printf.printf "skipped\n%!"
  else (
    f ();
    Printf.printf "ok\n%!")

let time ?skip name f =
  let skip = Option.value ~default:!skip_long skip in
  Printf.printf "- %s... %!" name;
  if skip then Printf.printf "skipped\n%!"
  else (
    let t0 = Sys.time () in
    f ();
    let t1 = Sys.time () in
    Printf.printf "%.02f s\n%!" (t1 -. t0));
  Gc.full_major ()

let () = Printf.printf "\n# Testing MM library\n\n%!"

let () =
  Printf.printf "## Architecture\n\n%!";
  Printf.printf "- word size: %d\n%!" Sys.word_size;
  Printf.printf "\n%!"

let () =
  Printf.printf "## Testing basic functions\n\n%!";
  assert (Audio.Note.to_string Audio.Note.a4 = "A 4");
  assert (Audio.Note.to_string Audio.Note.c5 = "C 5");
  assert (
    try
      ignore (Image.RGB8.Color.of_int 0xff00ff);
      true
    with _ -> false);
  assert (
    try
      ignore
        (Image.RGB8.Color.of_int
           (Option.get (Int32.unsigned_to_int 0xaaff00ffl)));
      false
    with _ -> true)

module A = Audio

let () =
  Printf.printf "## Testing audio\n\n%!";
  let len = 44100 in
  let iter = 10000 in
  test "basic functions" (fun () ->
      let a = A.create 2 len in
      A.noise a 0 len;
      A.pan 0.4 a 0 len;
      ignore (A.squares a 0 len);
      assert (A.length a = len);
      A.amplify 0.5 a 0 len);
  let b = A.create 2 len in
  let a = A.create 2 len in
  time "adding many buffers" (fun () ->
      for _ = 1 to 10000 do
        A.add b 0 a 0 len
      done);
  let src = A.make 2 len 1. in
  let buf = Bytes.create (A.U8.size 2 len) in
  let dst = A.create 2 len in
  time "u8 conversion" (fun () ->
      for _ = 1 to iter do
        A.U8.of_audio src 0 buf 0 len;
        A.U8.to_audio (Bytes.unsafe_to_string buf) 0 dst 0 len
      done;
      assert (dst.(1).(len - 1) = 1.));
  let src = A.make 2 len 1. in
  let buf = Bytes.create (A.S16LE.size 2 len) in
  let dst = A.create 2 len in
  time "s16le conversion" (fun () ->
      for _ = 1 to iter do
        A.S16LE.of_audio src 0 buf 0 len;
        A.S16LE.to_audio (Bytes.unsafe_to_string buf) 0 dst 0 len
      done;
      assert (dst.(1).(len - 1) = 1.);
      assert (dst.(1).(len - 1) = 1.));
  let src = A.make 2 len 1. in
  let buf = Bytes.create (A.S16BE.size 2 len) in
  let dst = A.create 2 len in
  time "s16be conversion" (fun () ->
      for _ = 1 to iter do
        A.S16BE.of_audio src 0 buf 0 len;
        A.S16BE.to_audio (Bytes.unsafe_to_string buf) 0 dst 0 len
      done;
      assert (dst.(1).(len - 1) = 1.);
      assert (dst.(1).(len - 1) = 1.));
  let src = A.make 2 len 1. in
  let buf = Bytes.create (A.S24LE.size 2 len) in
  let dst = A.create 2 len in
  time "s24le conversion" (fun () ->
      for _ = 1 to iter do
        A.S24LE.of_audio src 0 buf 0 len;
        A.S24LE.to_audio (Bytes.unsafe_to_string buf) 0 dst 0 len
      done;
      assert (dst.(1).(len - 1) = 1.);
      assert (dst.(1).(len - 1) = 1.));
  let src = A.make 2 len 1. in
  let buf = Bytes.create (A.S32LE.size 2 len) in
  let dst = A.create 2 len in
  time "s32le conversion" (fun () ->
      for _ = 1 to iter do
        A.S32LE.of_audio src 0 buf 0 len;
        A.S32LE.to_audio (Bytes.unsafe_to_string buf) 0 dst 0 len
      done;
      assert (dst.(1).(len - 1) = 1.));
  test "s16le with offset" (fun () ->
      let chans = 2 in
      let src = A.create chans len in
      let off = 21 in
      let buf = Bytes.create (off + A.S16LE.size chans len) in
      A.S16LE.of_audio src 5 buf off (len - 5);
      A.S16LE.to_audio (Bytes.unsafe_to_string buf) off src 0 len);
  test "generate sine" (fun () ->
      let chans = 2 in
      let samplerate = 44100 in
      let sl = new A.Mono.Generator.sine samplerate 440. in
      let sr = new A.Mono.Generator.sine samplerate 880. in
      let len = samplerate in
      let buf = A.create chans len in
      let f = new A.IO.Writer.to_wav_file chans samplerate "out/sine.wav" in
      for _ = 0 to 9 do
        sl#fill buf.(0) 0 len;
        sr#fill buf.(1) 0 len;
        f#write buf 0 len
      done;
      f#close);
  time "compute replaygain" (fun () ->
      let f = new A.IO.Reader.of_wav_file "out/sine.wav" in
      let channels = f#channels in
      let rg =
        A.Analyze.ReplayGain.create ~channels ~samplerate:f#sample_rate
      in
      let loop = ref true in
      let len = 1024 in
      let buf = A.create channels len in
      while !loop do
        let n = f#read buf 0 len in
        if n = 0 then loop := false
        else A.Analyze.ReplayGain.process rg buf 0 len
      done;
      let gain = A.Analyze.ReplayGain.gain rg in
      (* Printf.printf "[%.02f dB] %!" gain; *)
      assert (abs_float (gain -. -14.56) < 0.05))

let () = Printf.printf "\n"

module I = Image

let () =
  Printf.printf "## Testing images\n\n%!";
  test "rounding" (fun () ->
      for k = 1 to 5 do
        for n = 0 to 33 do
          assert (
            I.Data.round k n
            = Float.to_int (float k *. Float.ceil (float n /. float k)))
        done
      done);
  test "fill buffer" (fun () ->
      let a = I.YUV420.create 10 10 in
      I.YUV420.fill a (0, 0, 0));
  test "various sizes" (fun () ->
      for i = 0 to 7 do
        for j = 0 to 7 do
          let w = 16 + i in
          let h = 16 + j in
          let a = I.YUV420.create w h in
          I.YUV420.set_pixel_rgba a (w - 1) (h - 1) (0, 0, 0, 0);
          I.YUV420.fill a (0, 0, 0);
          I.YUV420.randomize a
        done
      done);
  test "adding images" (fun () ->
      let a = I.YUV420.create 640 480 in
      I.YUV420.blank a;
      I.YUV420.fill a (10, 10, 10);
      let b = I.YUV420.create 64 64 in
      I.YUV420.fill b (10, 10, 10);
      I.YUV420.add b a;
      I.YUV420.add b ~x:10 ~y:10 a;
      I.YUV420.add b ~x:(-10) ~y:(-10) a;
      I.YUV420.add b ~x:1000 ~y:1000 a);
  test "converting" (fun () ->
      let a = I.YUV420.create 640 480 in
      let b = I.YUV420.of_RGBA32 (I.YUV420.to_RGBA32 a) in
      ignore b);
  test "blank" (fun () ->
      let img = I.YUV420.create 640 480 in
      I.YUV420.blank img;
      write "blank.bmp" (I.YUV420.to_BMP img));
  test "add" (fun () ->
      let img = I.YUV420.create 640 480 in
      I.YUV420.blank img;
      I.YUV420.fill_alpha img 0;
      let r = I.YUV420.create 200 100 in
      I.YUV420.fill r (I.Pixel.yuv_of_rgb (0xff, 0, 0));
      I.YUV420.add r ~x:10 ~y:70 img;
      I.YUV420.rotate (I.YUV420.copy img) 200 200 0.7 img;
      write "add.bmp" (I.YUV420.to_BMP img));
  test "canvas line" (fun () ->
      for _ = 1 to 100 do
        let l =
          I.CanvasYUV420.Draw.line (0xff, 0xff, 0xff, 0xff) (15, 24) (59, 78)
        in
        ignore l
      done);
  test "hmirror" (fun () ->
      let img = I.YUV420.create 1000 1000 in
      I.YUV420.gradient_uv img (0, 0) (0xff, 0) (0, 0xff);
      I.YUV420.hmirror img;
      write "hmirror.bmp" (I.YUV420.to_BMP img));
  test "render canvas" (fun () ->
      let r = I.YUV420.create 200 200 in
      I.YUV420.fill r (I.Pixel.yuv_of_rgb (0xff, 0, 0));
      let img = I.CanvasYUV420.make ~x:150 ~y:200 ~width:600 ~height:600 r in
      let img = I.CanvasYUV420.render img in
      write "canvas.bmp" (I.YUV420.to_BMP img));
  test "scale grid proportionally" (fun () ->
      let d = 100 in
      let s = 10 in
      let img = I.YUV420.create d d in
      for j = 0 to d - 1 do
        for i = 0 to d - 1 do
          let c =
            if ((i / s) + (j / s)) mod 2 = 0 then (0xff, 0xff, 0xff, 0xff)
            else (0x00, 0x00, 0x00, 0xff)
          in
          I.YUV420.set_pixel_rgba img i j c
        done
      done;
      let img =
        I.CanvasYUV420.make img
        |> I.CanvasYUV420.resize 640 362
        |> I.CanvasYUV420.render
      in
      write "scale-grid.bmp" (I.YUV420.to_BMP img));
  test "scale canvas" (fun () ->
      let img = I.YUV420.create 1000 1000 in
      I.YUV420.gradient_uv img (0, 0) (0xff, 0) (0, 0xff);
      let img = I.CanvasYUV420.make img in
      let img' = I.CanvasYUV420.scale (200, 1000) (300, 1000) img in
      let img = I.CanvasYUV420.add img' img in
      let img = I.CanvasYUV420.render img in
      write "scale-canvas.bmp" (I.YUV420.to_BMP img));
  test "gradient" (fun () ->
      let img = I.YUV420.create 640 480 in
      I.YUV420.gradient_uv img (0, 0) (100, 200) (200, 150);
      write "gradient.bmp" (I.YUV420.to_BMP img));
  test "manual gradient" (fun () ->
      let d = 400 in
      let img = I.YUV420.create d d in
      for j = 0 to d - 1 do
        for i = 0 to d - 1 do
          I.YUV420.set_pixel_rgba img i j (0xff, 0, 0, (i + j) * 0xff / (2 * d))
        done
      done;
      let bg = I.YUV420.create d d in
      I.YUV420.fill bg (0, 0, 0);
      I.YUV420.add img ~x:50 ~y:50 bg;
      write "mgradient.bmp" (I.YUV420.to_BMP bg));
  test "color to alpha" (fun () ->
      let d = 500 in
      let img = I.YUV420.create d d in
      I.YUV420.fill img (0, 0, 0);
      let c = (0xff, 0xff, 20) in
      let r = I.YUV420.create 200 200 in
      I.YUV420.fill r c;
      I.YUV420.add r ~x:100 ~y:150 img;
      I.YUV420.alpha_of_color img c 5;
      write "c2a.bmp" (I.YUV420.to_BMP img));
  test "is_opaque" (fun () ->
      let img = I.YUV420.create 1000 1000 in
      assert (I.YUV420.is_opaque img);
      I.YUV420.set_pixel_rgba img 10 10 (0, 0, 0, 10);
      assert (not (I.YUV420.is_opaque img));
      I.YUV420.set_pixel_rgba img 10 10 (0, 0, 0, 0xff);
      assert (I.YUV420.is_opaque img));
  test "add" (fun () ->
      let a = I.YUV420.create 1280 480 in
      let b = I.YUV420.create 640 480 in
      let x = 1280 in
      let y = 0 in
      I.YUV420.add a ~x ~y b);
  time "many adds" (fun () ->
      let r = I.YUV420.create 500 500 in
      I.YUV420.fill r (I.Pixel.yuv_of_rgb (0xff, 0, 0));
      let img = ref (I.CanvasYUV420.create 2000 2000) in
      for i = 1 to 100000 do
        (* only the first 2000 are relevant *)
        let r = I.CanvasYUV420.make r |> I.CanvasYUV420.translate i i in
        img := I.CanvasYUV420.add r !img
      done;
      let img = I.CanvasYUV420.render !img in
      write "adds.bmp" (I.YUV420.to_BMP img));
  test "mean cannot clip" (fun () ->
      (* Ensure that we don't have to clip when computing the mean of two pixels
         in bytes. *)
      for s = 0 to 0xff do
        for t = 0 to 0xff do
          for a = 0 to 0xff do
            let p = ((s * a) + (t * (0xff - a))) / 0xff in
            assert (0 <= p && p <= 0xff)
          done
        done
      done);
  time "many adds with alpha" (fun () ->
      let r = I.YUV420.create 500 500 in
      for j = 0 to 499 do
        for i = 0 to 499 do
          let a = match i mod 3 with 0 -> 0 | 1 -> 0x7f | _ -> 0xff in
          I.YUV420.set_pixel_rgba r i j (0xff, 0, 0, a)
        done
      done;
      let img = ref (I.CanvasYUV420.create 2000 2000) in
      for i = 1 to 100000 do
        (* only the first 2000 are relevant *)
        let r = I.CanvasYUV420.make r |> I.CanvasYUV420.translate i i in
        img := I.CanvasYUV420.add r !img
      done;
      let img = I.CanvasYUV420.render !img in
      write "adds-alpha.bmp" (I.YUV420.to_BMP img));
  time "scale" (fun () ->
      let img = I.YUV420.create 1000 1000 in
      I.YUV420.gradient_uv img (0, 0) (0xff, 0) (0, 0xff);
      let img2 = I.YUV420.create 2000 2000 in
      I.YUV420.scale img img2;
      write "scale.bmp" (I.YUV420.to_BMP img2));
  test "font" (fun () ->
      let img = I.Bitmap.Font.render ~size:30 "Hello, world!\nHow are you?" in
      write "hello-world.bmp" (I.YUV420.to_BMP (I.YUV420.of_bitmap img)));
  time "sliding font" (fun () ->
      let width = 1280 in
      let height = 720 in
      let fps = 24 in
      let duration = 10 in
      let txt = I.Bitmap.Font.render ~size:30 "Hello, world!\nHow are you?" in
      let txt = I.YUV420.of_bitmap txt in
      let fname = "out/hello-world.avi" in
      let oc = open_out fname in
      output_string oc
        (Video.AVI.Writer.header ~width ~height ~framerate:fps ());
      for i = 0 to duration * fps do
        let img = I.YUV420.create width height in
        I.YUV420.fill img (I.Pixel.yuv_of_rgb (2 * i, 0, 0xff));
        I.YUV420.add txt ~x:(3 * i) ~y:(2 * i) img;
        output_string oc (Video.AVI.Writer.Chunk.video_yuv420 img)
      done;
      close_out oc);
  test "empty text" (fun () -> ignore (I.Bitmap.Font.render ~size:20 ""));
  time "increasing saw" (fun () ->
      let width = 640 in
      let height = 360 in
      let fontsize = 70 in
      let fps = 24 in
      let fname = "out/saw.avi" in
      let channels = 2 in
      let samplerate = 44100 in
      let oc = open_out fname in
      output_string oc
        (Video.AVI.Writer.header ~width ~height ~framerate:fps ~channels
           ~samplerate ());
      let fmin = 20. in
      let fmax = 20000. in
      let duration = 20. in
      let t = ref 0. in
      let dt = 1. /. float samplerate in
      (* fmin + 2^(duration/a) = fmax => a = duration / log2 (fmax - fmin) *)
      let a = duration /. (log (fmax -. fmin) /. log 2.) in
      let buf = Audio.create channels (samplerate / fps) in
      let osc = ref (-1.) in
      while !t <= duration do
        let f = fmin +. (2. ** (!t /. a)) in
        let txt =
          I.Bitmap.Font.render ~size:fontsize (Printf.sprintf "%.2f Hz" f)
        in
        let txt = I.YUV420.of_bitmap txt in
        let img = I.YUV420.create width height in
        I.YUV420.blank img;
        I.YUV420.add txt ~y:((height - fontsize) / 2) img;
        output_string oc (Video.AVI.Writer.Chunk.video_yuv420 img);
        for i = 0 to Audio.length buf - 1 do
          for c = 0 to Audio.channels buf - 1 do
            buf.(c).(i) <- !osc
          done;
          osc := !osc +. (2. *. f /. float samplerate);
          while !osc > 1. do
            osc := !osc -. 1.
          done;
          t := !t +. dt
        done;
        output_string oc (Video.AVI.Writer.Chunk.audio_s16le buf)
      done;
      close_out oc)

let () = Gc.full_major ()
