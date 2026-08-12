(* Plane sizing on the string output path: sizing a plane by stride * height
   ignores chroma subsampling and hands back U and V at twice their real
   length. *)

module Convert = Swscale.Make (Swscale.Bytes) (Swscale.Bytes)

let () =
  let w = 64 and h = 64 in
  let ctx = Convert.create [Swscale.Bilinear] w h `Rgb24 w h `Yuv420p in
  let src = [| (String.make (w * 3 * h) '\x80', w * 3) |] in

  (* Twice, so a stale high-water plane size would show up on the second. *)
  for pass = 1 to 2 do
    let out = Convert.convert ctx src in
    Test_assert.checkf
      (Array.length out = 3)
      "pass %d: yuv420p output has %d planes" pass (Array.length out);
    let len i = String.length (fst out.(i)) in
    Test_assert.checkf
      (len 0 = w * h)
      "pass %d: Y plane is %d bytes, want %d" pass (len 0) (w * h);
    Test_assert.checkf
      (len 1 = w * h / 4)
      "pass %d: U plane is %d bytes, want %d" pass (len 1)
      (w * h / 4);
    Test_assert.checkf
      (len 2 = w * h / 4)
      "pass %d: V plane is %d bytes, want %d" pass (len 2)
      (w * h / 4)
  done;

  Gc.full_major ();
  Test_assert.finish ()
