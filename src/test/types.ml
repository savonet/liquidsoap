module T = Lang_types

let should_work t t' r =
  let t = T.make t in
  let t' = T.make t' in
  let r = T.make r in
  Printf.printf "Finding min for %s and %s\n%!" (T.print t) (T.print t');
  let m = T.min_type t t' in
  Printf.printf "Got: %s, expect %s\n%!" (T.print m) (T.print r);
  T.(m <: r);
  T.(t <: m);
  T.(t' <: m)

let should_fail t t' =
  try
    ignore (T.min_type (T.make t) (T.make t'));
    assert false
  with _ -> ()

let () =
  should_work (T.EVar (1, [])) (T.Ground T.Bool) (T.Ground T.Bool);
  should_work (T.Ground T.Bool) (T.EVar (1, [])) (T.Ground T.Bool);

  should_fail (T.Ground T.Bool) (T.Ground T.Int);
  should_fail
    (T.List (T.make (T.Ground T.Bool)))
    (T.List (T.make (T.Ground T.Int)));

  let m =
    T.Meth ("aa", ([], T.make (T.Ground T.Int)), T.make (T.Ground T.Bool))
  in

  should_work m (T.Ground T.Bool) (T.Ground T.Bool);

  let n = T.Meth ("b", ([], T.make (T.Ground T.Bool)), T.make m) in

  should_work m n m;

  let n =
    T.Meth ("aa", ([], T.make (T.Ground T.Int)), T.make (T.Ground T.Int))
  in

  should_fail m n;

  let n =
    T.Meth ("aa", ([], T.make (T.Ground T.Bool)), T.make (T.Ground T.Bool))
  in

  should_fail m n;

  ()
