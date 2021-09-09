let should_work t t' r =
  let t = Type.make t in
  let t' = Type.make t' in
  let r = Type.make r in
  Printf.printf "Finding min for %s and %s\n%!" (Type.print t) (Type.print t');
  let m = Typing.sup t t' in
  Printf.printf "Got: %s, expect %s\n%!" (Type.print m) (Type.print r);
  Typing.(m <: r);
  Typing.(t <: m);
  Typing.(t' <: m)

let should_fail t t' =
  try
    ignore (Typing.sup (Type.make t) (Type.make t'));
    assert false
  with _ -> ()

let () =
  should_work
    (Type.EVar (1, []))
    (Type.Ground Type.Bool) (Type.Ground Type.Bool);
  should_work (Type.Ground Type.Bool)
    (Type.EVar (1, []))
    (Type.Ground Type.Bool);

  should_fail (Type.Ground Type.Bool) (Type.Ground Type.Int);
  should_fail
    (Type.List (Type.make (Type.Ground Type.Bool)))
    (Type.List (Type.make (Type.Ground Type.Int)));

  let m =
    Type.Meth
      ( "aa",
        ([], Type.make (Type.Ground Type.Int)),
        "",
        Type.make (Type.Ground Type.Bool) )
  in

  should_work m (Type.Ground Type.Bool) (Type.Ground Type.Bool);

  let n =
    Type.Meth ("b", ([], Type.make (Type.Ground Type.Bool)), "", Type.make m)
  in

  should_work m n m;

  let n =
    Type.Meth
      ( "aa",
        ([], Type.make (Type.Ground Type.Int)),
        "",
        Type.make (Type.Ground Type.Int) )
  in

  should_fail m n;

  let n =
    Type.Meth
      ( "aa",
        ([], Type.make (Type.Ground Type.Bool)),
        "",
        Type.make (Type.Ground Type.Bool) )
  in

  should_fail m n;

  ()
