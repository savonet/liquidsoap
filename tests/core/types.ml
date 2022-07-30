open Type

let should_work t t' r =
  let t = make t in
  let t' = make t' in
  let r = make r in
  Printf.printf "Finding min for %s and %s\n%!" (to_string t) (to_string t');
  let m = Typing.sup ~pos:None t t' in
  Printf.printf "Got: %s, expect %s\n%!" (to_string m) (to_string r);
  Typing.(m <: r);
  Typing.(t <: m);
  Typing.(t' <: m)

let should_fail t t' =
  try
    ignore (Typing.sup ~pos:None (make t) (make t'));
    assert false
  with _ -> ()

let () =
  should_work (var ()).descr (Ground Ground.Bool) (Ground Ground.Bool);
  should_work (Ground Ground.Bool) (var ()).descr (Ground Ground.Bool);

  should_fail (Ground Ground.Bool) (Ground Ground.Int);
  should_fail
    (List { t = make (Ground Ground.Bool); json_repr = `Tuple })
    (List { t = make (Ground Ground.Int); json_repr = `Tuple });

  let mk_meth meth ty t =
    Meth ({ meth; scheme = ([], make ty); doc = ""; json_name = None }, make t)
  in

  let m = mk_meth "aa" (Ground Ground.Int) (Ground Ground.Bool) in

  should_work m (Ground Ground.Bool) (Ground Ground.Bool);

  let n = mk_meth "b" (Ground Ground.Bool) m in

  should_work m n m;

  let n = mk_meth "aa" (Ground Ground.Int) (Ground Ground.Int) in

  should_fail m n;

  let n = mk_meth "aa" (Ground Ground.Bool) (Ground Ground.Bool) in

  should_fail m n;

  ()
