open Type

let should_work t t' r =
  let t = make t in
  let t' = make t' in
  let r = make r in
  Printf.printf "Finding min for %s and %s\n%!" (print t) (print t');
  let m = Typing.sup ~pos:None t t' in
  Printf.printf "Got: %s, expect %s\n%!" (print m) (print r);
  Typing.(m <: r);
  Typing.(t <: m);
  Typing.(t' <: m)

let should_fail t t' =
  try
    ignore (Typing.sup ~pos:None (make t) (make t'));
    assert false
  with _ -> ()

let () =
  should_work (var ()).descr (Ground Bool) (Ground Bool);
  should_work (Ground Bool) (var ()).descr (Ground Bool);

  should_fail (Ground Bool) (Ground Int);
  should_fail (List (make (Ground Bool))) (List (make (Ground Int)));

  let mk_meth meth ty t =
    Meth ({ meth; scheme = ([], make ty); doc = ""; json_name = None }, make t)
  in

  let m = mk_meth "aa" (Ground Int) (Ground Bool) in

  should_work m (Ground Bool) (Ground Bool);

  let n = mk_meth "b" (Ground Bool) m in

  should_work m n m;

  let n = mk_meth "aa" (Ground Int) (Ground Int) in

  should_fail m n;

  let n = mk_meth "aa" (Ground Bool) (Ground Bool) in

  should_fail m n;

  ()
