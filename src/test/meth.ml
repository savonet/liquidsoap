module T = Lang_types

let print t = Printf.printf "%s\n%!" (T.print t)

let () =
  let t = T.make (T.Ground T.Int) in
  print t;
  let t = T.meth "f" ([], T.make (T.Ground T.Float)) t in
  print t;
  let t = T.meth "ff" ([], T.make (T.Ground T.Float)) t in
  print t;
  let t = T.meths ["f"; "s"] ([], T.make (T.Ground T.String)) t in
  print t;
  let t = T.meths ["f"; "s"; "f'"] ([], T.make (T.Ground T.Float)) t in
  print t
