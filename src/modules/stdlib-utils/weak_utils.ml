(* Weak array internal layout (ephemeron with n keys, unused data field):
   field 0   : GC ephemeron list link
   field 1   : ephemeron data (unused in Weak.t)
   field 2+i : weak pointer i *)
let ephe_first_key = 2

(* caml_ephe_none: static sentinel written into empty slots by the GC.
   Captured at startup by reading an unset slot from a fresh weak array. *)
let ephe_none = Obj.field (Obj.repr (Weak.create 1)) ephe_first_key

let iter w f =
  let raw = Obj.repr w in
  for i = 0 to Weak.length w - 1 do
    let v = Obj.field raw (ephe_first_key + i) in
    if v != ephe_none then f (Obj.obj v)
  done

let fold_left f init w =
  let raw = Obj.repr w in
  let acc = ref init in
  for i = 0 to Weak.length w - 1 do
    let v = Obj.field raw (ephe_first_key + i) in
    if v != ephe_none then acc := f !acc (Obj.obj v)
  done;
  !acc
