(* Weak array internal layout (ephemeron with n keys, unused data field):
   field 0   : GC ephemeron list link
   field 1   : ephemeron data (unused in Weak.t)
   field 2+i : weak pointer i *)
let ephe_first_key = 2

(* Use Weak.check for liveness (no allocation), then Obj.field to read the raw
   value without boxing it into Some.  No allocation happens between check and
   read, so the GC cannot clear the slot in that window. *)
let iter w f =
  let raw = Obj.repr w in
  for i = 0 to Weak.length w - 1 do
    if Weak.check w i then f (Obj.obj (Obj.field raw (ephe_first_key + i)))
  done

let fold_left f init w =
  let raw = Obj.repr w in
  let acc = ref init in
  for i = 0 to Weak.length w - 1 do
    if Weak.check w i then
      acc := f !acc (Obj.obj (Obj.field raw (ephe_first_key + i)))
  done;
  !acc
