(** Should we restart Liquidsoap once it has been shut down? *)
let restart = ref false

let dummy = (None : Dtools.Init.t option)

let get x =
  match !x with
  | Some t -> t
  | None -> assert false

let duppy_atom = ref dummy
(** Duppy shutdown. *)
let duppy () = get duppy_atom
(* Alias because "duppy" apparently messes with camlp4 preprocessing... *)
let duppy_scheduler = duppy

let final_atom = ref dummy
(** It's the final shutdown tiluliluli. *)
let final () = get final_atom

let at_stop ?name ?depends f =
  ignore (Dtools.Init.at_stop ?name ?depends ~after:[final ()] f)
