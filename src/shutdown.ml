(** Should we restart Liquidsoap once it has been shut down? *)
let restart = ref false

let dummy = (None : Dtools.Init.t option)

let duppy_atom = ref dummy
(** Duppy shutdown task. *)
let duppy_scheduler () = Utils.get_some !duppy_atom

let final_atom = ref dummy
(** Final shutown task. This task might restart the application so other tasks
    should be executed before this one. *)
let final () = Utils.get_some !final_atom

(** Add a task to be executed at stop. Using this instead of
    [Dtools.Init.at_stop] ensures that it will be executed before the final
    task. *)
let at_stop ?name ?depends f =
  ignore (Dtools.Init.at_stop ?name ?depends ~after:[final ()] f)
