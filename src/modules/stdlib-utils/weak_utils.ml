external iter : 'a Weak.t -> ('a -> unit) -> unit = "caml_weak_utils_iter"

external fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a Weak.t -> 'acc
  = "caml_weak_utils_fold_left"
