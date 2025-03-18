include Liquidsoap_lang.Lang_string

type base_id = { name : string; mutable counter : int }

module IdMap = Weak.Make (struct
  type t = base_id

  let equal id id' = id.name = id'.name
  let hash { name } = Hashtbl.hash name
end)

(** Generate an identifier from the name of the source. *)
let generate_id =
  let m = Mutex.create () in
  let h = IdMap.create 10 in
  Mutex_utils.mutexify m (fun name ->
      let base_id = IdMap.merge h { name; counter = 0 } in
      let id =
        Bytes.(
          unsafe_to_string
            (of_string
               (match base_id.counter with
                 | 0 -> name
                 | n -> name ^ "." ^ string_of_int n)))
      in
      base_id.counter <- base_id.counter + 1;
      Gc.finalise_last (fun () -> ignore (Sys.opaque_identity base_id)) id;
      id)
