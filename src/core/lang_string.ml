include Liquidsoap_lang.Lang_string

type base_id = { name : string; mutable counter : int }
type id = { id : string; base_id : base_id } [@@warning "-69"]

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
      let n = base_id.counter in
      base_id.counter <- base_id.counter + 1;
      {
        id =
          (if n = 0 then base_id.name else base_id.name ^ "." ^ string_of_int n);
        base_id;
      })

let string_of_id { id } = id
