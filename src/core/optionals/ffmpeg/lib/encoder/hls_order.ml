type entry = { field : string; time : int option; write : unit -> unit }

type t = {
  (* Arrival order. *)
  mutable held : entry list;
  reached : (string, int) Hashtbl.t;
  mutable latest : int option;
}

let create () = { held = []; reached = Hashtbl.create 4; latest = None }

let push t ~field ~time ~write =
  (* ponytail: list append, the queue holds one encoder latency of packets. *)
  t.held <- t.held @ [{ field; time; write }];
  Option.iter
    (fun time ->
      let reached =
        Option.fold ~none:time ~some:(max time)
          (Hashtbl.find_opt t.reached field)
      in
      Hashtbl.replace t.reached field reached;
      t.latest <- Some (Option.fold ~none:time ~some:(max time) t.latest))
    time

(* The time up to which every gating stream has produced packets. *)
let frontier t gating =
  List.fold_left
    (fun frontier field ->
      match (frontier, Hashtbl.find_opt t.reached field) with
        | Some f, Some r -> Some (min f r)
        | _ -> None)
    (Some max_int) gating

(* The oldest packet of each stream, earliest first; a gating stream first at
   equal times, so that a packet at a key frame's time follows the split. *)
let next t gating =
  let heads =
    List.fold_left
      (fun heads e ->
        if List.exists (fun h -> h.field = e.field) heads then heads
        else e :: heads)
      [] t.held
  in
  let order e =
    (Option.value ~default:min_int e.time, not (List.mem e.field gating))
  in
  match List.sort (fun a b -> compare (order a) (order b)) heads with
    | e :: _ -> Some e
    | [] -> None

let rec release ?(all = false) t ~gating =
  match next t gating with
    | None -> ()
    | Some e ->
        let stalled time =
          match t.latest with
            | Some latest ->
                latest - time
                > Frame.main_of_seconds Settings.max_keyframe_wait#get
            | None -> false
        in
        let ready =
          all
          ||
            match (e.time, frontier t gating) with
            | None, _ -> true
            | Some time, Some frontier -> time <= frontier || stalled time
            | Some time, None -> stalled time
        in
        if ready then (
          t.held <- List.filter (fun h -> h != e) t.held;
          e.write ();
          release ~all t ~gating)
