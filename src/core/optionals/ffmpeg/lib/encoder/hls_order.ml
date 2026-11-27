type entry = { time : int option; write : unit -> unit }

type 'stream queue = {
  stream : 'stream;
  held : entry Queue.t;
  mutable reached : int option;
}

type 'stream t = {
  mutable queues : 'stream queue list;
  mutable latest : int option;
}

let create () = { queues = []; latest = None }
let later time = Option.fold ~none:time ~some:(max time)

let queue t stream =
  match List.find_opt (fun q -> q.stream = stream) t.queues with
    | Some q -> q
    | None ->
        let q = { stream; held = Queue.create (); reached = None } in
        t.queues <- t.queues @ [q];
        q

let push ~stream ~time ~write t =
  let s = queue t stream in
  Queue.push { time; write } s.held;
  Option.iter
    (fun time ->
      s.reached <- Some (later time s.reached);
      t.latest <- Some (later time t.latest))
    time

(* The time up to which every gating stream has produced packets. *)
let frontier t gating =
  List.fold_left
    (fun frontier stream ->
      match (frontier, (queue t stream).reached) with
        | Some f, Some r -> Some (min f r)
        | _ -> None)
    (Some max_int) gating

(* The stream whose oldest packet is earliest; a gating stream first at equal
   times, so that a packet at a key frame's time follows the split. *)
let next t gating =
  let order s e =
    (Option.value ~default:min_int e.time, not (List.mem s.stream gating))
  in
  let best =
    List.fold_left
      (fun best s ->
        match (Queue.peek_opt s.held, best) with
          | None, _ -> best
          | Some e, Some (_, _, o) when order s e >= o -> best
          | Some e, _ -> Some (s, e, order s e))
      None t.queues
  in
  Option.map (fun (s, e, _) -> (s, e)) best

let stalled t time =
  match t.latest with
    | Some latest ->
        latest - time > Frame.main_of_seconds Settings.max_keyframe_wait#get
    | None -> false

let rec release ?(all = false) ~gating t =
  match next t gating with
    | None -> ()
    | Some (s, e) ->
        let ready =
          all
          ||
            match (e.time, frontier t gating) with
            | None, _ -> true
            | Some time, Some frontier -> time <= frontier || stalled t time
            | Some time, None -> stalled t time
        in
        if ready then (
          ignore (Queue.pop s.held);
          e.write ();
          release ~all ~gating t)
