open Settings
open Status
open Event
open State
open Life

type merge = { from : clock; into : clock }

let rec resolve merges c =
  match List.find_opt (fun merge -> merge.from == c) merges with
    | Some merge -> resolve merges merge.into
    | None -> c

let class_parent merges c =
  let members =
    c
    :: List.filter_map
         (fun merge ->
           if resolve merges merge.from == c then Some merge.from else None)
         merges
  in
  Option.map get
    (List.find_map (fun member -> Atomic.get member.parent) members)

let pending_mode c =
  if state c = `Stopping then `Stopping
  else (c.sync_mode :> [ sync_mode | `Stopping ])

let direction ~pos ca cb =
  let merges_into c other =
    state c = `Stopped
    && (c.sync_mode = `Automatic
       || (c.sync_mode :> [ sync_mode | `Stopping ]) = pending_mode other)
  in
  if merges_into ca cb then { from = ca; into = cb }
  else if merges_into cb ca then { from = cb; into = ca }
  else raise (Conflict { pos; left = clock_name ca; right = clock_name cb })

let check_owners ~pos ca cb =
  if ca.sync_mode = `Passive && cb.sync_mode = `Passive && ca.owner <> cb.owner
  then
    raise
      (Controller_conflict
         {
           pos;
           left = clock_name ca;
           left_controller = string_of_controller ca;
           right = clock_name cb;
           right_controller = string_of_controller cb;
         })

let rec plan ~pos merges a b =
  let a = resolve merges a and b = resolve merges b in
  if a == b then merges
  else begin
    let merge = direction ~pos a b in
    check_owners ~pos a b;
    let parents = (class_parent merges a, class_parent merges b) in
    let merges = merges @ [merge] in
    match parents with Some a, Some b -> plan ~pos merges a b | _ -> merges
  end

exception Nested

let check_nesting merges =
  let rec climb visited cell =
    match class_parent merges cell with
      | None -> ()
      | Some parent ->
          let parent = resolve merges parent in
          if List.memq parent visited then raise Nested;
          climb (parent :: visited) parent
  in
  List.iter
    (fun merge ->
      let survivor = resolve merges merge.into in
      climb [survivor] survivor)
    merges

let merge_ids x y =
  match (Atomic.get x.id, Atomic.get y.id) with
    | Some id, None ->
        Registry.move_id ~from:x ~into:y id;
        Atomic.set y.id (Some id);
        Atomic.set y.log None
    | Some dropped, Some kept ->
        Registry.drop_id x;
        emit y (Id_kept { kept; dropped })
    | None, _ -> ()

let merge_subs c =
  let merged =
    List.fold_left
      (fun merged entry ->
        match
          List.partition (fun other -> get other.sub == get entry.sub) merged
        with
          | [same], others ->
              others
              @ [
                  {
                    same with
                    registrants = same.registrants + entry.registrants;
                  };
                ]
          | _ -> merged @ [entry])
      [] (Atomic.get c.subs)
  in
  Atomic.set c.subs merged

let commit { from = x; into = y } =
  let pending = Atomic.get y.pending in
  Atomic.set y.pending
    (pending
    @ List.filter (fun s -> not (List.memq s pending)) (Atomic.get x.pending));
  let moved = Atomic.get x.subs in
  Atomic.set y.subs (Atomic.get y.subs @ moved);
  Atomic.set y.error_handlers
    (Atomic.get y.error_handlers @ Atomic.get x.error_handlers);
  merge_ids x y;
  if Atomic.get y.stack = [] then Atomic.set y.stack (Atomic.get x.stack);
  if Atomic.get y.parent = None && Atomic.get x.parent <> None then begin
    Atomic.set y.parent (Atomic.get x.parent);
    Registry.remove y
  end;
  Unifier.(handle x <-- handle y);
  Registry.remove x;
  (y, moved)

(* Entries that came to designate one clock are merged on the survivors and
   on their parents, the only lists a plan can touch. *)
let settle_subs merges =
  List.iter
    (fun merge ->
      let survivor = merge.into in
      List.iter merge_subs
        (survivor
        :: Option.to_list (Option.map get (Atomic.get survivor.parent))))
    merges

let start_moved_subs (survivor, moved) =
  match (state survivor, Atomic.get survivor.streaming) with
    | `Started, Some st ->
        List.iter
          (fun { sub } ->
            let sub = get sub in
            if state sub = `Stopped then start_sub ~force:st.forced st sub)
          moved
    | _ -> ()

let unify ~pos a b =
  Transition.run (fun () ->
      let merges = plan ~pos [] (get a) (get b) in
      (try check_nesting merges
       with Nested ->
         raise
           (Loop { pos; left = clock_name (get a); right = clock_name (get b) }));
      let moved = List.map commit merges in
      settle_subs merges;
      List.iter start_moved_subs moved)
