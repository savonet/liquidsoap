open Settings
open Status
open Event
open State

let attach t source =
  let c = get t in
  let rec add () =
    let pending = Atomic.get c.pending in
    if
      (not (List.memq source pending))
      && not (Atomic.compare_and_set c.pending pending (pending @ [source]))
    then add ()
  in
  add ()

let rec forget_pending c source =
  let pending = Atomic.get c.pending in
  if
    List.memq source pending
    && not
         (Atomic.compare_and_set c.pending pending
            (List.filter (fun s -> s != source) pending))
  then forget_pending c source

let queue_removal st source =
  Mutex.protect st.m (fun () ->
      Option.iter
        (fun member -> Atomic.set member.removed true)
        (Sources.find_opt st.animated source);
      push st.removals source)

let detach t source =
  let c = get t in
  forget_pending c source;
  Option.iter (fun st -> queue_removal st source) (Atomic.get c.streaming)

(* The lists change before the source sleeps: sleeping detaches its children,
   which queues further removals. *)
let remove_source c st source =
  let sleeps, unsubscribe =
    Mutex.protect st.m (fun () ->
        let mine, others =
          List.partition (fun o -> o.source == source) (Atomic.get st.outputs)
        in
        Atomic.set st.outputs others;
        let member = Sources.find_opt st.animated source in
        Sources.remove st.animated source;
        let others queue =
          Queues.WeakQueue.filter_out queue (fun other -> other == source)
        in
        others st.animated_sources;
        others st.passive;
        if member <> None then Atomic.set st.dirty true;
        ( List.map (fun o -> o.sleep) mine,
          Option.map (fun member -> member.unsubscribe) member ))
  in
  List.iter (quietly c "putting a source to sleep") sleeps;
  Option.iter (quietly c "unsubscribing from a source") unsubscribe

let rec apply_removals c st =
  match Atomic.exchange st.removals [] with
    | [] -> ()
    | sources ->
        List.iter (remove_source c st) (List.rev sources);
        apply_removals c st

let source_error c st (source : source) fn =
  stop_check ();
  try fn () with
    | Stop_signal -> raise Stop_signal
    | error ->
        let backtrace = Printexc.get_raw_backtrace () in
        let handlers = Atomic.get c.error_handlers in
        emit c
          (Source_failure
             { source = source#id; error; backtrace; handled = handlers <> [] });
        forget_pending c source;
        queue_removal st source;
        if handlers = [] then begin
          st.failing <- Some source#id;
          Printexc.raise_with_backtrace error backtrace
        end;
        List.iter (fun handler -> handler error backtrace) handlers

let track st (source : source) role =
  let member =
    {
      role;
      removed = Atomic.make false;
      sync = None;
      change = 0;
      unsubscribe = ignore;
    }
  in
  member.unsubscribe <-
    source#on_sync_source (fun sync -> push st.changes (source, sync));
  Mutex.protect st.m (fun () ->
      Sources.replace st.animated source member;
      Queues.WeakQueue.push st.animated_sources source;
      if List.memq source (Atomic.get st.removals) then
        Atomic.set member.removed true);
  push st.changes (source, source#sync_source);
  member

let activate_source st (source : source) =
  match source#source_type with
    | `Passive ->
        Mutex.protect st.m (fun () ->
            if not (Queues.WeakQueue.exists st.passive (( == ) source)) then
              Queues.WeakQueue.push st.passive source)
    | `Active -> ignore (track st source `Active)
    | `Output ->
        let sleep = source#wake_up () in
        let member =
          try track st source `Output
          with error ->
            let backtrace = Printexc.get_raw_backtrace () in
            (try sleep () with _ -> ());
            Printexc.raise_with_backtrace error backtrace
        in
        Mutex.protect st.m (fun () ->
            Atomic.set st.outputs
              (Atomic.get st.outputs @ [{ sleep; source; member }]))

let rec pop_pending c =
  match Atomic.get c.pending with
    | [] -> None
    | source :: rest as pending ->
        if Atomic.compare_and_set c.pending pending rest then Some source
        else pop_pending c

let crossed_multiple ~before ~after threshold =
  threshold > 0 && before / threshold <> after / threshold

let activate c st =
  let rec batch count =
    match pop_pending c with
      | None -> count
      | Some source ->
          source_error c st source (fun () -> activate_source st source);
          batch (count + 1)
  in
  let count = batch 0 in
  if count > 0 then begin
    let before = Atomic.fetch_and_add c.activated count in
    let after = before + count in
    if crossed_multiple ~before ~after conf_leak_warning#get then
      emit c
        (Leak_warning { activated = after; status = Inspect.clock_status c })
  end
