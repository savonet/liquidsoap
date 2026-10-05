open Settings
open Status
open Event
open State
open Activation

let switch c st tracked =
  let pace = Atomic.get st.pace in
  let time_source =
    match tracked with
      | Some { Sync_source.pacing = `Timed { time_source = Some time_source } }
        ->
          time_source
      | _ -> st.default_time_source
  in
  Atomic.set st.pace
    {
      time_source;
      offset = time_source.now () -. stream_time st;
      followed = tracked;
    };
  update_figures st (fun figures ->
      { figures with switches = figures.switches + 1 });
  let sync_name = Option.map (fun (s : Sync_source.t) -> s.name) in
  emit c
    (Sync_source_switch
       {
         from = sync_name pace.followed;
         into = sync_name tracked;
         pacing =
           Option.map
             (fun (s : Sync_source.t) -> Sync_source.string_of_pacing s.pacing)
             tracked;
         latency = latency st;
         max_latency = max_latency st;
       })

let reporting st =
  List.filter_map
    (fun (source, member) ->
      match member.sync with
        | Some sync when not (Atomic.get member.removed) ->
            Some (source, member, sync)
        | _ -> None)
    (members st)

let distinct_sync_sources reporting =
  List.fold_left
    (fun distinct (_, _, sync) ->
      if List.exists (Sync_source.equal sync) distinct then distinct
      else sync :: distinct)
    [] reporting

let sync_error c reporting =
  sync_error ~clock:(clock_name c)
    (List.map (fun (source, _, sync) -> (source, sync)) reporting)

let rec single_sync_source c st =
  let reporting = reporting st in
  match distinct_sync_sources reporting with
    | [] -> None
    | [sync] -> Some sync
    | _ ->
        let last (_, a, _) (_, b, _) = Int.compare b.change a.change in
        let source, member, _ = List.hd (List.sort last reporting) in
        let error = sync_error c reporting in
        member.sync <- None;
        source_error c st source (fun () -> raise error);
        single_sync_source c st

let announce_lease c st =
  match Atomic.get st.animator with
    | Some (`Thread, _)
      when lease_left st > 0. && conf_task#get && c.sync_mode <> `Unsynced ->
        emit c (Thread_lease { lease = lease_left st })
    | _ -> ()

(* The thread a clock moves to is leased when the blocking source came back
   within a lease of leaving. *)
let detect_flap c st ~now =
  match st.last_unblocked with
    | Some left when (not st.leased) && now -. left <= conf_thread_lease#get ->
        st.leased <- true;
        emit c (Flap { gap = now -. left; lease = conf_thread_lease#get })
    | _ -> ()

let update_blocking c st =
  let blocking =
    (match Atomic.get st.tracked with
      | Some sync -> Sync_source.blocks sync
      | None -> false)
    || Atomic.get st.sub_blocking > 0
  in
  if Atomic.exchange st.blocking blocking <> blocking then begin
    let now = Duppy.time () in
    if blocking then begin
      detect_flap c st ~now;
      st.unblocked_at <- None
    end
    else begin
      st.unblocked_at <- Some now;
      st.last_unblocked <- Some now;
      announce_lease c st
    end;
    tell_parent c blocking
  end

let wanted_animator c st =
  if Atomic.get st.blocking then
    ( `Thread,
      match Atomic.get st.tracked with
        | Some sync when Sync_source.blocks sync -> "blocks: " ^ sync.name
        | _ -> "blocks: sub-clock" )
  else if c.sync_mode = `Unsynced then (`Thread, "unsynced")
  else if not conf_task#get then (`Thread, "tasks off")
  else if lease_left st > 0. then (`Thread, "lease")
  else (`Task, "rests")

type _ Effect.t +=
  | Context : (clock * streaming) option Effect.t
  | Hop : animator -> unit Effect.t

let context () = try Effect.perform Context with Effect.Unhandled _ -> None

let change_animator c st =
  match Atomic.get st.animator with
    | Some (current, _) ->
        let wanted, why = wanted_animator c st in
        if wanted <> current then begin
          Atomic.set st.animator (Some (wanted, why));
          update_figures st (fun figures ->
              { figures with animator_changes = figures.animator_changes + 1 });
          emit c (Animator_change { from = current; into = wanted; why });
          Effect.perform (Hop wanted);
          st.worker_since <- Duppy.time ()
        end
        else Atomic.set st.animator (Some (current, why))
    | None -> ()

let apply_change st (source, sync) =
  match Sources.find_opt st.animated source with
    | Some member ->
        st.changes_applied <- st.changes_applied + 1;
        member.sync <- sync;
        member.change <- st.changes_applied
    | None -> ()

let pacing_point c st =
  let changes = List.rev (Atomic.exchange st.changes []) in
  let lease_over = st.unblocked_at <> None && lease_left st = 0. in
  if lease_over then begin
    st.unblocked_at <- None;
    st.leased <- false
  end;
  if Atomic.exchange st.dirty false || changes <> [] || lease_over then begin
    Mutex.protect st.m (fun () -> List.iter (apply_change st) changes);
    let tracked = single_sync_source c st in
    Atomic.set st.tracked tracked;
    update_blocking c st;
    change_animator c st;
    if
      c.sync_mode = `Automatic
      && not (Sync_source.same tracked (Atomic.get st.pace).followed)
    then switch c st tracked
  end

let park c st ?delay what =
  let started = Duppy.time () in
  Atomic.set st.activity
    (match what with `Rest -> `Resting | `Release -> `Released);
  (match (Atomic.get st.animator, what) with
    | Some (`Task, _), `Release -> Duppy.reschedule ~priority:`Clock scheduler
    | Some (`Task, _), _ -> Parker.park_task ?delay st.parker
    | _ -> Parker.park_thread ?delay st.parker);
  let real_time = Duppy.time () in
  let spent = real_time -. started in
  st.worker_since <- real_time;
  Atomic.set st.activity `Idle;
  if wants_debug c then emit c (Park { what; delay; spent });
  spent

let rest_on_timer c st delay_until =
  let rec rest () =
    let delay = delay_until () in
    if delay > 0. && running c then begin
      let spent = park c st ~delay `Rest in
      update_figures st (fun figures ->
          {
            figures with
            resting = figures.resting +. Float.min spent delay;
            no_worker = figures.no_worker +. Float.max 0. (spent -. delay);
          });
      rest ()
    end
  in
  rest ();
  update_figures st (fun figures -> { figures with rests = figures.rests + 1 })

let rest_on_wait c st (wait : Sync_source.blocking_wait) target =
  let started = Duppy.time () in
  Atomic.set st.interrupt_wait wait.interrupt;
  Atomic.set st.activity `Resting;
  let result =
    Fun.protect
      ~finally:(fun () ->
        Atomic.set st.interrupt_wait ignore;
        Atomic.set st.activity `Idle)
      (fun () -> wait.until ~interrupted:(fun () -> not (running c)) target)
  in
  update_figures st (fun figures ->
      {
        figures with
        resting = figures.resting +. (Duppy.time () -. started);
        rests = figures.rests + 1;
      });
  if result = `Ended then begin
    Option.iter
      (fun (sync : Sync_source.t) ->
        emit c (Sync_source_ended { sync_source = sync.name }))
      (Atomic.get st.pace).followed;
    ignore (request_stop c `Sync_source_ended)
  end

(* The deadline is the target on the time source, read again at every
   wake-up: delays are never added up. *)
let rest_until c st target =
  let { time_source; offset } = Atomic.get st.pace in
  match time_source.wait with
    | `Timer delay -> rest_on_timer c st (fun () -> delay (target +. offset))
    | `Blocking wait -> rest_on_wait c st wait (target +. offset)

let mark_warning st =
  let since_last =
    figures_since ~slowest:st.warning_slowest st.warning_mark
      (Atomic.get st.life)
  in
  st.warning_mark <- Atomic.get st.life;
  st.warning_slowest.duration <- 0.;
  st.warning_slowest.culprit <- None;
  st.last_warning <- Duppy.time ();
  since_last

let reset c st ~lateness =
  let ticks_before = Atomic.get st.ticks in
  let ticks_after = int_of_float (floor (now st /. st.frame_duration)) in
  Atomic.set st.ticks ticks_after;
  update_figures st (fun figures ->
      { figures with resets = figures.resets + 1 });
  emit c
    (Latency_reset
       { lateness; since_last = mark_warning st; ticks_before; ticks_after });
  let reset source =
    Option.iter (fun (a : active) -> a#reset) (active source)
  in
  List.iter (fun o -> reset o.source) (Atomic.get st.outputs);
  List.iter (fun ((source : source), _) -> reset source) (active_members st)

(* Returns whether the clock rested. *)
let rest_or_lateness c st =
  let now = now st in
  let target = stream_time st in
  if now < target then
    target -. now >= latency st
    && begin
      rest_until c st target;
      true
    end
  else begin
    let lateness = now -. target in
    if lateness >= max_latency st then reset c st ~lateness
    else if
      lateness >= conf_log_delay_threshold#get
      && Duppy.time () -. st.last_warning >= conf_log_delay#get
    then emit c (Latency_warning { lateness; since_last = mark_warning st });
    false
  end

let time_box c st =
  match Atomic.get st.animator with
    | Some (`Task, _) when Duppy.time () -. st.worker_since >= conf_time_box#get
      ->
        let spent = park c st `Release in
        update_figures st (fun figures ->
            {
              figures with
              released = figures.released +. spent;
              releases = figures.releases + 1;
            })
    | _ -> ()

let between_ticks c st =
  stop_check ();
  if not (measures c st && rest_or_lateness c st) then time_box c st
