open Settings
open Status
open Event
open State
open Activation
open Pacing

let failure_policy : (t -> failure -> unit) Atomic.t =
  Atomic.make (fun _ _ -> Tutils.shutdown 1)

let set_failure_policy policy = Atomic.set failure_policy policy

let cannot_start ~force c =
  if state c <> `Stopped then Some `Not_stopped
  else if Atomic.get global_stop then Some `Global_stop
  else if not (force || Atomic.get application_started) then
    Some `Application_not_started
  else if
    not
      (force || c.sync_mode = `Passive
      || List.exists
           (fun (s : source) -> role s = `Output)
           (Atomic.get c.pending))
  then Some `No_output
  else None

let default_time_source c =
  let wanted = conf_preferred#get in
  match Sync_source.find_time_source wanted with
    | Some time_source -> time_source
    | None ->
        emit c
          (Unknown_time_source { wanted; used = Sync_source.builtin.label });
        Sync_source.builtin

let slowest () = { duration = 0.; culprit = None }

let new_streaming ~force c =
  let default_time_source = default_time_source c in
  let real_time = Duppy.time () in
  {
    frame_duration = Lazy.Mutexed.force Frame.duration;
    forced = force;
    default_time_source;
    ticks = Atomic.make 0;
    m = Mutex.create ();
    outputs = Atomic.make [];
    animated = Sources.create 8;
    animated_sources = Queues.WeakQueue.create ();
    passive = Queues.WeakQueue.create ();
    removals = Atomic.make [];
    changes = Atomic.make [];
    dirty = Atomic.make false;
    on_tick = Atomic.make [];
    after_tick = Atomic.make [];
    pulled = Atomic.make false;
    animator = Atomic.make None;
    pace =
      Atomic.make
        {
          time_source = default_time_source;
          offset = default_time_source.now ();
          followed = None;
        };
    tracked = Atomic.make None;
    blocking = Atomic.make false;
    sub_blocking = Atomic.make 0;
    parker = Parker.create ();
    interrupt_wait = Atomic.make ignore;
    activity = Atomic.make `Idle;
    life = Atomic.make no_figures;
    recent = Atomic.make None;
    changes_applied = 0;
    failing = None;
    unblocked_at = None;
    last_unblocked = None;
    leased = false;
    worker_since = real_time;
    tick_waited = 0.;
    tick_slowest = slowest ();
    last_warning = neg_infinity;
    last_long_tick = neg_infinity;
    warning_mark = no_figures;
    warning_slowest = slowest ();
    window_mark = no_figures;
    window_started = real_time;
    window_slowest = slowest ();
  }

let take_callbacks callbacks =
  List.iter
    (fun fn ->
      stop_check ();
      fn ())
    (List.rev (Atomic.exchange callbacks []))

let note_slowest slowest ~duration ~name =
  if duration > slowest.duration then begin
    slowest.duration <- duration;
    slowest.culprit <- name
  end

let animate c st (source : source) member =
  if not (Atomic.get member.removed) then begin
    let started = Duppy.time () in
    let waited = st.tick_waited in
    source_error c st source (fun () ->
        Option.iter (fun (a : active) -> a#output) (active source));
    note_slowest st.tick_slowest
      ~duration:(Duppy.time () -. started -. (st.tick_waited -. waited))
      ~name:(Some source#id)
  end

let roll_window st real_time =
  if real_time -. st.window_started >= window_length then begin
    let life = Atomic.get st.life in
    Atomic.set st.recent
      (Some (figures_since ~slowest:st.window_slowest st.window_mark life));
    st.window_mark <- life;
    st.window_started <- real_time;
    st.window_slowest.duration <- 0.;
    st.window_slowest.culprit <- None
  end

let update_statistics c st ~started =
  let real_time = Duppy.time () in
  let duration = real_time -. started -. st.tick_waited in
  let name = st.tick_slowest.culprit in
  note_slowest st.warning_slowest ~duration ~name;
  note_slowest st.window_slowest ~duration ~name;
  update_figures st (fun figures ->
      let longest = duration > figures.longest_tick in
      {
        figures with
        ticks = figures.ticks + 1;
        producing = figures.producing +. duration;
        longest_tick = (if longest then duration else figures.longest_tick);
        slowest_source = (if longest then name else figures.slowest_source);
      });
  roll_window st real_time;
  let waits_in_tick =
    match (Atomic.get st.pace).followed with
      | Some { Sync_source.pacing = `Self_paced } -> true
      | _ -> false
  in
  if
    duration > conf_time_box#get
    && (Atomic.get st.life).ticks > 1
    && (not waits_in_tick)
    && real_time -. st.last_long_tick >= conf_log_delay#get
  then begin
    st.last_long_tick <- real_time;
    emit c (Long_tick { duration; slowest_source = name })
  end

type sub_snapshot = {
  snapshot_of : clock;
  streaming_then : streaming option;
  ticks_then : int;
}

let sub_snapshot c =
  List.map
    (fun { sub } ->
      let snapshot_of = get sub in
      let streaming_then = Atomic.get snapshot_of.streaming in
      {
        snapshot_of;
        streaming_then;
        ticks_then =
          (match streaming_then with
            | Some st -> Atomic.get st.ticks
            | None -> 0);
      })
    (Atomic.get c.subs)

let ticked_since { snapshot_of; streaming_then; ticks_then } =
  match (Atomic.get snapshot_of.streaming, streaming_then) with
    | Some current, Some before when current == before ->
        Atomic.get current.ticks > ticks_then
    | Some current, _ -> Atomic.get current.ticks > 0
    | None, _ -> false

let report_failure c failure =
  emit c (Failure failure);
  quietly c "reporting a failure" (fun () ->
      (Atomic.get failure_policy) (handle c) failure)

let rec wind_down c =
  Transition.run (fun () ->
      match (state c, Atomic.get c.streaming) with
        | `Stopping, Some st -> (
            let subs = Atomic.get c.subs in
            List.iter
              (fun o -> quietly c "putting an output to sleep" o.sleep)
              (Atomic.get st.outputs);
            List.iter
              (fun (_, member) ->
                quietly c "unsubscribing from a source" member.unsubscribe)
              (members st);
            Mutex.protect st.m (fun () ->
                Atomic.set st.outputs [];
                Sources.reset st.animated;
                ignore (Queues.WeakQueue.flush_elements st.animated_sources);
                ignore (Queues.WeakQueue.flush_elements st.passive));
            List.iter
              (fun queue -> Atomic.set queue [])
              [st.on_tick; st.after_tick];
            List.iter
              (fun { sub } ->
                quietly c "stopping a sub-clock" (fun () ->
                    stop_clock (get sub) `Parent_stopped))
              subs;
            if Atomic.get st.blocking then tell_parent c false;
            let reason = Atomic.get c.stop_reason in
            Atomic.set c.streaming None;
            Atomic.set c.state `Stopped;
            if is_top_level c then Registry.wait c;
            emit c
              (Stop
                 {
                   reason;
                   ticks = Atomic.get st.ticks;
                   stream_time = stream_time st;
                 });
            match reason with
              | `Failed failure -> report_failure c failure
              | _ -> ())
        | _ -> ())

and stop_clock c reason =
  Transition.run (fun () ->
      if request_stop c reason then begin
        Option.iter interrupt (Atomic.get c.streaming);
        if c.sync_mode = `Passive && not (Atomic.get c.ticking) then wind_down c
      end)

let stop t = stop_clock (get t) `Requested

let fail c st error backtrace =
  ignore (request_stop c (`Failed { error; backtrace; source = st.failing }))

let rec run_tick c st ~pull =
  let started = Duppy.time () in
  Atomic.set st.activity (`Ticking started);
  st.tick_waited <- 0.;
  st.tick_slowest.duration <- 0.;
  st.tick_slowest.culprit <- None;
  let subs = sub_snapshot c in
  apply_removals c st;
  Atomic.set st.pulled pull;
  activate c st;
  pacing_point c st;
  List.iter (fun o -> animate c st o.source o.member) (Atomic.get st.outputs);
  List.iter
    (fun (source, member) -> animate c st source member)
    (active_members st);
  take_callbacks st.on_tick;
  Atomic.set st.pulled false;
  stop_check ();
  List.iter tick_sub subs;
  Atomic.incr st.ticks;
  stop_check ();
  take_callbacks st.after_tick;
  apply_removals c st;
  update_statistics c st ~started;
  Atomic.set st.activity `Idle

and tick_sub snapshot =
  if state snapshot.snapshot_of = `Started && not (ticked_since snapshot) then (
    try tick_passive snapshot.snapshot_of ~pull:false with
      | Stop_signal -> raise Stop_signal
      | _ -> ())

(* [ticking] is raised before the state is read, and [stop_clock] reads it
   after changing the state: one of the two winds the clock down. *)
and tick_passive c ~pull =
  if Atomic.exchange c.ticking true then
    invalid_arg "Clock.tick: a tick of this clock is in progress";
  let finish () =
    Atomic.set c.ticking false;
    if state c = `Stopping then wind_down c
  in
  match (state c, Atomic.get c.streaming) with
    | `Started, Some st -> (
        match run_tick c st ~pull with
          | () -> finish ()
          | exception Stop_signal ->
              Atomic.set st.pulled false;
              finish ();
              raise Stop_signal
          | exception error ->
              let backtrace = Printexc.get_raw_backtrace () in
              Atomic.set st.pulled false;
              fail c st error backtrace;
              finish ();
              Printexc.raise_with_backtrace error backtrace)
    | _ ->
        finish ();
        not_running c

let passive t =
  let c = get t in
  if c.sync_mode <> `Passive then
    invalid_arg "Clock: only a passive clock is ticked by its controller";
  c

let tick ?(pull = false) t = tick_passive (passive t) ~pull

let activate_pending t =
  let c = passive t in
  if Atomic.exchange c.ticking true then
    invalid_arg "Clock.activate_pending: a tick of this clock is in progress";
  Fun.protect
    ~finally:(fun () ->
      Atomic.set c.ticking false;
      if state c = `Stopping then wind_down c)
    (fun () ->
      let st = started_streaming c in
      try activate c st with
        | Stop_signal -> raise Stop_signal
        | error ->
            let backtrace = Printexc.get_raw_backtrace () in
            fail c st error backtrace;
            Printexc.raise_with_backtrace error backtrace)

let has_work c st =
  Atomic.get c.pending <> []
  || Atomic.get st.outputs <> []
  || active_members st <> []

let loop c st () =
  st.worker_since <- Duppy.time ();
  (try
     while running c && has_work c st do
       run_tick c st ~pull:false;
       between_ticks c st
     done;
     if Atomic.get global_stop then ignore (request_stop c `Global_stop)
     else if not (has_work c st) then ignore (request_stop c `No_sources)
   with
    | Stop_signal -> ignore (request_stop c `Global_stop)
    | error -> fail c st error (Printexc.get_raw_backtrace ()));
  wind_down c

let rec spawn animator fn =
  match animator with
    | `Thread ->
        (* Script code registers its callbacks through an effect that only the
           scheduler's own tasks handle. *)
        Duppy.thread ~priority:`Clock scheduler (fun () ->
            Script_callback.uncollected fn)
    | `Task ->
        Duppy.Task.add scheduler
          {
            Duppy.Task.priority = `Clock;
            events = [`Delay 0.];
            handler =
              (fun _ ->
                Duppy.run fn;
                []);
          }

(* A deep handler is part of the continuation, so the loop keeps it when it
   resumes under another animator. *)
and animated c st fn () =
  Effect.Deep.match_with fn ()
    {
      retc = Fun.id;
      exnc = raise;
      effc =
        (fun (type a) (performed : a Effect.t) ->
          match performed with
            | Context ->
                Some
                  (fun (k : (a, unit) Effect.Deep.continuation) ->
                    Effect.Deep.continue k (Some (c, st)))
            | Hop animator ->
                Some
                  (fun (k : (a, unit) Effect.Deep.continuation) ->
                    spawn animator (fun () -> Effect.Deep.continue k ()))
            | _ -> None);
    }

let sources_of_pending c =
  List.map (fun (s : source) -> (s#id, role s)) (Atomic.get c.pending)

let rec start_clock ~force c =
  Transition.run (fun () ->
      (match cannot_start ~force c with
        | Some reason -> raise (Cannot_start { clock = clock_name c; reason })
        | None -> ());
      let name = clock_name c in
      let st = new_streaming ~force c in
      set_clock_id c name;
      Atomic.set c.streaming (Some st);
      Atomic.set c.state `Started;
      if is_top_level c then Registry.run c;
      let animator =
        if c.sync_mode = `Passive then None else Some (wanted_animator c st)
      in
      Atomic.set st.animator animator;
      emit c
        (Start
           {
             top_level = is_top_level c;
             controller =
               (if c.sync_mode = `Passive then Some (string_of_controller c)
                else None);
             sync_mode = c.sync_mode;
             sources = sources_of_pending c;
             animator;
           });
      List.iter
        (fun { sub } -> start_sub ~force st (get sub))
        (Atomic.get c.subs);
      Option.iter
        (fun (animator, _) -> spawn animator (animated c st (loop c st)))
        animator)

and start_sub ~force parent sub =
  if cannot_start ~force sub = None then start_clock ~force sub;
  match Atomic.get sub.streaming with
    | Some st when Atomic.get st.blocking ->
        Atomic.incr parent.sub_blocking;
        Atomic.set parent.dirty true
    | _ -> ()

let start ?(force = false) t = start_clock ~force (get t)

let start_pass () =
  let waiting = Registry.waiting_clocks () in
  List.iter
    (fun c ->
      if c.sync_mode <> `Passive then
        Transition.run (fun () ->
            if cannot_start ~force:false c = None then
              start_clock ~force:false c))
    waiting;
  List.length waiting

let start_scope fn =
  let result = fn () in
  ignore (start_pass ());
  result

let application_start () =
  Atomic.set application_started true;
  ignore (start_pass ())

let find_sub c sub =
  List.find_opt (fun entry -> get entry.sub == get sub) (Atomic.get c.subs)

let set_registrants c sub registrants =
  let others =
    List.filter (fun entry -> get entry.sub != get sub) (Atomic.get c.subs)
  in
  Atomic.set c.subs
    (if registrants > 0 then others @ [{ sub; registrants }] else others)

let registrants c sub =
  match find_sub c sub with Some { registrants } -> registrants | None -> 0

let register ~parent sub =
  Transition.run (fun () ->
      let p = get parent in
      let s = get sub in
      (match Atomic.get s.parent with
        | Some own when s.sync_mode = `Passive && get own == p -> ()
        | _ ->
            raise
              (Not_a_sub_clock { clock = clock_name s; parent = clock_name p }));
      set_registrants p sub (registrants p sub + 1);
      match (state p, Atomic.get p.streaming) with
        | `Started, Some st when state s = `Stopped ->
            start_sub ~force:st.forced st s
        | _ -> ())

let deregister ~parent sub =
  Transition.run (fun () ->
      let p = get parent in
      match registrants p sub with
        | 0 -> ()
        | 1 ->
            set_registrants p sub 0;
            stop_clock (get sub) `Parent_stopped
        | count -> set_registrants p sub (count - 1))

let sub_clocks t = List.map (fun { sub } -> sub) (Atomic.get (get t).subs)
