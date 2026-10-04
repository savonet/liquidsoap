open Settings
open Status
open State

let sorted entries =
  List.sort (fun a b -> String.compare a.Status.id b.Status.id) entries

let rec clock_status c =
  let st = Atomic.get c.streaming in
  let streaming fn = Option.map fn st in
  let sources fn = Option.value ~default:[] (streaming fn) in
  {
    Status.name = clock_name c;
    state =
      (match state c with
        | `Stopped -> `Stopped (Atomic.get c.stop_reason)
        | (`Started | `Stopping) as state -> state);
    sync_mode = c.sync_mode;
    controller =
      (if c.sync_mode = `Passive then
         Some
           {
             Status.parent =
               Option.map
                 (fun parent -> clock_name (get parent))
                 (Atomic.get c.parent);
             owner = c.owner;
           }
       else None);
    animator = Option.bind st (fun st -> Atomic.get st.animator);
    sync =
      Option.bind st (fun st ->
          Option.map
            (fun (tracked : Sync_source.t) ->
              {
                Status.sync_source = tracked.name;
                pacing = Sync_source.string_of_pacing tracked.pacing;
                followed =
                  Sync_source.same (Some tracked) (Atomic.get st.pace).followed;
              })
            (Atomic.get st.tracked));
    ticks = streaming (fun st -> Atomic.get st.ticks);
    stream_time = streaming stream_time;
    lateness = Option.bind st (lateness c);
    pending = sorted (List.map entry (Atomic.get c.pending));
    outputs =
      sources (fun st ->
          sorted (List.map (fun o -> entry o.source) (Atomic.get st.outputs)));
    active =
      sources (fun st ->
          sorted (List.map (fun (s, _) -> entry s) (active_members st)));
    passive = sources (fun st -> sorted (List.map entry (passive_sources st)));
    sub_clocks =
      List.map (fun { sub } -> clock_status (get sub)) (Atomic.get c.subs);
    statistics =
      streaming (fun st ->
          { Status.life = Atomic.get st.life; recent = recent_figures st });
  }
