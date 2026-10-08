open Settings
open Status
open State

let sorted entries =
  List.sort (fun a b -> String.compare a.Status.id b.Status.id) entries

let run_status c st =
  {
    Status.animator =
      (match Atomic.get st.animator with
        | Some (animator, "lease") ->
            Some (animator, Printf.sprintf "lease: %.01fs" (lease_left st))
        | animator -> animator);
    sync =
      Option.map
        (fun (tracked : Sync_source.t) ->
          {
            Status.sync_source = tracked.name;
            pacing = Sync_source.string_of_pacing tracked.pacing;
            followed =
              Sync_source.same (Some tracked) (Atomic.get st.pace).followed;
          })
        (Atomic.get st.tracked);
    ticks = Atomic.get st.ticks;
    stream_time = stream_time st;
    lateness = lateness c st;
    outputs =
      sorted (List.map (fun o -> entry o.source) (Atomic.get st.outputs));
    active = sorted (List.map (fun (s, _) -> entry s) (active_members st));
    passive = sorted (List.map entry (Queues.WeakQueue.elements st.passive));
    statistics =
      { Status.life = Atomic.get st.life; recent = recent_figures st };
  }

let rec clock_status c =
  {
    Status.name = clock_name c;
    state =
      (match lifecycle c with
        | `Stopped reason -> `Stopped reason
        | `Started st -> `Started (run_status c st)
        | `Stopping (st, _) -> `Stopping (run_status c st));
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
    pending = sorted (List.map entry (Atomic.get c.pending));
    sub_clocks =
      List.map (fun { sub } -> clock_status (get sub)) (Atomic.get c.subs);
  }
