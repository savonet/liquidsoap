type sync_mode = [ `Automatic | `Cpu | `Unsynced | `Passive ]
type source_type = [ `Passive | `Active | `Output ]
type owner = { kind : string; id : string }
type animator = [ `Task | `Thread ]

type failure = {
  error : exn;
  backtrace : Printexc.raw_backtrace;
  source : string option;
}

type stop_reason =
  [ `Never_started
  | `Requested
  | `No_sources
  | `Global_stop
  | `Sync_source_ended
  | `Parent_stopped
  | `Failed of failure ]

type figures = {
  ticks : int;
  producing : float;
  (* Seconds spent resting ahead of the time source. *)
  resting : float;
  rests : int;
  (* Seconds spent away from a worker after a time box. *)
  released : float;
  releases : int;
  (* Seconds a rest lasted beyond its delay, waiting for a worker. *)
  no_worker : float;
  resets : int;
  switches : int;
  animator_changes : int;
  longest_tick : float;
  (* Slowest source of the longest tick. *)
  slowest_source : string option;
}

(* A source in a status: its id, its kind and the ids of what keeps it awake. *)
type entry = {
  id : string;
  source_type : source_type;
  activations : string list;
}

type controller = { parent : string option; owner : owner option }
type sync = { sync_source : string; pacing : string; followed : bool }
type statistics = { life : figures; recent : figures }

(* What a clock tells only while it has a run; [animator] is [None] for a
   passive clock. *)
type run = {
  animator : (animator * string) option;
  sync : sync option;
  ticks : int;
  stream_time : float;
  lateness : float option;
  outputs : entry list;
  active : entry list;
  passive : entry list;
  statistics : statistics;
}

(* A snapshot of a clock and of its sub-clocks. *)
type t = {
  name : string;
  state : [ `Stopped of stop_reason | `Started of run | `Stopping of run ];
  sync_mode : sync_mode;
  controller : controller option;
  pending : entry list;
  sub_clocks : t list;
}

(* What a clock's loop is doing; [`Ticking] carries the tick's start time. *)
type activity = [ `Idle | `Ticking of float | `Resting | `Released ]

let no_figures =
  {
    ticks = 0;
    producing = 0.;
    resting = 0.;
    rests = 0;
    released = 0.;
    releases = 0;
    no_worker = 0.;
    resets = 0;
    switches = 0;
    animator_changes = 0;
    longest_tick = 0.;
    slowest_source = None;
  }

type slowest = { mutable duration : float; mutable culprit : string option }

(* The maxima of a span cannot be had by subtraction: they come from [slowest].
*)
let figures_since ~slowest (mark : figures) (figures : figures) =
  {
    ticks = figures.ticks - mark.ticks;
    producing = figures.producing -. mark.producing;
    resting = figures.resting -. mark.resting;
    rests = figures.rests - mark.rests;
    released = figures.released -. mark.released;
    releases = figures.releases - mark.releases;
    no_worker = figures.no_worker -. mark.no_worker;
    resets = figures.resets - mark.resets;
    switches = figures.switches - mark.switches;
    animator_changes = figures.animator_changes - mark.animator_changes;
    longest_tick = slowest.duration;
    slowest_source = slowest.culprit;
  }

let sync_mode_of_string = function
  | "auto" -> `Automatic
  | "cpu" -> `Cpu
  | "none" -> `Unsynced
  | "passive" -> `Passive
  | mode -> invalid_arg ("Invalid sync mode: " ^ mode)

let none = "-"
let or_none = Option.value ~default:none

let string_of_sync_mode = function
  | `Automatic -> "auto"
  | `Cpu -> "cpu"
  | `Unsynced -> "none"
  | `Passive -> "passive"

let string_of_source_type = function
  | `Passive -> "passive"
  | `Active -> "active"
  | `Output -> "output"

let string_of_animator = function `Task -> "task" | `Thread -> "thread"
let string_of_owner { kind; id } = Printf.sprintf "%s %s" kind id

let string_of_stop_reason : stop_reason -> string = function
  | `Never_started -> "never started"
  | `Requested -> "requested"
  | `No_sources -> "no sources"
  | `Global_stop -> "global stop"
  | `Sync_source_ended -> "sync source ended"
  | `Parent_stopped -> "parent stopped"
  | `Failed { error } -> Printf.sprintf "failed (%s)" (Printexc.to_string error)

let string_of_controller { parent; owner } =
  match (owner, parent) with
    | Some owner, _ -> string_of_owner owner
    | None, Some parent -> parent
    | None, None -> none

let run status =
  match status.state with
    | `Started run | `Stopping run -> Some run
    | `Stopped _ -> None

let header status =
  let state =
    match status.state with
      | `Stopped reason -> "stopped: " ^ string_of_stop_reason reason
      | `Started _ -> "started"
      | `Stopping _ -> "stopping"
  in
  let driver =
    match
      (status.controller, Option.bind (run status) (fun run -> run.animator))
    with
      | Some controller, _ ->
          [Printf.sprintf "controlled by %s" (string_of_controller controller)]
      | None, Some (animator, why) ->
          [Printf.sprintf "%s: %s" (string_of_animator animator) why]
      | None, None -> []
  in
  Printf.sprintf "%s [%s]" status.name
    (String.concat ", "
       ([state; string_of_sync_mode status.sync_mode] @ driver))

let string_of_entries = function
  | [] -> none
  | entries ->
      String.concat ", "
        (List.map
           (fun { id; activations } ->
             Printf.sprintf "%s [%s]" id (String.concat ", " activations))
           entries)

let source_lines status =
  let line label entries =
    Printf.sprintf "%s: %s" label (string_of_entries entries)
  in
  (if status.pending = [] then [] else [line "pending" status.pending])
  @
    match run status with
    | None -> []
    | Some run ->
        [
          line "outputs" run.outputs;
          line "active" run.active;
          line "passive" run.passive;
        ]

let streaming_lines status =
  match run status with
    | Some { sync; ticks; stream_time; lateness; statistics = { life } } ->
        (match sync with
          | Some { sync_source; pacing; followed } ->
              [
                Printf.sprintf "sync source: %s (%s%s)" sync_source pacing
                  (if followed then ", followed" else "");
              ]
          | None -> [])
        @ [
            Printf.sprintf "ticks: %d  time: %.02fs  lateness: %s" ticks
              stream_time
              (match lateness with
                | Some lateness -> Printf.sprintf "%.03fs" lateness
                | None -> none);
            Printf.sprintf "tick: mean %.01fms max %.01fms (slowest: %s)"
              (if life.ticks = 0 then 0.
               else 1000. *. life.producing /. float life.ticks)
              (1000. *. life.longest_tick)
              (or_none life.slowest_source);
            Printf.sprintf
              "held: producing %.01fs  resting %.01fs  released %.01fs  no \
               worker %.01fs"
              life.producing life.resting life.released life.no_worker;
          ]
    | None -> []

let rec block indent status =
  let body = indent ^ "  " in
  let nested = body ^ "   " in
  header status
  :: List.map (( ^ ) body) (streaming_lines status @ source_lines status)
  @ List.concat_map
      (fun sub ->
        match block nested sub with
          | first :: rest -> (body ^ "└─ " ^ first) :: rest
          | [] -> [])
      status.sub_clocks

let report statuses =
  String.concat "\n\n"
    (List.map (fun status -> String.concat "\n" (block "" status)) statuses)

let all_entries status =
  (match run status with
    | Some run -> run.outputs @ run.active @ run.passive
    | None -> [])
  @ status.pending

(* A source's own id among its activations is how its clock holds it, not an
   edge of the graph. *)
let activators entries (entry : entry) =
  let known id = List.exists (fun (other : entry) -> other.id = id) entries in
  List.partition known (List.filter (( <> ) entry.id) entry.activations)

(* A clock's sources as trees: from its outputs, from sources woken by other
   clocks, and from sources nobody woke. *)
let graph_lines entries =
  let printed = Hashtbl.create 16 in
  let children (parent : entry) =
    List.filter
      (fun (entry : entry) ->
        entry.id <> parent.id && List.mem parent.id entry.activations)
      entries
  in
  let rec tree indent first (entry : entry) =
    let label =
      Printf.sprintf "%s%s [%s]" first entry.id
        (string_of_source_type entry.source_type)
    in
    if Hashtbl.mem printed entry.id then [label ^ " (see above)"]
    else begin
      Hashtbl.replace printed entry.id ();
      label
      :: List.concat_map
           (tree (indent ^ "   ") (indent ^ "└─ "))
           (children entry)
    end
  in
  let top = tree "  " "  " in
  let inside entry = fst (activators entries entry) in
  let outside entry = snd (activators entries entry) in
  let roots =
    List.filter
      (fun (entry : entry) ->
        entry.source_type = `Output && inside entry = [] && outside entry = [])
      entries
  in
  let from_outside =
    List.filter (fun entry -> inside entry = [] && outside entry <> []) entries
  in
  let external_activators =
    List.sort_uniq String.compare (List.concat_map outside from_outside)
  in
  let unwoken =
    List.filter
      (fun (entry : entry) ->
        entry.source_type <> `Output && inside entry = [] && outside entry = [])
      entries
  in
  let section title lines = if lines = [] then [] else title @ lines in
  List.concat_map top roots
  @ section
      ["  Woken from outside this clock:"]
      (List.concat_map
         (fun activator ->
           Printf.sprintf "  %s [external]" activator
           :: List.concat_map (tree "     " "  └─ ")
                (List.filter
                   (fun entry -> List.mem activator (outside entry))
                   from_outside))
         external_activators)
  @ section ["  Not woken:"] (List.concat_map top unwoken)

let rec graphs ?parent status =
  let title =
    match parent with
      | Some parent -> Printf.sprintf "Clock %s (in %s):" status.name parent
      | None -> Printf.sprintf "Clock %s:" status.name
  in
  String.concat "\n" (title :: graph_lines (all_entries status))
  :: List.concat_map (graphs ~parent:status.name) status.sub_clocks

let source_graph statuses =
  String.concat "\n\n" (List.concat_map (fun status -> graphs status) statuses)
