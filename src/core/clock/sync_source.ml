type blocking_wait = {
  until :
    interrupted:(unit -> bool) -> float -> [ `Due | `Ended | `Interrupted ];
  interrupt : unit -> unit;
}

type wait = [ `Timer of float -> float | `Blocking of blocking_wait ]
type time_source = { label : string; now : unit -> float; wait : wait }

type timed = {
  time_source : time_source option;
  latency : float option;
  max_latency : float option;
}

type pacing = [ `Self_paced | `Timed of timed ]
type t = { identity : int; name : string; pacing : pacing }

let identities = Atomic.make 0

let make ~name pacing =
  { identity = Atomic.fetch_and_add identities 1; name; pacing }

let generic ~name =
  make ~name (`Timed { time_source = None; latency = None; max_latency = None })

let equal a b = a.identity = b.identity

let same a b =
  match (a, b) with
    | None, None -> true
    | Some a, Some b -> equal a b
    | _ -> false

let blocks sync_source =
  match sync_source.pacing with
    | `Self_paced -> true
    | `Timed { time_source = Some { wait = `Blocking _ } } -> true
    | `Timed _ -> false

let string_of_pacing = function
  | `Self_paced -> "self-paced"
  | `Timed _ -> "timed"

let builtin =
  {
    label = "ocaml";
    now = Duppy.time;
    wait = `Timer (fun target -> target -. Duppy.time ());
  }

let of_liq_time (module Time : Liq_time.T) =
  let now () = Time.to_float (Time.time ()) in
  {
    label = Time.implementation;
    now;
    wait = `Timer (fun target -> target -. now ());
  }

let time_sources : (string, time_source) Hashtbl.t = Hashtbl.create 2
let () = Hashtbl.replace time_sources builtin.label builtin

let find_time_source name =
  match Hashtbl.find_opt time_sources name with
    | Some time_source -> Some time_source
    | None ->
        Option.map of_liq_time (Hashtbl.find_opt Liq_time.implementations name)
